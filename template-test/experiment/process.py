import re
import sys
from dataclasses import dataclass
from pathlib import Path

import pandas as pd

EXPERIMENT = Path(__file__).resolve().parent
LOGS = EXPERIMENT / "logs"
BUILD = ["package", "config", "rep", "target"]


# Load

def load(run: str, name: str) -> pd.DataFrame:
    return pd.read_csv(LOGS / f"{run}-{name}.csv", dtype=str, keep_default_na=False)


def write(run: str, name: str, table: pd.DataFrame) -> None:
    table.to_csv(LOGS / f"{run}-{name}.csv", index=False)


# Tidy

@dataclass(frozen=True)
class Observations:
    packages: pd.DataFrame
    modules: pd.DataFrame
    builds: pd.DataFrame
    calls: pd.DataFrame
    objects: pd.DataFrame
    units: pd.DataFrame
    sites: pd.DataFrame


def tidy(run: str) -> Observations:
    def experiments(name: str) -> pd.DataFrame:
        return pd.concat([load(run, f"{e}-{name}").assign(experiment=e)
                          for e in ("clean", "rebuild")], ignore_index=True)

    packages = load(run, "packages").set_index("package")
    modules = load(run, "modules")
    modules["test"] = modules["test"] == "true"
    builds = experiments("builds").astype({"rep": int})
    builds["ok"] = builds["ok"] == "true"
    builds = builds[["experiment", *BUILD, "ok"]]
    calls = experiments("calls").astype({"rep": int})
    calls = calls.assign(
        program=calls["program"].str.rsplit("/", n=1).str[-1],
        outputs=calls["outputs"].str.replace("_build/default/", "", regex=False),
        cpu_s=(calls["user_cpu_time"].astype(int) + calls["system_cpu_time"].astype(int)) / 1e9,
    )[["experiment", *BUILD, "program", "outputs", "cpu_s"]]
    objects = load(run, "clean-cmo").astype({"rep": int, "bytecode": int})
    objects = objects[["package", "config", "rep", "path", "module", "bytecode"]]
    units = load(run, "clean-units")[["package", "config", "module"]]
    sites = load(run, "clean-sites").astype({"line": int})
    sites = sites[["package", "config", "role", "module", "name", "kind", "file", "line"]]

    for _, b in builds[~builds["ok"]].iterrows():
        print("warning: the build failed:", *b[["experiment", *BUILD]])
    write(run, "tidy-builds", builds)
    write(run, "tidy-calls", calls)
    write(run, "tidy-objects", objects)
    write(run, "tidy-sites", sites)
    return Observations(packages, modules, builds, calls, objects, units, sites)


# Transform

# A path without its extension, and without the $macros of a compile-time object.
def stem(path: pd.Series) -> pd.Series:
    return path.str.rsplit(".", n=1).str[0].str.removesuffix("$macros")


# A process belongs to the package when it writes an object of one of its modules.
def select_package_calls(observations: Observations) -> pd.DataFrame:
    modules = observations.modules
    stems = pd.concat([modules[["package", "byte"]].rename(columns={"byte": "stem"}),
                       modules[["package", "native"]].rename(columns={"native": "stem"})])
    calls = observations.calls.reset_index(names="call")
    written = calls[["call", "package", "outputs"]].assign(output=calls["outputs"].str.split()) \
        .explode("output").dropna(subset=["output"])
    belongs = written.assign(stem=stem(written["output"])) \
        .merge(stems.drop_duplicates(), on=["package", "stem"])["call"].unique()
    calls = calls[calls["call"].isin(belongs)].drop(columns="call")
    return calls.assign(
        ocamlc=calls["program"].str.startswith("ocamlc"),
        writes_cmo=calls["outputs"].str.split().apply(
            lambda outputs: any(o.endswith(".cmo") and "$" not in o for o in outputs)))


def tally_compilations(observations: Observations, package_calls: pd.DataFrame,
                       experiment: str) -> pd.DataFrame:
    calls = package_calls[package_calls["experiment"] == experiment]
    tally = calls.assign(ocamlc_cpu_s=calls["cpu_s"].where(calls["ocamlc"], 0.0),
                         cmo_units=(calls["ocamlc"] & calls["writes_cmo"]).astype(int)) \
        .groupby(BUILD, as_index=False)[["ocamlc_cpu_s", "cmo_units"]].sum()
    builds = observations.builds[observations.builds["experiment"] == experiment][BUILD]
    return builds.merge(tally, how="left").fillna({"ocamlc_cpu_s": 0.0, "cmo_units": 0}) \
        .astype({"cmo_units": int})


def check_bytecode(observations: Observations, package_calls: pd.DataFrame) -> None:
    calls = package_calls[(package_calls["experiment"] == "clean") & package_calls["ocamlc"]]
    written = calls.assign(output=calls["outputs"].str.split()).explode("output")
    written = written[written["output"].str.endswith(".cmo")]
    written = written.assign(byte=stem(written["output"]))[[*BUILD, "byte"]]
    builds = observations.builds[observations.builds["experiment"] == "clean"][BUILD]
    expected = builds.merge(observations.modules[["package", "byte"]], on="package")
    missing = expected.merge(written.drop_duplicates(), how="left", indicator=True)
    missing = missing[missing["_merge"] == "left_only"]
    for build, n in missing.groupby(BUILD).size().items():
        print("warning: ocamlc did not compile", n, "modules of", *build)


def select_test_objects(observations: Observations) -> pd.DataFrame:
    tests = observations.modules[observations.modules["test"]][["package", "byte"]]
    objects = observations.objects
    return objects.assign(byte=stem(objects["path"])).merge(tests, on=["package", "byte"]) \
        .drop(columns="byte")


def sum_test_bytecode(observations: Observations, test_objects: pd.DataFrame) -> pd.DataFrame:
    total = test_objects.groupby(["package", "config", "rep"], as_index=False)["bytecode"].sum()
    builds = observations.builds[observations.builds["experiment"] == "clean"]
    return builds[["package", "config", "rep"]].merge(total, how="left") \
        .fillna({"bytecode": 0}).astype({"bytecode": int}) \
        .rename(columns={"bytecode": "test_bytecode"})


def mark_sites_of_package(observations: Observations) -> pd.DataFrame:
    units = observations.units.rename(columns={"module": "callee_module"}).drop_duplicates()
    sites = observations.sites.assign(
        callee_module=observations.sites["name"].str.split(".").str[0])
    sites = sites.merge(units.assign(of_package=True), how="left",
                        on=["package", "config", "callee_module"])
    return sites.assign(of_package=sites["of_package"].notna())


def count_functors(sites: pd.DataFrame) -> pd.DataFrame:
    def count(rows: pd.DataFrame, suffix: str) -> pd.DataFrame:
        return rows.groupby(["package", "config", "kind"]).size().unstack(fill_value=0) \
            .rename(columns=lambda kind: kind + suffix)

    definitions = sites[sites["role"] == "definition"]
    applications = sites[sites["role"] == "application"]
    counts = pd.concat([count(definitions, "s"), count(applications, "_apps"),
                        count(applications[applications["of_package"]], "_apps_of_package")],
                       axis=1).fillna(0).astype(int)
    columns = ["functors", "templates", "functor_apps", "template_apps",
               "functor_apps_of_package", "template_apps_of_package"]
    return counts.reindex(columns=columns, fill_value=0).reset_index()


# Summarise

def summarise(clean: pd.DataFrame, bytecode: pd.DataFrame,
              rebuild: pd.DataFrame) -> pd.DataFrame:
    measures = [
        ("compile_cpu_s", clean, "ocamlc_cpu_s", "median"),
        ("test_bytecode", bytecode, "test_bytecode", "median"),
        ("rebuild_cpu_s", rebuild, "ocamlc_cpu_s", "mean"),
        ("rebuild_units", rebuild, "cmo_units", "mean"),
    ]
    tables = []
    for quantity, table, column, statistic in measures:
        by = table.groupby(["package", "config"])[column].agg(statistic).unstack("config")
        tables.append(by.assign(quantity=quantity, factor=by["templated"] / by["port"]))
    return pd.concat(tables).reset_index()[["package", "quantity", "port", "templated", "factor"]]


def process(run: str) -> None:
    observations = tidy(run)
    package_calls = select_package_calls(observations)
    test_objects = select_test_objects(observations)
    sites = mark_sites_of_package(observations)
    write(run, "package-calls", package_calls)
    write(run, "test-objects", test_objects)
    write(run, "sites", sites)
    check_bytecode(observations, package_calls)

    clean = tally_compilations(observations, package_calls, "clean")
    bytecode = sum_test_bytecode(observations, test_objects)
    rebuild = tally_compilations(observations, package_calls, "rebuild")
    write(run, "compile-time", clean)
    write(run, "test-bytecode", bytecode)
    write(run, "rebuild-cost", rebuild)
    write(run, "functor-counts", count_functors(sites))
    write(run, "summary", summarise(clean, bytecode, rebuild))


# Targets: the rebuilds' edits, chosen from the clean builds' sites

def find_struct_line(file: Path, line: int) -> int | None:
    lines = file.read_text().splitlines()
    for i in range(line - 1, min(line + 30, len(lines))):  # long parameter signatures
        if re.search(r"\bstruct\s*$", lines[i]):
            return i + 1
    return None


def print_targets(run: str, package: str, k: int) -> None:
    root = EXPERIMENT / load(run, "packages").set_index("package").at[package, "root"]
    sites = load(run, "clean-sites")
    sites = sites[(sites["package"] == package) & (sites["config"] == "templated")]
    applications = sites[sites["role"] == "application"]["name"].value_counts()
    found = []
    for _, d in sites[(sites["role"] == "definition") & (sites["kind"] == "template")].iterrows():
        source = d["file"].replace(".pp.ml", ".ml")  # ppx output names its source
        if applications.get(d["name"], 0) and (root / source).exists():
            line = find_struct_line(root / source, int(d["line"]))
            if line is not None:
                found.append((applications[d["name"]], d["name"], source, line))
    for _, name, source, line in sorted(found, key=lambda f: -f[0])[:k]:
        print(name, source, line, sep="\t")


def main() -> None:
    match sys.argv[1:]:
        case ["targets", run, package, k]:
            print_targets(run, package, int(k))
        case [run]:
            process(run)
        case _:
            sys.exit("usage: process.py (RUN | targets RUN PACKAGE K)")


if __name__ == "__main__":
    main()
