#!/bin/bash

set -euo pipefail
experiment=$(cd "$(dirname "$0")" && pwd)
measure=$experiment/measure
logs=$experiment/logs
reps=$1 k=$2
shift 2
run=$(date +%Y-%m-%dT%H-%M)
manifest=$logs/$run
mkdir -p "$logs" "$manifest-lists"
opam exec --switch=functors-as-templates -- dune build --root "$measure" --profile release
"$experiment/scripts/expand.sh" "$manifest" "$@"

# PACKAGE's row of the manifest: sets root, targets and scope.
load() {
  IFS=, read -r _ root targets scope \
    < <(awk -F, -v p="$1" 'NR > 1 && $1 == p' "$manifest-packages.csv")
  root=$experiment/$root
}

# The bytecode paths of PACKAGE's modules, below _build/default, each ending in SUFFIX.
modules() {
  awk -F, -v p="$1" -v s="$2" 'NR > 1 && $1 == p { print $4 s }' "$manifest-modules.csv"
}

build() {
  # shellcheck disable=SC2046,SC2086
  "$experiment/scripts/build.sh" "$1" "$root" "$3" "$scope" "$4" $targets $(modules "$2" .cmo)
}

# Record a finished build: its row in PREFIX-builds.csv, its trace's processes in
# PREFIX-calls.csv, the .cmo files listed in CMOS, and the .cmt files listed in CMTS (-
# for neither), each row after the values of COLUMN=VALUE ...
record() {
  local prefix=$1 ok=$2 build=$3 cmos=$4 cmts=$5 tools=$measure/_build/default
  shift 5
  [ -f "$prefix-builds.csv" ] || (IFS=,; echo "${*%%=*},ok") > "$prefix-builds.csv"
  (IFS=,; echo "${*#*=},$ok") >> "$prefix-builds.csv"
  "$tools/trace.exe" "$prefix-calls.csv" "$@" -- "$logs/trace"
  [ "$cmos" = - ] || (cd "$build" && xargs "$tools/cmo.exe" "$prefix-cmo.csv" "$@" --) < "$cmos"
  [ "$cmts" = - ] || (cd "$build" && xargs "$tools/cmt.exe" "$prefix" "$@" --) < "$cmts"
}

for package in "$@"; do
  modules "$package" .cmo > "$manifest-lists/$package.cmo.txt"
  modules "$package" .cmt > "$manifest-lists/$package.cmt.txt"
done

for rep in $(seq 0 $((reps - 1))); do
  for package in "$@"; do
    load "$package"
    for config in port templated; do
      ok=$(build scratch "$package" "$config" "$logs/trace")
      cmts=-
      [ "$rep" = 0 ] && cmts=$manifest-lists/$package.cmt.txt
      record "$manifest-clean" "$ok" "$root/_build/default" \
        "$manifest-lists/$package.cmo.txt" "$cmts" \
        package="$package" config="$config" rep="$rep" target=
      echo "clean $package $config $rep $ok"
    done
  done
done

edited=
restore() { if [ -n "$edited" ]; then mv "$edited.orig" "$edited"; edited=; fi; }
trap restore EXIT
for package in "$@"; do
  uv run --project "$experiment" python "$experiment/process.py" targets "$run" "$package" "$k" \
    > "$manifest-lists/$package.targets.txt"
done
for rep in $(seq 0 $((reps - 1))); do
  for package in "$@"; do
    load "$package"
    for config in port templated; do
      n=0
      while IFS=$'\t' read -r target source line; do
        n=$((n + 1))
        build scratch "$package" "$config" - > /dev/null
        edited=$root/$source; cp "$edited" "$edited.orig"
        awk -v l="$line" -v n="$n" 'NR == l { $0 = $0 " let _ = ref " n } { print }' \
          "$edited.orig" > "$edited"
        ok=$(build incremental "$package" "$config" "$logs/trace")
        restore
        record "$manifest-rebuild" "$ok" "$root/_build/default" - - \
          package="$package" config="$config" rep="$rep" target="$target" < /dev/null
        echo "rebuild $package $config $rep $target $ok"
      done < "$manifest-lists/$package.targets.txt"
    done
  done
done
rm -f "$logs/trace"
uv run --project "$experiment" python "$experiment/process.py" "$run"
