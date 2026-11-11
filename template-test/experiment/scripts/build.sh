#!/bin/bash
set -euo pipefail
mode=$1 root=$2 config=$3 scope=$4 trace=$5
shift 5

case $mode in
  scratch)
    if [ -d "$root/_build" ]; then
      old=$(mktemp -d "$root/.old-build.XXXXXX")
      mv "$root/_build" "$old/"
      rm -rf "$old" || true
    fi ;;
  incremental) ;;
  *) echo "no mode $mode" >&2; exit 2 ;;
esac

case $config,$scope in
  port,*) param=_,g=0 ;;
  templated,) param=_,g=0,functors-as-templates=1 ;;
  templated,*)
    param=_,g=0,marked-templates=1
    trap 'rm -f "$root/$scope/dune"' EXIT
    printf '%s\n%s\n' \
      ";; Only for the templated build: this package's functors become templates." \
      "(env (_ (flags (:standard -functors-as-templates))))" > "$root/$scope/dune" ;;
  *) echo "no configuration $config" >&2; exit 2 ;;
esac

trace_file=()
[ "$trace" = - ] || trace_file=(--trace-file "$trace")
if (cd "$root" && OCAMLPARAM=$param DUNE_CACHE=disabled \
    opam exec --switch=functors-as-templates -- \
    dune build --root . --profile release "$@" -j 1 ${trace_file[@]+"${trace_file[@]}"}) \
    < /dev/null > /dev/null 2>&1
then echo true; else echo false; fi
