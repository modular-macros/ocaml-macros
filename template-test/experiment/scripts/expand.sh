#!/bin/bash
set -euo pipefail
experiment=$(cd "$(dirname "$0")/.." && pwd)
manifest=$1
shift

for package in "$@"; do
  IFS=$'\t' read -r _ root targets directories test_patterns scope \
    < <(awk -F'\t' -v p="$package" '$1 == p' "$experiment/packages.tsv")
  scope=${scope#-}
  [ -f "$manifest-packages.csv" ] || echo "package,root,targets,scope" > "$manifest-packages.csv"
  echo "$package,$root,$targets,$scope" >> "$manifest-packages.csv"

  # shellcheck disable=SC2086
  "$experiment/scripts/build.sh" scratch "$experiment/$root" port "$scope" - $targets > /dev/null

  [ -f "$manifest-modules.csv" ] || echo "package,library,module,byte,native,test" > "$manifest-modules.csv"
  set -f  # the directories and patterns are matched, not expanded
  # shellcheck disable=SC2086
  (cd "$experiment/$root/_build/default" && find $directories -name '*.cmx' ! -name '*$*') \
    | sort | while read -r cmx; do
    native=${cmx%.cmx}
    byte=$(echo "$native" | sed 's|/native/|/byte/|')
    library=$(dirname "$(dirname "$cmx")")
    test=false
    for p in $test_patterns; do case $byte.cmo in $p) test=true; break ;; esac; done
    case $library,$test in *.eobjs,false) continue ;; esac  # a build tool
    echo "$package,$library,$(basename "$native"),$byte,$native,$test"
  done >> "$manifest-modules.csv"
  set +f
done
