#!/bin/sh
set -eu
here=$(cd "$(dirname "$0")" && pwd)
switch=${OPAMSWITCH:-functors-as-templates}
mkdir -p "$1"
dir=$(cd "$1" && pwd)

grep -v '^#' "$here/packages.txt" | while read -r dest spec; do
  [ -d "$dir/$dest" ] && { echo "$dest: already present" >&2; exit 1; }
  case $spec in
    git+*)
      url=${spec#git+}
      git clone -q "${url%#*}" "$dir/$dest"
      git -C "$dir/$dest" checkout -q "${url##*#}" ;;
    *)
      opam source --switch="$switch" "$spec" --dir="$dir/$dest" > /dev/null ;;
  esac
done

printf '(lang dune 3.24)\n' > "$dir/clients/core/dune-workspace"

find "$dir" -name dune-project | while read -r f; do
  python3 - "$f" <<'EOF'
import re, sys
p = sys.argv[1]; s = open(p).read()
s = re.sub(r"\A\(lang dune [0-9.]+\)\n(\(using macocaml 0\.1\)\n)?", "", s)
open(p, "w").write("(lang dune 3.24)\n(using macocaml 0.1)\n" + s)
EOF
done

for kind in port annotations; do
  cd "$here/patches/$kind"
  find . -name '*.patch' | sed 's|^\./||; s|\.patch$||' | while read -r dest; do
    patch -s -E -d "$dir/$dest" -p1 < "$dest.patch"
  done
done
