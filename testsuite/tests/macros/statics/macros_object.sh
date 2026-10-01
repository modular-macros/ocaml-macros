#!/bin/sh
# Which of the two units has a compile-time object
for unit in macros_object_plain pack_member; do
  if test -f "$unit\$macros.cmo"; then
    echo "$1: $unit has one" >> macros_object.result
  else
    echo "$1: $unit has none" >> macros_object.result
  fi
  rm -f "$unit\$macros.cmo"
done
