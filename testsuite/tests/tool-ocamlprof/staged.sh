# Instrument a unit, compile and run it, and print the unit annotated
# with the counts that the run recorded.
set -e
ocamlrun=$1; srcdir=$2; source=$3; output=$4
$ocamlrun $srcdir/tools/ocamlprof -instrument -m a $source > instrumented.ml
$ocamlrun $srcdir/ocamlc -nostdlib -I $srcdir/stdlib -I $srcdir/tools \
  $srcdir/tools/profiling.cmo instrumented.ml -o instrumented.byte
$ocamlrun ./instrumented.byte > $output
$ocamlrun $srcdir/tools/ocamlprof -f ocamlprof.dump $source >> $output
