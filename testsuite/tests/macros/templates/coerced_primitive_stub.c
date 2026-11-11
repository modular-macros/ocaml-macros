#include <caml/mlvalues.h>

value coerced_primitive_succ(value n)
{
  return Val_long(Long_val(n) + 1);
}
