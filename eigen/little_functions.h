#ifndef _LITTLE_FUNCTIONS_H_
#define _LITTLE_FUNCTIONS_H_

#include "eigen_base_types.h"

SCALAR
abs_val(SCALAR x)
{
  return (x < 0.0) ? -x : x;
}

SCALAR
max_abs(SCALAR x, SCALAR y)
{
  SCALAR ax = abs_val(x);
  SCALAR ay = abs_val(y);
  return (ax >= ay) ? ax : ay;
}

int
min_int(int i, int j)
{
  return (i<j) ? i : j; }

bool
close(SCALAR a, SCALAR b)
{
  return abs_val(a-b) < 1.0e-50;
}

#endif
