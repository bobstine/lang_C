#include "little_functions.h"

bool
is_binary_vector(VECTOR const& v)
{
  for(int i=0; i<v.size(); ++i)
    if( (v[i] != 0) && (v[i] != 1) )
      return false;
  return true;
}
