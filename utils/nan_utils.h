#ifndef _NAN_UTILS_H_
#define _NAN_UTILS_H_

#include <stdint.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"

inline
bool IsNanf(float f)
{
  const uint32_t u = *(uint32_t*)&f;
  return (u&0x7F800000) == 0x7F800000 && (u&0x7FFFFF);    // Both NaN and qNan.
}


  
inline
bool IsNan(double d)
{
  const uint64_t u = *(uint64_t*)&d;
  return (u&0x7FF0000000000000ULL) == 0x7FF0000000000000ULL && (u&0xFFFFFFFFFFFFFULL);
}

#pragma GCC diagnostic pop

#endif

