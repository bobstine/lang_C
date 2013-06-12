#ifndef _ORDER_H_
#define _ORDER_H_


#include <vector>
#include <algorithm>

template <typename T>
std::vector<size_t>
order(std::vector<T> const& v, bool descending=false)
{
  std::vector<size_t> idx(v.size());
  for (size_t i = 0; i < idx.size(); ++i)      idx[i] = i;
  if(descending)
    std::sort(idx.begin(), idx.end(),
	      [&v](size_t i1, size_t i2)->bool {return v[i1] > v[i2];});  // careful! *strict* weak ordering
  else
    std::sort(idx.begin(), idx.end(),
	      [&v](size_t i1, size_t i2)->bool {return v[i1] < v[i2];});
  return idx;
}

#endif
