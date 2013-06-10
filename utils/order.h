#ifndef _ORDER_H_
#define _ORDER_H_


#include <vector>
#include <algorithm>

template <typename T>
std::vector<size_t>
sort_indices(std::vector<T> const& v, bool descending=false)
{
  std::vector<size_t> idx(v.size());
  std::clog << "SORT: size is " << v.size() << std::endl;
  for (size_t i = 0; i < idx.size(); ++i)
    idx[i] = i;
  std::clog << "SORT: size 2 is " << idx.size() << std::endl;
  if(descending)
    std::sort(idx.begin(), idx.end(),
	      [&v](size_t i1, size_t i2)->bool {return v[i1] >= v[i2];});
  else
    std::sort(idx.begin(), idx.end(),
	      [&v](size_t i1, size_t i2)->bool {return v[i1] <  v[i2];});
  std::clog << "SORT: exit size is " << idx.size() << std::endl;
  return idx;
}

#endif
