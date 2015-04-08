#include <iostream> 
#include <cmath>
#include <map>
#include <vector>
#include <stdint.h>

inline
bool IsNan(float f)
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


void
check_for_nan (std::vector<float> vec)   // works fine for const& as well
{
  std::cout << "\n  Check using   std::isnan: ";
  for (size_t i=0; i<vec.size(); ++i)
    if (std::isnan(vec[i]))
      std::cout << "     v[" << i << "] = NaN ";
}

void
check_for_nan2 (std::vector<float> vec)   // works fine for const& as well
{
  std::cout << "\n  Check using   IsNan: ";
  for (size_t i=0; i<vec.size(); ++i)
    if (IsNan(vec[i]))
      std::cout << "     v[" << i << "] = NaN";
}

int main()
{
  std::vector<float> x(10);
  for (size_t i=0; i<x.size(); ++i)
    x[i] = (float)i;

  x[2] = std::nanf("Missing");
  x[4] = std::nanf("");
  x[7] = NAN;
  x[8] = std::nanf("2");
  x.push_back(std::nanf("Missing"));

  std::cout << "TEST: vector of values is ";
  for (float xi : x)
    std::cout << " " << xi;
  std::cout << std::endl;

  check_for_nan(x);
  check_for_nan2(x);

  std::map<std::string, std::vector<float>> m;

  m["zero"] = std::vector<float>(11,0);
  m["x"]    = x;

  std::cout << "Checking zero...\n";
  check_for_nan(m["zero"]);
  std::cout << "Checking x...\n";
  check_for_nan(m["x"]);

}
