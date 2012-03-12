#ifndef _WEALTH_H_
#define _WEALTH_H_

#include "dynamic_array.h"

#include <iostream>
#include <functional>


typedef double (*ProbDist)(int);

double universal      (int k);
double geometric      (int k);
// double uniform_to_end (int k, int left);


/**********************************************************************************

   Wealth tracker basically maps from integers that follow the tests
   to the wealth of the expert or bidder.  The wealth is monotone increasing
   in the index/position k.

   Normalized to have value omega at index 0.

 **********************************************************************************/


class WealthArray
{
  typedef double(*Tfunc)(int);

  const std::string     mName;
  const int             mSize;       // number of distinct wealth values
  const double          mOmega;      // defines wealth at zeroIndex and determines how far 'up' wealth can go 
  const int             mZeroIndex;  // position of W_0, initially the place used for omega
  DynamicArray<double>  mWealth;     // negative indices indicate wealth below omega

 public:
 WealthArray(std::string name, int size, double omega, int zeroIndex, Tfunc pdf)
   : mName(name), mSize(size), mOmega(omega), mZeroIndex(zeroIndex), mWealth() { initialize_array(pdf);}

  int    size ()                   const { return mSize; }
  int    zero_index ()             const { return mZeroIndex ; }
  double omega ()                  const { return mOmega; }
  
  double bid(int k)                const { return mWealth[k]-mWealth[k-1]; }
  double wealth(int k)             const { return mWealth[k]; }
  double operator[](int k)         const { return mWealth[k]; }
  
  std::pair<int, double> new_wealth_position (int k, double increaseInWealth) const;
  
  void print_to (std::ostream& os) const { os << "Wealth array " << mName << "  " << mWealth; }
  
 private:
  void initialize_array(Tfunc p);
};

inline
std::ostream&
operator<< (std::ostream& os, WealthArray const& wa)
{
  wa.print_to(os);
  return os;
}



/**********************************************************************************

   Probability distributions that control spending

     All take 2 integer arguments (k since reject, j remaining)
     but some (like universal) don't use the second argument

 **********************************************************************************/

class GeometricDist: public std::unary_function<int,double>   // has flexible prob
{
  const double mP;
  const double mNorm;

  public:

  GeometricDist (double p): mP(p), mNorm((1-p)/p) { }

  double operator()(int k) const
  { double p=1;
    for (int i=0; i<=k; ++i) p *= mP;
    return p * mNorm;
  }

};
  


#endif
