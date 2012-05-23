#ifndef _WEALTH_H_
#define _WEALTH_H_

#include "dynamic_array.h"

#include <iostream>
#include <functional>
#include <vector>

class ProbDist: public std::unary_function<int,double>
{
  public:
  virtual
    std::string identifier()    const = 0;
  virtual
    double      operator()(int) const = 0;
};



/**********************************************************************************

   Probability distributions that control spending

     All take 2 integer arguments (k since reject, j remaining)
     but some (like universal) don't use the second argument

 **********************************************************************************/

class GeometricDist: public ProbDist
{
  const double mPsi;
  const double mOneMinusPsi;

  public:

  GeometricDist (double psi): mPsi(psi), mOneMinusPsi(1-psi) { }   

  std::string identifier() const; 
  double operator()(int k) const;    // percent of wealth spent at step k; adds to 1
};
  

class UniversalDist: public ProbDist
{
  public:
  std::string identifier() const { return "univ"; }
  double operator()(int k) const;
  };

// double uniform_to_end (int k, int left);


/**********************************************************************************

   Wealth tracker basically maps from integers that follow the tests
   to the wealth of the expert or bidder.  The wealth is monotone increasing
   in the index/position k.

   Normalized to have value omega at index 0.

 **********************************************************************************/

class WealthArray
{
  const std::string     mName;
  const int             mPadding;    // space for wealth above omega
  const int             mSize;       // number of distinct wealth values
  const double          mOmega;      // defines wealth at zeroIndex and determines how far 'up' wealth can go 
  const int             mZeroIndex;  // position of W_0, the place used for omega
  DynamicArray<double>  mWealth;     // negative indices indicate wealth below omega
  std::vector< std::pair<int,double> > mPositions;  // hold locations for new positions

 public:
 WealthArray(std::string name, double omega, int zeroIndex, ProbDist const& pdf)
   : mName(name), mPadding(25), mSize(zeroIndex+mPadding), mOmega(omega), mZeroIndex(zeroIndex), mWealth(), mPositions() { initialize_array(pdf);}

  int    size ()                   const { return mSize; }
  int    zero_index ()             const { return mZeroIndex ; }
  double omega ()                  const { return mOmega; }
  
  double bid(int k)                const { return mWealth[k+1]-mWealth[k]; }    // recursion goes from small wealth to larger (note inlined in init array too)
  double wealth(int k)             const { return mWealth[k]; }
  double operator[](int k)         const { return mWealth[k]; }
  
  std::pair<int, double> wealth_position (int k) const { return mPositions[k]; }
  
  void print_to (std::ostream& os) const { os << "Wealth array " << mName << "  " << mWealth; }
  
 private:
  void initialize_array(ProbDist const& p);
  std::pair<int, double> find_wealth_position (int k, double increaseInWealth) const;

};

inline
std::ostream&
operator<< (std::ostream& os, WealthArray const& wa)
{
  wa.print_to(os);
  return os;
}



#endif
