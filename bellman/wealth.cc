#include "wealth.h"
#include "line_search.Template.h"

#include <utility>
#include <math.h>
#include <sstream>

/////////////////////////////////////////  Distributions  ////////////////////////////////////////

double
GeometricDist::operator() (int k) const
{
  double p=1.0;
  for (int i=0; i<k; ++i) p *= mOneMinusPsi;
  return p * mPsi;
}


std::string
GeometricDist::identifier() const
{
  std::stringstream ss;
  ss << "g";
  if (mPsi < 0.1)
  { ss << "0";
    if (mPsi < 0.01)
    { ss << "0";
      if (mPsi < 0.001)
      { ss << "0";
        if (mPsi < 0.0001)
	{ ss << "0";
	  if (mPsi < 0.00001)
	    ss << "0";
	}
      }
    }
  }
  ss << floor(100000*mPsi);
  return ss.str();
}


double
UniversalDist::operator() (int k) const
{
  const int start = 20;                 // where to start the code
  const double normConst = 0.3346;      // so sums to 1 (computed in MMa)
  double ll = log(k+1+start);
  return 1.0/( (k+start) * ll * ll * normConst);
}


double uniform_to_end (int k, int left)         // equal spread over possible locations
{
  return 1.0/(double)(k + left);
}



//     WealthArray     WealthArray     WealthArray     WealthArray     WealthArray     WealthArray     WealthArray     WealthArray

std::pair<int, double>
WealthArray::find_wealth_position (int k0, double increase)  const // k0 is current position denoting current wealth
{
  double target = mWealth[k0] + increase;      // 'wealth' is 'new wealth' > 'current wealth'
  int k1 (mSize-1);                            // W[k0] <= W[k1]
  assert (target <= mWealth[k1]);              // needs to be in range of table
  while (k0+1 < k1)                            // bracket between k0 and k1
  { int kk = floor((k0+k1)/2);
    // std::cout << "DEBUG: find_wealth_position   W[" << k0 << "]="
    //          << mWealth[k0] << " W[" << kk << "]=" << mWealth[kk] << "  W[" << k1 << "]=" << mWealth[k1] << std::endl;
    if (target < mWealth[kk])
    { k1 = kk; }
    else
    { k0 = kk; }
  }
  if (k0<k1)  // inside range
  { double p ( (target - mWealth[k0]) / (mWealth[k1] - mWealth[k0]) );
    return std::make_pair(k0,1-p);
  }
  else
    return std::make_pair(k0,1);
}


void
WealthArray::initialize_array(ProbDist const& p)
{
  // std::cout << "WARRAY: Building dyn array with " << mSize << " steps and wealth " << mOmega << " @ " << mZeroIndex << std::endl;
  assert((0 < mZeroIndex) && (mZeroIndex < mSize-2));
  DynamicArray<double> da(0,mSize-1);
  da.assign(mZeroIndex,mOmega);
  for(int i=mZeroIndex-1; 0 <= i; --i)
    { // std::cout << " da[i="<<i<<"] = (da[i+1="<<i+1<<"]="<< da[i+1]<<")-("<<mOmega<<")*(p["<< mZeroIndex-i<<"]="<< p(mZeroIndex-i)<<")\n";
    da.assign(i, da[i+1] - mOmega * p(mZeroIndex-i-1) );  // note error would be: mZeroIndex-i 'banks' some wealth
  }
  // Add padding for wealth above omega by incrementing the fixed bid b to omega over padding steps
  double w (0.5);           // allow to grow this much
  int    k (mPadding-2) ;   // over this many steps
  double b (mOmega*p(0));   // incrementing this top probability bid
  double m (Line_Search::Bisection(0.00001,std::make_pair(1.00001,1.5))
	    ([&w,&k,&b](double x){ double xk(x); for(int j=1;j<k;++j) xk *= x; return x*(1.0-xk)/(1-x) - w/b;}));
  //  std::cout << " w = " << w << "   k=" << k << "  b=" << b << "   m= " << m << std::endl;
  for(int i=mZeroIndex+1; i < mSize-1; ++i)
  { b *= m;
    da.assign(i, da[i-1] + b);
  }
  // increment last by omega
  da.assign(mSize-1, da[mSize-2]+mOmega);
  mWealth=da;
  // lock in indexing for finding new positions since the increment is known in advance
  mPositions.push_back( std::make_pair(0,0) ) ;
  for(int j = 1; j<mSize-1; ++j)
    mPositions.push_back( find_wealth_position(j,mOmega-bid(j)) );
}

