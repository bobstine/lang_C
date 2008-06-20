// $Id: threshold.cc,v 1.14 2000/04/25 17:30:33 bob Exp $-*- c++ -*-

#include <vector.h>
#include <algo.h>
#include <map.h>
#include <multimap.h>
#include <math.h>

#include "threshold.h"
#include "encoder.h"
#include "coding.h"
#include "utils.h"

//////////////////////////   Constants  //////////////////////////////

// min_length is the length of the shortest wavelet group processed
static const int min_length = 8;

//////////////////////////  Utility class  //////////////////////////

namespace
{
  class Divider
  {
  public:
    Divider::Divider(double divisor):
      mDivisor(divisor)
      {
	assert (divisor != 0.0);  
      }
    double operator()(double numerator) const
      {
	return numerator / mDivisor;
      }
  private:
    double mDivisor;
  };
}

///////////////////////////  Base Class  /////////////////////////////

ThresholdBaseClass::~ThresholdBaseClass ()
{
}

ThresholdBaseClass::ThresholdBaseClass ()
{
}

/////////////////////////////  Hard  ///////////////////////////////////

HardThreshold::~HardThreshold ()
{
}


HardThreshold::HardThreshold(double threshold):
  ThresholdBaseClass(),
  mThreshold(threshold)
{
}
  

namespace
{ class HardThreshOp
  {
  public:
    HardThreshOp (double th): mT(th)
      {
      }
    double operator()(double x)
      { if (x > mT || x < -mT)
	return x;
      else
	return 0.0;
      }
  private:
    double mT;
  };
}
vector<double>
HardThreshold::operator()(const vector<double> &coef) const
{
  int n = coef.size();
  if(n < min_length)
    return coef;
  else
  {  vector<double> result (n);
     transform (coef.begin(), coef.end(),
		result.begin(),
		HardThreshOp(mThreshold));
     return result;
  }
}

/////////////////////////////  Soft  ///////////////////////////////////


SoftThreshold::~SoftThreshold ()
{
}


SoftThreshold::SoftThreshold(double threshold):
  ThresholdBaseClass(),
  mThreshold(threshold)
{
}


namespace
{ class SoftThreshOp
  {
  public:
    SoftThreshOp (double th): mT(th)
      {
      }
    double operator()(double x)
      {
	if (x > mT )
	  return (x - mT);
	else if (x < -mT)
	  return (x + mT);
	else
	  return 0.0;
      }
  private:
    double mT;
  };
}

vector<double>
SoftThreshold::operator()(const vector<double> &coef) const
{
  if((int)coef.size() < min_length)
    return coef;
  else
  {  vector<double> result (coef.size());
     transform (coef.begin(), coef.end(),
		result.begin(),
		SoftThreshOp(mThreshold));
     return result;
  }
}

///////////////////////   Loop Coder  /////////////////////////

LoopThreshold::~LoopThreshold()
{
}

LoopThreshold::LoopThreshold(double se):
  ThresholdBaseClass(),
  mSE(se)
{
}


vector<double>
LoopThreshold::operator()(const vector<double> &coef) const
{
  if((int)coef.size() < min_length)
    return coef;
  else
  {  vector<double> result (coef.size());
     transform(coef.begin(), coef.end(), result.begin(), Divider(mSE));
     return encode_vector(result);
  }
}


///////////////////////   Adaptive   /////////////////////////

AdaptiveThreshold::~AdaptiveThreshold()
{
}

AdaptiveThreshold::AdaptiveThreshold(double se):
  ThresholdBaseClass(),
  mSE(se)
{
}


namespace
{
  const double kSESpacing = 2.0;
  const int    kTableSize = 16;

  class Recoder
    // Converts the input raw coef to pair<rounded est, integer code>
  {
  public:
    Recoder (const double se)
      : mSE (se),
	mScale (se * kSESpacing)
	// mLoopCoder(kSESpacing, kTableSize)
      {
      }
    pair<double,int> operator()(const pair<double,int> &coef)
      {
	// int code = mLoopCoder(coef.first/mSE);
	// return (make_pair(mScale * (double) code, code));
	return make_pair(0.0, 0);
      }
  private:
    double mSE;
    double mScale;
    // no longer used LoopCoder mLoopCoder;
  };
  
  class Bit_calculator
  {
  public:
    Bit_calculator (double totalSS, double se, int p)
      : mTotalSS (totalSS),
	mResidualSS (totalSS),  // initially, fit nothing
	mSE (se),
	mP (p),
	mCodesSoFar(p, 0),
	mCodesSoFarPtr (mCodesSoFar.begin())
      {
      }
    int operator() (const pair<double, int> estCoef,
		    const pair<double, int> codedCoef)
      // Add z for this coef to the fitted model, return total length
      // when its added to those previously added. Decrement res ss.
      {
	// if coded this value, then resid ss drops
	mResidualSS -= estCoef.first * estCoef.first;
	assert (mResidualSS > 0.0);
	// coef is rounded, so add back rel entropy
	double diff = (estCoef.first - codedCoef.first)/mSE;
	mResidualSS += diff*diff;
	assert (mResidualSS <= mTotalSS);
	// move the associated code into code vector for compression
	*mCodesSoFarPtr++ = codedCoef.second;	
	// return (optimal_block_bit_length(mCodesSoFar) +
	//	normal_data_bit_length(mResidualSS));  // assumes sigma^2=1
	return (0);
      }
  private:
    double mTotalSS;
    double mResidualSS;
    double mSE;
    double mP;
    vector <int> mCodesSoFar;  // Accumulates codes in initial zero vector
    vector <int>::iterator mCodesSoFarPtr;
  };

  class GreaterAbs : public binary_function<double, double, bool>
  {
  public:
    bool operator()(const double x, const double y) const
      {
	return ( fabs(x) > fabs(y) );
      }
  };
}

vector<double>
block_code_selection (const vector<double>& coefs, const double se)
{
  // find the total ss associated with whole vector of coefs
  double tss = inner_product(coefs.begin(), coefs.end(), coefs.begin(), 0.0);
  
  // use multimap to sort coefs with index
  int p = coefs.size();
  multimap< double,int,GreaterAbs > coefMap;
  for (int j=0; j<p; ++j)
    coefMap.insert( make_pair(coefs[j],j) );
  assert (p == (int)coefMap.size() );
  // cout << "Coef ests,index pairs are " << coefMap;

  // recode coefs using loop coder to <rounded z, int code> pairs
  vector< pair<double,int> > coefCodes (p);
  transform (coefMap.begin(), coefMap.end(), coefCodes.begin(), Recoder(se));
  // cout << "Coef codes are " << coefCodes;
  
  // hold the bits required, starting with a model having none of these
  vector<int> modelBits (p+1);
  modelBits[0] = 1 + normal_data_bit_length(tss);
  transform (coefMap.begin(), coefMap.end(), coefCodes.begin(),
	     modelBits.begin()+1,  Bit_calculator(tss, se, p));
  // cout << "Model bits for sequential fits are " << modelBits;
  
  // find minimum length
  vector<int>::iterator bitIter;
  bitIter = min_element(modelBits.begin(), modelBits.end());
  int nCoefsToKeep = bitIter - modelBits.begin();

  // put the retained coefs into result
  vector<double> result (p, 0.0);
  map<double,int>::const_iterator estPtr(coefMap.begin());
  for (int j=0; j<nCoefsToKeep; ++j)
  {
    result[ (*estPtr++).second ] = coefCodes[j].first;
  }
  return result;
}

vector<double>
AdaptiveThreshold::operator()(const vector<double> &coef) const
{
  /*
    if((int)coef.size() < min_length)
    return coef;
    else
    return block_code_selection (coef, mSE);
  */
  vector<double> result;
  return result;
}
/////////////////////   Wrapper class for use with STL  ////////////////////


ThresholdFunction::ThresholdFunction(ThresholdBaseClass *thresh):
    mThresh(thresh)
    {
    };

vector<double>
ThresholdFunction::operator()(const vector<double> &ests) const
{
  return (*mThresh)(ests);
}



//////////////////////////////   EOF  ////////////////////////////
