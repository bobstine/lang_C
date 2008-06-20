// $Id: wavelet.cc,v 1.7 2000/04/25 16:29:52 bob Exp $-*- c++ -*-

#include <iostream>
#include <vector.h>
#include <list.h>
#include <algo.h>
#include <function.h>
#include <numeric>

#include <assert.h>
#include <math.h>

#include "wavelet.h"
#include "threshold.h"
#include "cyclic.h"
#include "utils.h"


//////////////////  Constructor  //////////////////////////////

Wavelet::Wavelet(const Wave_coef& coefs)
  :
  mN(0),
  mLoCoefs(coefs.lo_pass()),
  mHiCoefs(coefs.hi_pass()),
  mDecomposition()
{
}


//////////////////  Wavelet decomposition  //////////////////////////////

void
Wavelet::decompose(const vector<double> &data)
{
  mN = data.size();
    
  mDecomposition.push_front(data);
  int n = mN;
  while (n>=2)
  {
    n = n/2;
    vector<double> lo (n);
    vector<double> hi (n);
  
    decompose_step(n, mDecomposition.front(), lo.begin(), hi.begin());
    mDecomposition.pop_front();
    mDecomposition.push_front(hi);
    mDecomposition.push_front(lo);
  }
}

void
Wavelet::decompose_step(int count,
		       const vector<double> &inputData,
		       vector<double>::iterator loOutput,
		       vector<double>::iterator hiOutput)
{
  Cyclic_iterator
    input(const_cast<vector<double>::iterator>(inputData.begin()),
	  inputData.end());
  
  while(count>0)
  {
    --count;
    *loOutput = inner_product(mLoCoefs.begin(),mLoCoefs.end(),input,0.0);
    *hiOutput = inner_product(mHiCoefs.begin(),mHiCoefs.end(),input,0.0);
    ++loOutput;
    ++hiOutput;
    ++input;
    ++input;
  }
}

namespace
{
  class accum_ss
  {
  public:
    double operator()(double sum, const vector<double> &c)
      {
	return sum + inner_product(c.begin(), c.end(), c.begin(), 0.0);
      }
  };
}

double
Wavelet::sum_of_squares() const
{
  return accumulate(mDecomposition.begin(), mDecomposition.end(),
		    0.0, accum_ss());
}

  
deque< vector<double> >
Wavelet::estimated_coefficient_deque() const
{
  // return makes a copy of the deque automatically
  return mDecomposition;
}

namespace
{
  class add_to
  {
  public:
    add_to (vector<double> *vec) : mVec(vec)
      {
      }
    void operator()(const double coef)
      {
	(*mVec).push_back(coef);
      }
    void operator()(const vector<double>& coefs)
      {
	for_each(coefs.begin(), coefs.end(), add_to(mVec));
      }
  private:
    vector<double> *mVec;
  };
}

vector<double>
Wavelet::estimated_coefficient_vector() const
{
  vector<double> result;
  for_each(mDecomposition.begin(), mDecomposition.end(), add_to(&result));
  return result;
}

vector<double>
Wavelet::estimated_coefficient_vector(ThresholdBaseClass* thresh) const
{
  // transform the raw wavelet coef estimates
  deque< vector<double> > transCoefs (mDecomposition.size());
  transform(mDecomposition.begin(), mDecomposition.end(),
  	    transCoefs.begin(),
	    ThresholdFunction(thresh));   // avoid if oper() in thresholdbase???

  // pack deque into a vector (as above)  ??? avoid this copy step
  vector<double> result;
  for_each(transCoefs.begin(), transCoefs.end(), add_to(&result));
  return result;  
}

  

//////////////////  Wavelet reconstruction  //////////////////////////////

namespace
{
  class AXpC
  {
  public:
    AXpC (double a): mA(a)
      {
      }
    double operator()(double x, double c)
      {
	return (c + mA * x);
      }
  private:
    double mA;
  };

  class AXpBY  // a X + b Y helper
  {
  public:
    AXpBY (double a, double b): mA(a), mB(b)
      {
      }
    double operator()(double x, double y)
      {
	return (mA*x + mB*y);
      }
  private:
    double mA;
    double mB;
  };

  class accum_wave // accumulator for wavelet
  {
  public:
    accum_wave (const Wavelet &wavelet): mWavelet (wavelet)
      {
      }
    vector<double> operator() (const vector<double> &lo,
			       const vector<double> &hi)
      {
	vector<double> result (2 * lo.size(), 0.0);

	mWavelet.reconstruct_step(hi.begin(), hi.end(),
				 lo.begin(), 
				 Cyclic_iterator(result.begin(),result.end()));
	return result;
      }
  private:
    Wavelet mWavelet;
  }; 
}

vector<double>
Wavelet::reconstruction() const
{
  return reconstruction(mDecomposition.begin(), mDecomposition.end());
}


vector<double>
Wavelet::reconstruction(ThresholdBaseClass* thresh) const
{
  // transform the raw wavelet coef estimates
  deque< vector<double> > transCoefs (mDecomposition.size());
  transform(mDecomposition.begin(), mDecomposition.end(),
  	    transCoefs.begin(),
	    ThresholdFunction(thresh));   // avoid with op in threshbase???
  return reconstruction(transCoefs.begin(), transCoefs.end());
}

vector<double>
Wavelet::reconstruction(deque< vector<double> >::const_iterator it,
		       deque< vector<double> >::const_iterator itEnd) const
{
  // peel off base scaling, wavelet coefs
  double lo, hi;
  lo =  ((*it)[0]);
  ++it;
  hi =  ((*it)[0]);
  ++it;
  
  // initial result from base terms
  vector<double> init(2, 0.0);
  Cyclic_iterator cyclicIt(init.begin(), init.end());
  transform (mLoCoefs.begin(), mLoCoefs.end(),   // have to increment result
	     cyclicIt, cyclicIt,                 // rather than over-write
	     AXpC(lo));
  transform (mHiCoefs.begin(), mHiCoefs.end(),
	     cyclicIt, cyclicIt,
	     AXpC(hi));

  // call the accumulator to build up final reconstruction
  vector<double> result;
  accum_wave accumulator(*this);
  result = accumulate(it, itEnd, init, accumulator);
  return result;
}


template<class iter>
void Wavelet::reconstruct_step(vector<double>::const_iterator hiEst,
			      vector<double>::const_iterator hiEstEnd,
			      vector<double>::const_iterator loEst,
			      iter outputIt) const
{
  while (hiEst != hiEstEnd)
  {
    transform (mLoCoefs.begin(), mLoCoefs.end(),
	       outputIt,
	       outputIt,
	       AXpC(*loEst));
    transform (mHiCoefs.begin(), mHiCoefs.end(),
	       outputIt,
	       outputIt,
	       AXpC(*hiEst));
    ++loEst;
    ++hiEst;
    ++outputIt;
    ++outputIt;
  }
}

////////////////////////////  EOF  ////////////////////////////////////
