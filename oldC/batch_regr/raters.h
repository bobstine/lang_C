// $Id: raters.h,v 1.5 2003/06/21 12:23:33 bob Exp $

/*
  22 May 03 ... Ported from old sweeper code for use in C++ sequential models.
*/

#ifndef _RATERS_H_
#define _RATERS_H_

#include "sweep_matrix.h"
#include "datasets.h"
#include "projector.h"

// From range code
#include "range.h"

/*
  The various raters sort the available predictors using an honest
  estimate of the reduction in the residual SS obtained by adding a
  predictor.

  If none of the predictors guarantees a reduction in the SS at the
  indicated level of confidence, then the returned vector is empty. It
  contains a list of *only* those that guarantee a reduction, up to
  the number k requested by recommend_predictors.
*/


// CODING  CODING  CODING  CODING  CODING  CODING  CODING  CODING  CODING  CODING  CODING  CODING  

class TwoPartCodingRater
{
  SweepMatrix const& mSM;
  int const mN;
  
 public:

  TwoPartCodingRater (SweepMatrix const& sm, int n)
    : mSM(sm), mN(n) {}

  std::vector<int>
    recommend_predictors (int k) const;
};

//  STEPWISE  STEPWISE  STEPWISE  STEPWISE  STEPWISE  STEPWISE  STEPWISE  STEPWISE  STEPWISE  STEPWISE

class StepwiseRater
{
  SweepMatrix const& mSM;
  int const mN;
  
 public:

  StepwiseRater (SweepMatrix const& sm, int n)
    : mSM(sm), mN(n) {}

  std::vector<int>
    recommend_predictors (int k, double pValue) const;

};


//  WEIGHTED STEPWISE  WEIGHTED STEPWISE  WEIGHTED STEPWISE  WEIGHTED STEPWISE  WEIGHTED STEPWISE

template <class Data>
class WeightedRater
{
  SweepMatrix const& mSM;
  Data const& mData;
  std::vector<double> mWeights;
  
 public:
  
  WeightedRater (SweepMatrix const& sm, Data const& data, std::vector<double> weights)
    : mSM (sm), mData(data), mWeights(weights) {}
  
  std::vector<int>
    recommend_predictors (int k, double pValue)
    { Projector<Data> projector (mData, mSM);
      return pick(k, pValue, projector.weighted_partial_residual_ss_vector(mWeights));
    }
  
 private:

  std::vector<int>
    pick (int k, double pValue, std::vector<double> const& wts) const;
};


template <class Data>
WeightedRater<Data>
make_white_rater (SweepMatrix const& sm, Data const& data)
{
  Projector<Data> projector(data,sm);
  std::vector<double> e (projector.ols_residual_vector());
  for (unsigned int i=0; i<e.size(); ++i)
    e[i] *= e[i];
  return WeightedRater<Data>(sm, data, e);
}
  

template <class Data>
WeightedRater<Data>
make_binomial_rater (SweepMatrix const& sm, Data const& data)
{
  Projector<Data> projector(data,sm);
  std::vector<double> fit (projector.truncated_ols_fit_vector(0.00001, 0.99999));
  for (unsigned int i=0; i<fit.size(); ++i)
    fit[i] *= (1.0 - fit[i]);
  return WeightedRater<Data>(sm, data, fit);
}


//  BENNETT  BENNETT  BENNETT  BENNETT  BENNETT  BENNETT  BENNETT  BENNETT  BENNETT  BENNETT  BENNETT  

template <class Data>
class BennettRater
{
  SweepMatrix const& mSM;
  Data const& mData;
  std::vector<double> mWeights;
  
 public:
  
  BennettRater (SweepMatrix const& sm, Data const& data, std::vector<double> weights)
    : mSM (sm), mData(data), mWeights(weights) {}
  
  std::vector<int>
    recommend_predictors (int k, double pValue)
    { Projector<Data> projector (mData, mSM);
      std::pair< std::vector<double>, std::vector<double> > aAndB (projector.bennett_residual_pair(mWeights));
      return pick(k, pValue, aAndB.first, aAndB.second);
    }
  
 private:

  std::vector<int>
    pick (int k, double pValue, std::vector<double> const& a, std::vector<double> const& b) const;
};


template <class Data>
BennettRater<Data>
make_bennett_white_rater (SweepMatrix const& sm, Data const& data)
{
  Projector<Data> projector(data,sm);
  std::vector<double> e (projector.ols_residual_vector());
  for (unsigned int i=0; i<e.size(); ++i)
    e[i] *= e[i];
  return BennettRater<Data>(sm, data, e);
}


template <class Data>
BennettRater<Data>
make_bennett_binomial_rater (SweepMatrix const& sm, Data const& data)
{
  Projector<Data> projector(data,sm);
  std::vector<double> fit (projector.truncated_ols_fit_vector(0.00001,0.99999));
  for (unsigned int i=0; i<fit.size(); ++i)
    fit[i] *= (1.0 - fit[i]);
  return BennettRater<Data>(sm, data, fit);
}


#endif
