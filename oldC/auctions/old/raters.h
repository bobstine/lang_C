// $Id: raters.h,v 1.1.1.1 2003/08/06 21:57:31 bob Exp $

#ifndef _RATERS_H_
#define _RATERS_H_

/*
  Bettors rate and wager on variables offered by prediction streams.
  A bettor assigns probabilities to the available streams, and from
  this probability constructs its wager in an auction.  Raters are
  used to customize the behaviour of bettors, assigning probabilities
  to streams.

  01 Aug 03 ... Created.

*/

#include "predictor_stream.h"
#include <iostream>


namespace {
  double max(double a, double b)  { return (a<b) ? a : b; }
}


class GeometricRater
{
  const PredictorType mType;
  const double mRate;
 public:
  GeometricRater ()                                : mType(ANY ), mRate(0.5)  { }
  GeometricRater (PredictorType type)              : mType(type), mRate(0.5)  { }
  GeometricRater (PredictorType type, double rate) : mType(type), mRate(rate) { }
  
  template <class PredStream>
  double operator()(PredStream const& preds) const
    { if (mType == ANY || mType == preds.predictor_type())
      { double result (rate);
	int n (preds.num_predictors_tried_since_last_used());
	while (n) { result *= rate; --n; }
	return result;
      }
      else
      { std::cout << "RATE: stream type " << preds.predictor_type() << " does not match my type " << mType << std::endl;
	return 0.0;
      }
    }
};


class ConstantRater
{
  const PredictorType mType;
 public:
  ConstantRater ()                   : mType(ANY) { }
  ConstantRater (PredictorType type) : mType(type) { }
  
  template <class PredStream>
  double operator()(PredStream const& preds) const
    { if (mType == ANY || mType == preds.predictor_type())
      { int p (preds.num_predictors_possible());
	return (1.0/p);
      }
      else
      { std::cout << "RATE: stream type " << preds.predictor_type() << " does not match my type " << mType << std::endl;
	return 0.0;
      }
    }
}; 
  
#endif
