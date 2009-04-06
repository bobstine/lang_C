// $Id: auction.h,v 3.11 2008/06/19 22:55:08 bob Exp $

#ifndef _AUCTION_H_
#define _AUCTION_H_

/*
  19 Jun 08 ... move to git repository
   2 Jan 08 ... Generic version over model class
  13 Dec 07 ... GSL model as statistical engine, merging ols/wls/logistic code base
   1 Aug 03 ... Created
*/  

#include "my_features.h"
#include "experts.h"
#include "gsl_data.h"
#include "gsl_model.h"

#include <sstream>
#include <vector>

template <class Model>
class Auction
{
 public:
  typedef Auction                                     AuctionClass;
  typedef Model                                       ModelClass;
  typedef typename std::vector<ExpertABC*>            ExpertVector;
  typedef typename std::vector<ExpertABC*>::iterator  ExpertIterator;
  typedef typename std::vector<FeatureABC*>           FeatureVector;
  typedef typename std::pair<double,double>           TestResult;

private:
  bool              mHasActiveExpert;
  int               mRound;
  const double      mPayoff;
  int               mCalibrationDF;     // use calibration if positive
  ExpertVector      mExperts;
  Model&            mModel;
  FeatureVector     mModelFeatures;     // Only those in the model
  FeatureVector     mSkippedFeatures;   // Those tried and not used
  std::ostream&     mLogStream;         // log messages

  
 public:
  Auction (Model& m, int calibrationDF, std::ostream& logStream)
    : mHasActiveExpert(true), mRound(0), mPayoff(0.05), mCalibrationDF(calibrationDF),
    mExperts(), mModel(m), mModelFeatures(), mSkippedFeatures(), mLogStream(logStream) { }
  
  double                 model_goodness_of_fit()    const { return mModel.gof(); }

  int                    number_of_experts ()       const { return mExperts.size(); }
  int                    add_expert(ExpertABC* e)         { mExperts.push_back(e); return mExperts.size(); }
  double                 total_expert_alpha ()      const;  
  bool                   has_active_expert()        const { return mHasActiveExpert; }  
									 
  int                    number_of_predictors()     const { return mModel.q(); }
  int                    number_of_features_tried() const { return mModelFeatures.size() + mSkippedFeatures.size(); }
  FeatureVector const&   model_features ()          const { return mModelFeatures; }
  FeatureVector const&   skipped_features ()        const { return mSkippedFeatures; }
  Model const&           model()                    const { return mModel; }
  bool                   calibrated()               const { return (mCalibrationDF>0); }

  bool                   auction_next_feature (std::ostream&);          // write to output csv file if not null

  void print_to                  (std::ostream& os)                      const;
  void print_model_features_to   (std::ostream& os)                      const;
  void print_model_to            (std::ostream& os, bool useHTML=false)  const;
  void write_model_to            (std::ostream& os)                      const;
  void write_model_data_to       (std::ostream& os)                      const;
  
  void write_csv_header_to       (std::ostream& os)                      const;


 private:
  std::pair<ExpertABC*,double> collect_bids(std::ostream&);
  FeatureABC *  xb_feature(std::vector<double> const& b)  const;
  FeatureABC *  calibration_feature()                     const;
  void          print_features(FeatureVector const& fv)   const;
};

template <class Model>
std::ostream&
operator<<(std::ostream& os, Auction<Model> const& auction);




#include "auction.Template.h"
  
#endif
