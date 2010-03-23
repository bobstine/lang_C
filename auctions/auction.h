// $Id: auction.h,v 3.11 2008/06/19 22:55:08 bob Exp $

#ifndef _AUCTION_H_
#define _AUCTION_H_

/*
  19 Jun 08 ... move to git repository
   2 Jan 08 ... Generic version over model class
  13 Dec 07 ... GSL model as statistical engine, merging ols/wls/logistic code base
   1 Aug 03 ... Created
*/  

#include "features.h"
#include "experts.h"
#include "gsl_data.h"
#include "gsl_model.h"

#include <sstream>
#include <vector>

template <class Model>
class Auction
{
 public:
  typedef Auction                            AuctionClass;
  typedef Model                              ModelClass;
  typedef typename std::vector<Expert>       ExpertVector;
  typedef typename std::set<std::string>     StringSet;
  typedef typename std::pair<double,double>  TestResult;

  
private:
  const double        mPayoff;            // payoff for a winning bid
  const double        mBidTaxRate;        // percentage taken from winning bid
  const double        mPayoffTaxRate;     //                       payoff
  const int           mBlockSize;
  double              mRecoveredAlpha;    // collected from experts that expired
  bool                mHasActiveExpert;
  bool                mCalibrateFit;      // use calibration
  int                 mRound;          
  std::vector<double> mPayoffHistory;     // all prior payoff amounts (positive denote accepted variables)
  int                 mNumInitialExperts; // for building csv file  (set when prior to csv header is written)
  ExpertVector        mExperts;
  Model&              mModel;
  FeatureVector       mModelFeatures;     // those in the model
  FeatureVector       mRejectedFeatures;  // tried and not used
  FeatureSource       mFeatureSource; 
  std::ostream&       mLogStream;         // send log messages

  
 public:
  ~Auction()
    {
      debugging::debug("AUCT",0) << "Deleting auction. \n";
    }
    
  Auction (Model& m, FeatureSource const& featureSrc, bool calibrate, int blockSize, std::ostream& logStream)
    : mPayoff(0.05), mBidTaxRate(0.05), mPayoffTaxRate(0.40), mBlockSize(blockSize), mRecoveredAlpha(0),  mHasActiveExpert(true),
      mCalibrateFit(calibrate), mRound(0), mPayoffHistory(), mExperts(), mModel(m),
      mModelFeatures(), mRejectedFeatures(), mFeatureSource(featureSrc), mLogStream(logStream) {  } 
  
  double                 model_goodness_of_fit()    const { return mModel.gof(); }

  int                    number_of_experts ()       const { return mExperts.size(); }
  int                    add_expert(Expert e)             { mExperts.push_back(e); return mExperts.size(); }
  double                 total_expert_alpha ()      const;
  double                 recovered_alpha()          const { return mRecoveredAlpha; }
  bool                   has_active_expert()        const { return mHasActiveExpert; }  
									 
  int                    number_of_predictors()     const { return mModel.q(); }
  int                    number_of_features_tried() const { return mModelFeatures.size() + mRejectedFeatures.size(); }
  FeatureVector const&   model_features()           const { return mModelFeatures; }
  FeatureVector const&   rejected_features()        const { return mRejectedFeatures; }
  AuctionState           auction_state()            const { return AuctionState(mPayoffHistory, mModelFeatures, mRejectedFeatures); }
  Model const&           model()                    const { return mModel; }

  bool                   auction_next_feature (std::ostream&);          // write to output csv file if not null

  void print_to                  (std::ostream& os)                      const;
  void print_model_features_to   (std::ostream& os)                      const;
  void print_model_to            (std::ostream& os, bool useHTML=false)  const;
  void write_model_to            (std::ostream& os)                      const;
  void write_model_data_to       (std::ostream& os)                      const;
  


 private:
  void                     write_csv_header_to (std::ostream& os)                        const;

  int                      purge_empty_experts();
  std::pair<Expert,double> collect_bids(std::ostream&);
  double                   tax_bid(Expert e, double bid);
  double                   pay_winning_expert (Expert e, FeatureVector const& fv);
  double                   collect_from_losing_expert (Expert e, double bid, bool singular);

  FeatureABC *             xb_feature(std::vector<double> const& b)                      const;
  FeatureABC *             calibration_feature()                                         const;
  void                     print_features(FeatureVector const& fv)                       const;
};



template <class Model>
std::ostream&
operator<<(std::ostream& os, Auction<Model> const& auction);




#include "auction.Template.h"
  
#endif
