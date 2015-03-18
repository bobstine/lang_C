// -*- c++ -*-

#ifndef _AUCTION_H_
#define _AUCTION_H_

/*
  19 Jun 08 ... move to git repository
   2 Jan 08 ... Generic version over model class
  13 Dec 07 ... GSL model as statistical engine, merging ols/wls/logistic code base
   1 Aug 03 ... Created
*/  

#include "auction_base_types.h"

#include "features.h"
#include "experts.h"

#include <sstream>
#include <vector>

template <class Model>
class Auction
{
  
 public:
  typedef Auction                            AuctionClass;
  typedef Model                              ModelClass;
  typedef SCALAR                             Scalar;
  typedef typename std::vector<Expert>       ExpertVector;
  typedef typename std::set<std::string>     StringSet;
  typedef typename std::vector<std::string>  StringVec;
  typedef typename std::pair<Scalar,Scalar>  TestResult;
  
private:
  const Scalar        mPayoff;            // payoff for a winning bid
  const Scalar        mBidTaxRate;        // percentage taken from winning bid
  const Scalar        mPayoffTaxRate;     //                       payoff
  const int           mBlockSize;
  Scalar              mRecoveredAlpha;    // collected from experts that expired
  bool                mTerminating;
  bool                mCalibrateFit;      // use calibration
  std::string         mCalibrationSignature;
  int                 mRound;          
  std::vector<Scalar> mPayoffHistory;     // all prior payoff amounts (positive denote accepted variables)
  int                 mNumInitialExperts; // for building results file  (set when prior to header is written)
  ExpertVector        mExperts;
  StringVec           mPurgedExpertNames; // keep track of the names of any expert that is purged 
  Model&              mModel;
  FeatureVector       mModelFeatures;     // those in the model
  FeatureVector       mRejectedFeatures;  // tried and not used
  std::ostream&       mProgressStream;    // tracks progress by rounds
  
 public:
  ~Auction()
    {
      debugging::debug("AUCT",0) << "Deleting auction. \n";
    }
    
  Auction (Model& m, bool calibrate, std::string calSig, int blockSize, std::ostream& progressStream)
    : mPayoff((Scalar)0.05), mBidTaxRate((Scalar)0.05), mPayoffTaxRate((Scalar)0.40), mBlockSize(blockSize), mRecoveredAlpha((Scalar)0),  mTerminating(false),
      mCalibrateFit(calibrate), mCalibrationSignature(calSig), mRound(0), mPayoffHistory(), mExperts(), mPurgedExpertNames(), mModel(m),
      mModelFeatures(), mRejectedFeatures(), mProgressStream(progressStream) {  } 
  
  Scalar                 model_goodness_of_fit()    const { return mModel.goodness_of_fit(); }

  int                    number_of_experts ()       const { return (int) mExperts.size(); }
  int                    add_expert(Expert e)             { mExperts.push_back(e); return (int)mExperts.size(); }
  Scalar                 total_expert_alpha ()      const;
  Scalar                 recovered_alpha()          const { return mRecoveredAlpha; }
  bool                   is_terminating()           const { return mTerminating; }
  StringVec              purged_expert_names()      const { return mPurgedExpertNames; }
									 
  int                    number_of_predictors()     const { return mModel.dimension(); }
  int                    number_of_features_tried() const { return (int) (mModelFeatures.size() + mRejectedFeatures.size()); }
  int                    number_of_model_features() const { return (int) mModelFeatures.size(); }
  FeatureVector const&   model_features()           const { return mModelFeatures; }
  FeatureVector const&   rejected_features()        const { return mRejectedFeatures; }
  BiddingHistory         auction_history()          const { return BiddingHistory(mPayoffHistory, mModelFeatures, mRejectedFeatures); }
  Model const&           model()                    const { return mModel; }

  void                   prepare_to_start_auction ();
  unsigned int           add_initial_features     (FeatureVector const& f);
  bool                   auction_next_feature     ();          

  void print_to                  (std::ostream& os, bool compact=false)  const;
  void print_model_features_to   (std::ostream& os)                      const;
  void print_model_to            (std::ostream& os, bool useHTML=false)  const;
  void write_model_to            (std::ostream& os)                      const;
  void write_model_data_to       (std::ostream& os, int maxNumXCols)     const;

 private:
  void                     write_header_to_progress_stream ()                            const;

  int                      purge_empty_experts();
  bool                     have_available_bid();
  std::pair<Expert,Scalar> collect_bids();
  Scalar                   tax_bid(Expert e, Scalar bid);
  Scalar                   pay_winning_expert (Expert e, FeatureVector const& fv);
  Scalar                   collect_from_losing_expert (Expert e, Scalar bid, bool singular);
  
  FeatureVector            without_calibration_features(FeatureVector const& fv)         const;
  bool                     is_calibration_feature(Feature const& f)                      const;
  FeatureABC *             xb_feature(std::vector<Scalar> const& b)                      const;
  FeatureABC *             calibration_feature()                                         const;

};




template <class ModelClass>
inline
std::ostream&
operator<<(std::ostream& os, Auction<ModelClass> const& auction) 
{ 
  auction.print_to(os); 
  return os; 
}


#endif
