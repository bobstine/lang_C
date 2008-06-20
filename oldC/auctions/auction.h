// $Id: auction.h,v 1.30 2004/08/25 22:14:40 bob Exp $

#ifndef _AUCTION_H_
#define _AUCTION_H_

/*
   1 Aug 03 ... Created
*/

#include "bidders.h"
#include "log_regr.h"

#include <vector>


class AuctionModelFeatures;

class Auction
{
 public:
  typedef std::vector<BidderABC*>            BidderVector;
  typedef std::vector<BidderABC::FeaturePtr> FeatureVector;
  typedef LogisticRegression                 Model;
  
 private:
  int               mCalibrationDF;     // use calibration if positive
  BidderVector      mBidders;           
  Model             mModel;
  FeatureVector     mFeatures;          // All features that have entered the auction.
  FeatureVector     mModelFeatures;     // Only those in the model.
  FeatureFactory*   mFeatureFactory;
  
 public:
  Auction (BidderVector const& bidders, Model const& m, int calibrationDF, FeatureFactory* ff)
    : mCalibrationDF(calibrationDF), mBidders(bidders), mModel(m), mFeatures(), mModelFeatures(), mFeatureFactory(ff) { }
  
  double                 total_bidder_alpha ()      const;
  bool                   has_active_bidder()        const;
  int                    number_of_bidders ()       const { return mBidders.size(); }
  int                    add_bidder(BidderABC* pb)        { mBidders.push_back(pb); return mBidders.size(); }
									 
  int                    number_of_predictors()     const { return mModel.number_of_predictors(); }
  int                    number_of_features()       const { return mFeatures.size(); }
  FeatureVector const&   model_features ()          const { return mModelFeatures; }
  FeatureVector const&   features ()                const { return mFeatures; }
  Model const&           model()                    const { return mModel; }
  bool                   calibrated()               const { return mModel.calibrated(); }
  bool                   auction_next_feature ();  

  void print_to            (std::ostream& os)       const;
  void print_features_to   (std::ostream& os)       const;
  void write_model_to      (std::ostream& os)       const;
  void write_model_data_to (std::ostream& os)       const { mModel.write_data_to(os); }
  void write_alphas_to     (std::ostream& os)       const;

 private:
  void          add_calibrator_to_model();
  FeatureABC *  fit_feature(std::vector<double> b)  const;
  FeatureABC *  calibration_feature()               const;
};


inline
std::ostream&
operator<<(std::ostream& os, Auction const& auction)
{ auction.print_to(os); return os; }


////  Feature source from the auction itself

class AuctionModelFeatures
{
  Auction const& mAuction;
  
 public:
  typedef Auction::FeatureVector Vector;
  
  ~AuctionModelFeatures() { }

  AuctionModelFeatures(Auction const& a)
    : mAuction(a) { };

  FeatureABC const*   operator[](int j) const { return (mAuction.model_features()[j]); }    // act like a vector
  int                 size()            const { return mAuction.model_features().size(); }

  Vector const&       features()        const { return mAuction.model_features(); }
};
  
#endif
