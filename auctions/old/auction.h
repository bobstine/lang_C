// $Id: auction.h,v 1.1 2008/01/20 19:55:40 bob Exp $

#ifndef _AUCTION_H_
#define _AUCTION_H_

/*
   2 Jan 08 ... Generic version over model class
  13 Dec 07 ... GSL model as statistical engine, merging ols/wls/logistic code base
   1 Aug 03 ... Created
*/

#include "bidders.h"
#include "gsl_data.h"
#include "gsl_model.h"

#include <vector>

template <class Model>
class Auction
{
 public:
  typedef Auction                                     AuctionClass;
  typedef Model                                       ModelClass;
  typedef typename std::vector<BidderABC*>            BidderVector;
  typedef typename std::vector<BidderABC::FeaturePtr> FeatureVector;
  typedef typename std::pair<double,double>           TestResult;

private:
  const double      mPayoff;
  int               mCalibrationDF;     // use calibration if positive
  BidderVector      mBidders; 
  Model&            mModel;
  FeatureVector     mFeatures;          // All features that have entered the auction.
  FeatureVector     mModelFeatures;     // Only those in the model.
  FeatureFactory*   mFeatureFactory;
  
 public:
  Auction (BidderVector const& bidders, Model& m, int calibrationDF, FeatureFactory* ff)
    : mPayoff(0.05), mCalibrationDF(calibrationDF), mBidders(bidders), mModel(m), mFeatures(), mModelFeatures(), mFeatureFactory(ff) { }
  
  double                 total_bidder_alpha ()      const;
  bool                   has_active_bidder()        const;
  int                    number_of_bidders ()       const { return mBidders.size(); }
  int                    add_bidder(BidderABC* pb)        { mBidders.push_back(pb); return mBidders.size(); }
									 
  int                    number_of_predictors()     const { return mModel.q(); }
  int                    number_of_features()       const { return mFeatures.size(); }
  FeatureVector const&   model_features ()          const { return mModelFeatures; }
  FeatureVector const&   features ()                const { return mFeatures; }
  Model const&           model()                    const { return mModel; }
  bool                   calibrated()               const { return (mCalibrationDF>0); }
  bool                   auction_next_feature ();  

  void print_to            (std::ostream& os)       const;
  void print_features_to   (std::ostream& os)       const;
  void write_model_to      (std::ostream& os)       const;
  void write_model_data_to (std::ostream& os)       const { mModel.write_data_to(os); }
  void write_alphas_to     (std::ostream& os)       const;

 private:
  FeatureABC *  xb_feature(std::vector<double> const& b)          const;
  FeatureABC *  calibration_feature()                             const;
};

template <class Model>
std::ostream&
operator<<(std::ostream& os, Auction<Model> const& auction);

                            
//    Make the auction a source of features to itself so that the auction can
//    pick up explanatory variables once they appear in the auction.

template <class Auction>
class AuctionModelFeatures
{
  typename Auction::AuctionClass const& mAuction;
  
 public:
  typedef typename Auction::FeatureVector Vector;
  
  ~AuctionModelFeatures() { }

  AuctionModelFeatures(Auction const& a)
    : mAuction(a) { };

  FeatureABC const*   operator[](int j) const { return (mAuction.model_features()[j]); }    // act like a vector
  int                 size()            const { return mAuction.model_features().size(); }

  Vector const&       features()        const { return mAuction.model_features(); }
};

template <class Auction>
AuctionModelFeatures<typename Auction::AuctionClass>
make_auction_model_features (Auction const& a)
{
  return AuctionModelFeatures<Auction>(a);
}


#include "auction.Template.h"
  
#endif
