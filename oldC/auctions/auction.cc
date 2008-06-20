// $Id: auction.cc,v 1.35 2004/08/25 22:14:40 bob Exp $

/*
   6 Aug 03 ... Created
*/

#include "auction.h"
#include "print_utils.h"
#include "range_ops.h"
#include "smoothing_spline.h"

#include <numeric>
#include <sstream>

bool
Auction::auction_next_feature ()
{  
  BidCollector collector;
  std::cout << "AUCT: Begining auction round #" << mFeatures.size()+1 << std::endl;
  // find the feature with highest total bid
  for(std::vector<BidderABC*>::iterator it = mBidders.begin(); it != mBidders.end(); ++it)
    collector(*it);
  BidderABC::Bid       highBid   (collector.high_bid());
  std::cout << "AUCT: High bid is " << highBid.second << " for feature " << highBid.first->name() << "." << std::endl;
  // identify the bidders for this feature
  std::vector<BidderABC*> bidderVector (collector.bidders_on_feature(highBid.first));
  std::cout << "AUCT: Bidders on " << highBid.first->name() << " are\n" << bidderVector << std::endl;
  // score the predictor
  std::pair<double,double> result (mModel.score_predictor(highBid.first->name(), highBid.first->range(),
							  highBid.first->center(), highBid.first->scale()));
  bool addedPredictor (false);
  if (result.second < 3 * highBid.second)               // passes score test with inflated threshold
  { result = mModel.add_predictor(highBid.first->name(), highBid.first->range(), highBid.first->center(), highBid.first->scale(), highBid.second);
    addedPredictor = (result.second < highBid.second);  // passes likelihood ratio test
    if (addedPredictor)
    {
      if (calibrated())
      { mModel.remove_last_predictor();                 // take new X back off so can update calibrator
	std::cout << "AUCT: Removing prior calibrator from model.\n";
	mModel.remove_last_predictor();                 // take off the calibrator
	mModel.add_predictor(highBid.first->name(), highBid.first->range(), highBid.first->center(), highBid.first->scale(), 1.0); // force in
      }
      if ((mCalibrationDF>0) && (mModel.number_of_predictors() > 2))
      {	std::cout << "AUCT: Adding calibrator with " << mCalibrationDF << " df to underlying logistic model.\n";
	mModel.add_calibrator(mCalibrationDF);
      }
    }
  }
  // save most recent feature
  mFeatures.push_back(highBid.first);
  if (addedPredictor) mModelFeatures.push_back(highBid.first);
  // payoff bidders on this feature (or just collect their bid. this also informs feature of auction result)
  double payoff = (addedPredictor) ? 0.05 : 0.0;
  for (BidCollector::BidderVector::iterator it = bidderVector.begin(); it != bidderVector.end(); ++it)
    (*it)->bid_accepted (payoff, result.second);
  // tell those who passed what happened
  BidCollector::BidderVector passerVector (collector.not_bidders_on_feature(highBid.first));
  for(BidCollector::BidderVector::iterator it = passerVector.begin(); it != passerVector.end(); ++it)
    (*it)->bid_declined (highBid.first);
  return addedPredictor;
}

bool
Auction::has_active_bidder()      const
{
  bool hasBidder (false);
  for (int i=0; i<number_of_bidders(); ++i)
  { 
    hasBidder = mBidders[i]->has_bid();
    if (hasBidder) break;
  }
  return hasBidder;
}  

double
Auction::total_bidder_alpha () const
{
  double total (0.0);
  for (int i=0; i<number_of_bidders(); ++i)
  { double prob (mBidders[i]->alpha());
    total += prob;
  }
  return total;
}

// Output

void
Auction::print_to (std::ostream& os) const
{
  os << std::endl << "     Auction    " << mBidders.size()
     << " bidders with total alpha " << total_bidder_alpha() << std::endl;
  for (int b=0; b<number_of_bidders(); ++b)
    os << "   Bidder #" << b << " " << mBidders[b] << std::endl;
  os << mModel << std::endl;
}

void
Auction::print_features_to (std::ostream& os) const
{
  for (int p=0; p<number_of_features(); ++p)
    os << "   Feature #" << p << ":  " << mFeatures[p] << std::endl;
}

FeatureABC*
Auction::fit_feature(std::vector<double> beta) const
{
  int q (beta.size()-1);                     // dont count intercept
  FeatureVector useFeatures(q);              // only use leading q features (as needed with calibrator)
  for (int j=0; j<q; ++j)
    useFeatures[j] = mModelFeatures[j];
  std::ostringstream oss;
  oss << q;
  std::string name ("Fit_" + oss.str());
  LinearCombinationFeature* xb (mFeatureFactory->make_linear_combination_feature_ptr(beta, useFeatures));
  return mFeatureFactory->make_unary_feature_ptr(Function_Utils::LogisticPos(), xb);
}
 

FeatureABC*
Auction::calibration_feature() const
{
  if (not calibrated())
  { std::cout << "AUCT: ***Error*** Attempt to extract calibrator from uncalibrated model.\n";
    return 0;
  }
  else
  { int q (mModel.number_of_predictors()-1);   // dont count calibrator
    std::ostringstream oss;
    oss << q;
    std::string name ("Cal_" + oss.str());     // Name ought to be used
    return mFeatureFactory->make_unary_feature_ptr(mModel.calibration_operator(),
						   fit_feature(mModel.calibration_beta()));
  }
}

void
Auction::write_model_to  (std::ostream& os) const
{
  os << "Auction Model: ";
  mModel.write_without_calibration_to(os);
  int q (number_of_predictors());
  if (calibrated())
  { --q;                   // dont count the calibrator
    // calibration_feature()->write_to(os);
    // only need to write the operator to the file
    mModel.write_calibrator_to(os);
  }
  for (int j=0; j<q; ++j)
    mModelFeatures[j]->write_to(os);
}
  

void
Auction::write_alphas_to (std::ostream& os) const
{
  double total (0.0);
  for (int b=0; b<number_of_bidders(); ++b)
  { double alpha (mBidders[b]->alpha());
    total += alpha;
    os << alpha << " ";
  }
  os << " " << total << " " << mModel.goodness_of_fit() << std::endl;
}
