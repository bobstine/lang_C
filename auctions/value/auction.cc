// $Id: auction.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

/*
   6 Aug 03 ... Created
*/

#include "auction.h"
#include "print_utils.h"
#include "range_ops.h"

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
  std::cout << "AUCT: High bid is " << highBid.second << " for feature " << highBid.first.name() << "." << std::endl;
  // identify the bidders for this feature
  std::vector<BidderABC*> bidderVector (collector.bidders_on_feature(highBid.first));
  std::cout << "AUCT: Bidders on " << highBid.first.name() << " are\n" << bidderVector << std::endl;
  // try the indicated feature
  double center,scale;
  if (highBid.first.is_dummy())
  { center = 0; scale = 1; }
  else
  { center = highBid.first.center();
    scale = highBid.first.scale();
  }
  std::pair<double,double> result (mModel.add_predictor(highBid.first.name(), highBid.first.range(),
							center, scale, highBid.second));
  bool addedPredictor (result.second < highBid.second);
  // save feature
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

void
Auction::write_model_to  (std::ostream& os) const
{
  os << "Auction Model: ";
  mModel.write_to(os);
  for (int p=0; p<number_of_predictors(); ++p)
    mModelFeatures[p].write_to(os);
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
