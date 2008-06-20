// $Id: bidders.cc,v 1.1 2008/01/20 19:55:40 bob Exp $

#include "bidders.h"

// universal PDF 
#include "coding.h"

#include <math.h>

namespace {
  double min(double a, double b)  { return (a<b) ? a : b; }
}

//  BidHistory    BidHistory    BidHistory    BidHistory    BidHistory    BidHistory    BidHistory   


void               
BidHistory::append_bid_outcome (bool success)     
{ 
  int result ((success) ? 1 : 0); 
  mResults.push_back(result); 
  if (success) 
  { mBidsSinceLastSuccess = 0;
    ++mNumberSuccessfulBids;
  }
  else
    ++mBidsSinceLastSuccess;
}


//  BidderABC  BidderABC  BidderABC  BidderABC  BidderABC  BidderABC  BidderABC  BidderABC  

const RecommenderABC*
BidderABC::print_to (std::ostream& os) const
{
  os << " " << mName << " @ " << bid_amount() << " of " <<  mAlpha;
  return mRecommender;
}


void
BidderABC::bid_outcome (FeatureABC const* f, double payoff)
{
  mAlpha += payoff - mLastBidAmt/(1-mLastBidAmt);
  if (mAlpha < 0.0)
  { std::cout << "BIDR: Bidder " << name() << " has run into negative alpha; set to zero.\n";
    mAlpha = 0.0;
  }
  mRecommender-> update_history(f, payoff > 0);   // causes to make next feature
}


//  BidCollector  BidCollector  BidCollector  BidCollector  BidCollector  BidCollector  BidCollector

double
BidCollector::operator()(BidderABC* bidder)
{
  if (bidder->has_bid())
  { BidderABC::Bid bid(bidder->bid());
    mAmounts[bid.first] += bid.second;
    mBidders[bid.first].push_back(bidder);
    return bid.second;
  }
  else
    { //std::cout << "BDCL: Bidder " << bidder->name() << " does not have a bid.\n";
      return 0.0;
    }
}

BidderABC::Bid
BidCollector::high_bid () const
{ double maxBid(0.0);
  BidderABC::FeaturePtr f(0) ;
  for (AmountMap::const_iterator it=mAmounts.begin(); it != mAmounts.end(); ++it)
    if (it->second > maxBid)
    { maxBid = it->second;
      f = it->first;
    }
  return std::make_pair(f,maxBid);
}

BidCollector::BidderVector
BidCollector::not_bidders_on_feature(BidderABC::FeaturePtr f) const
{
  BidderVector bv;
  for (BidderMap::const_iterator it=mBidders.begin(); it != mBidders.end(); ++it)
    if (it->first != f)
      std::copy(it->second.begin(), it->second.end(), std::back_inserter(bv));
  return bv;
}


/////////   Printing collection of bidders 

double
total_bid(const std::vector<BidderABC*>& s)
{
  double result = 0;
  for(std::vector<BidderABC*>::const_iterator i = s.begin(); i != s.end(); ++i)
    result += (*i)->bid_amount();
  return result;
}

std::ostream&
operator<< (std::ostream& os, const std::vector<BidderABC*>& s)
{
  os << "      { ";
  if(s.size() < 5)
  { std::set<const RecommenderABC*> r;
    for(std::vector<BidderABC*>::const_iterator i = s.begin(); i != s.end(); ++i)
    { (*i)->print_to(os);
      r.insert((*i)->recommender());
      os << "\n        ";
    };
    os << "    - - - Recommenders - - -";
    for(std::set<const RecommenderABC*>::const_iterator i = r.begin(); i != r.end(); ++i)
    { os << "\n         ";
      (*i)->print_to(os);
    }
  }
  else
  { std::set<const RecommenderABC*> r;
    double total = total_bid(s);
    double not_printed = 0;
    for(std::vector<BidderABC*>::const_iterator i = s.begin(); i != s.end(); ++i)
    { if((*i)->bid_amount() > total / 10.)
    { (*i)->print_to(os);
      os << "\n        ";
    }
      else
        not_printed += (*i)->bid_amount();
      r.insert((*i)->recommender());
    };
    os << "(total = " << total << " = above + " << not_printed << ")\n        ";
    os << "    - - - Recommenders - - -";
    for(std::set<const RecommenderABC*>::const_iterator i = r.begin(); i != r.end(); ++i)
    { os << "\n         ";
      (*i)->print_to(os);
    }
  }
  os << "\n      }";
  return os;
}


//  ConstantBidder  ConstantBidder  ConstantBidder  ConstantBidder  ConstantBidder  ConstantBidder  

double
ConstantBidder::bid_amount () const
{
  double nLeft (recommender()->number_remaining());
  double bid   (alpha()/nLeft);
  double max   (max_bid());
  return (bid < max) ? bid : max;
}


//  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  

double
GeometricBidder::bid_amount() const
{
  double bid (mRate * alpha());
  double max (max_bid());
  return (bid < max) ? bid : max;
}



//  UniversalBidder  UniversalBidder  UniversalBidder  UniversalBidder  UniversalBidder  UniversalBidder  


double
UniversalBidder::bid_amount() const
{
  double bid (universalPDF(mBidsSinceLastWinner) * alpha());
  double max (max_bid());
  return (bid < max) ? bid : max;
}


