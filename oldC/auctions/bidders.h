// $Id: bidders.h,v 1.10 2004/04/21 13:01:57 bob Exp $

#ifndef _BIDDERS_H_
#define _BIDDERS_H_

/*  
  Bidders bid on features offered by recommenders.  BidderABC holds
  the methods for tracking the wealth of the bidder and interacting
  the with auction.  Inherited classes define a method for assigning
  probabilities to associated recommender.

  Bidders possess wealth as an alpha rate that the bidder consumes when
  it is wrong and expands when it anticipates a used predictor.  The
  bidder 'spends' this alpha along the way as the auction proceeds,
  earning more chance for error when it gets one right.

   6 Apr 04 ... Smarter constant bidder spreads wealth evenly over the rest.
  25 Mar 04 ... Next version
   6 Aug 03 ... Use objects to have one class so can put them in one collection.
  29 Jul 03 ... Created.
*/

#include "my_features.h"
#include "recommenders.h"
#include <iostream>
#include <vector>
#include <map>

class BidderABC
{
 public:
  typedef FeatureABC*                   FeaturePtr;
  typedef FeatureABC const*             ConstFeaturePtr;
  typedef std::pair<FeaturePtr, double> Bid;
  
 protected:
  RecommenderABC*  mRecommender;

 private:
  std::string      mName;
  double           mAlpha;
  double           mLastBidAmt;
  
 public:
  virtual ~BidderABC () { };
  
  BidderABC ()
    : mRecommender(0), mName("empty"), mAlpha(0.0), mLastBidAmt (0.0) {}
  BidderABC (std::string const& name, RecommenderABC* r, double alpha)
    : mRecommender(r), mName(name), mAlpha(alpha), mLastBidAmt (0.0) {}

  std::string const& name()                                   const { return mName; }
  double             alpha ()                                 const { return mAlpha; }
  RecommenderABC*    recommender()                            const { return mRecommender; }

  bool               has_bid ()                               const { return ((mAlpha > 0) && mRecommender->has_a_feature()); }
  
  Bid                bid ()                                         { mLastBidAmt = bid_amount();
                                                                      return std::make_pair(bid_feature(), mLastBidAmt); }

  virtual double     bid_amount ()                            const = 0;
  FeaturePtr         bid_feature ()                           const { return mRecommender->feature(); }
  void               bid_accepted (double payoff, double pval)      { mAlpha += payoff - mLastBidAmt;
                                                                      mRecommender-> update_history(payoff > 0, pval); }

  virtual void       bid_declined (ConstFeaturePtr f)                { mRecommender->chosen_feature(f); } // virtual, f is feature chosen
  virtual void       print_to (std::ostream& os)  const;  
};

inline
std::ostream&
operator<< (std::ostream& os, BidderABC* bidder)
{ bidder->print_to(os); return os; }

inline
std::ostream&
operator<< (std::ostream& os, BidderABC::Bid const& bid)
{
  os << " Bid " << bid.second << " on " << bid.first;
  return os;
}

inline
std::ostream&
operator<< (std::ostream& os, std::vector<BidderABC*> s)
{
  os << "      { ";
  std::copy(s.begin(), s.end(), std::ostream_iterator<BidderABC*>(os,"\n        "));
  os << "}";
  return os;
}


////  ConstantBidder  ConstantBidder  ConstantBidder  ConstantBidder  ConstantBidder  ConstantBidder  

class ConstantBidder : public BidderABC
{
 public:
  ~ConstantBidder () { }
  
  ConstantBidder ()
    : BidderABC() {  }
  ConstantBidder (std::string const& name, RecommenderABC* r, double alpha)
    : BidderABC(name, r, alpha) { }

  double bid_amount () const;

  void   print_to (std::ostream& os) const { os << "Constant   "; BidderABC::print_to(os); }
}; 



////  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder

class GeometricBidder : public BidderABC
{
  const double mRate;
  
 public:
  ~GeometricBidder () {}
  
  GeometricBidder ()
    : BidderABC(), mRate(0.5) {}
  GeometricBidder (std::string const& name, RecommenderABC* r, double alpha, double rate)
    : BidderABC(name, r, alpha), mRate(rate) {}
  
  double bid_amount () const;

  void   print_to (std::ostream& os) const  { os << "Geom (" << mRate << ") "; BidderABC::print_to(os); }
};


////  BidCollector  BidCollector  BidCollector  BidCollector  BidCollector  BidCollector  BidCollector  

class BidCollector
{
 public:
  typedef std::vector<BidderABC*>          BidderVector;

 private:
  typedef std::map<BidderABC::FeaturePtr, double>       AmountMap;
  typedef std::map<BidderABC::FeaturePtr, BidderVector> BidderMap;
  
  AmountMap mAmounts;
  BidderMap mBidders;
  
public:
  BidCollector()
    : mAmounts(), mBidders() { }

  double          operator()(BidderABC* bidder);
  
  BidderABC::Bid  high_bid ()                                     const;
  BidderVector    bidders_on_feature(BidderABC::FeaturePtr f)           { return  mBidders[f]; }
  BidderVector    not_bidders_on_feature(BidderABC::FeaturePtr f) const;
};

#endif
