// $Id: bidders.h,v 1.1 2008/01/20 19:55:40 bob Exp $

#ifndef _BIDDERS_H_
#define _BIDDERS_H_

/*  
  A bidder represents a recommender in the auction. A bidder has but
  one recommender (recommenders can have several bidders).

  Bidders price features offered by recommenders.  BidderABC holds
  the methods for tracking the wealth of the bidder and interacting
  the with auction.  Inherited classes define a method for assigning
  probabilities to associated recommender.

  Bidders possess wealth as an alpha rate that the bidder consumes when
  it is wrong and expands when it anticipates a used predictor.  The
  bidder 'spends' this alpha along the way as the auction proceeds,
  earning more chance for error when it gets one right.

  16 Jan 08 ... Add universal bidder; make payoff conform to JRSS results. No pvalue.
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

//   BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  
/*
 This is the 0/1 vector from JRSS paper
 */

class BidHistory 
{
  std::vector<int> mResults;
  int mBidsSinceLastSuccess;
  int mNumberSuccessfulBids;
  
public:
    BidHistory ()
    : mResults(), mBidsSinceLastSuccess(0), mNumberSuccessfulBids(0) { }
  
  std::vector<int> const&  results()                       const { return mResults; }
  std::pair<int,int>       bid_results_summary()           const { int n (mResults.size()); return std::make_pair(mNumberSuccessfulBids, n-mNumberSuccessfulBids); }
  int                      number_bids_since_last_sucess() const { return mBidsSinceLastSuccess; }

  void                     append_bid_outcome (bool success);

};
  
//   FiniteBidder    FiniteBidder    FiniteBidder    FiniteBidder    FiniteBidder    FiniteBidder    FiniteBidder   

template <class Stream>
class FiniteBidder
{
  Stream const& mStream;
  
public:
  
  FiniteBidder (Stream const& stream) 
  : mStream(stream) { }
  
  std::string name() const { return "Finite bidder"; }
  
  double bid (double alpha, BidHistory const& history) const { return alpha/mStream.number_remaining(); }
  
}; 


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

  std::string const& name()           const { return mName; }
  double             alpha ()         const { return mAlpha; }
  double             max_bid ()       const { return mAlpha/(1.0-mAlpha); }
  RecommenderABC*    recommender()    const { return mRecommender; }

  bool               has_bid ()       const { return ((mAlpha > 0) && mRecommender->has_a_feature()); }
  Bid                bid ()                 { mLastBidAmt = bid_amount(); return std::make_pair(bid_feature(), mLastBidAmt); }
  FeaturePtr         bid_feature ()   const { return mRecommender->feature(); }               // name feature

  virtual double     bid_amount   ()  const = 0;
  virtual void       bid_outcome  (const FeatureABC* f, double payoff);                       // payoff 0 implies was not successful
  virtual void       bid_declined (const FeatureABC* f)  { mRecommender->chosen_feature(f); } // for those who had to sit out a round of auction
  
  const RecommenderABC* print_to (std::ostream& os)  const;  
};

inline
std::ostream&
operator<< (std::ostream& os, BidderABC* bidder)
{
  const RecommenderABC* p_rec = bidder->print_to(os);
  os << ", ";
  p_rec->print_to(os);
  return os;
}

inline
std::ostream&
operator<< (std::ostream& os, BidderABC::Bid const& bid)
{
  os << " Bid " << bid.second << " on " << bid.first << std::endl;
  return os;
}

// operators for a collection of bidders

double
total_bid(const std::vector<BidderABC*>& s);

std::ostream&
operator<< (std::ostream& os, const std::vector<BidderABC*>& s);


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

  const RecommenderABC*
    print_to (std::ostream& os) const { os << "Constant   "; return BidderABC::print_to(os); }
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
  const RecommenderABC*
    print_to (std::ostream& os) const  { os << "Geom (" << mRate << ") "; return BidderABC::print_to(os); }
};



////  UniversalBidder  UniversalBidder  UniversalBidder  UniversalBidder  UniversalBidder  UniversalBidder

class UniversalBidder : public BidderABC
{
  int mBidsSinceLastWinner;
  
public:
  ~UniversalBidder () {}
  
  UniversalBidder ()
  : BidderABC() {}
  UniversalBidder (std::string const& name, RecommenderABC* r, double alpha)
  : BidderABC(name, r, alpha), mBidsSinceLastWinner(1) {}       // 1 means this is first bid since winning
  
  double bid_amount () const;
  
  void bid_outcome(FeatureABC* f, bool result) { BidderABC::bid_outcome(f,result); mBidsSinceLastWinner = (result) ? 1 : 1+mBidsSinceLastWinner; }
  
  const RecommenderABC*  print_to (std::ostream& os) const  { os << "Universal "; return BidderABC::print_to(os); }
};


#endif
