// -*- c++ -*-
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

  Bid history tracks the bids of this expert. Payoff history tracks
  the auction as a whole rather than just a specific bidder.

  19 Jan 08 ... Revised to simplified policy class, without an ABC
  16 Jan 08 ... Add universal bidder; make payoff conform to JRSS results. No pvalue.
   6 Apr 04 ... Smarter constant bidder spreads wealth evenly over the rest.
  25 Mar 04 ... Next version
   6 Aug 03 ... Use objects to have one class so can put them in one collection.
  29 Jul 03 ... Created.
*/

// universal PDF 
#include "coding.h"

#include "auction_base_types.h"
#include "debug.h"
#include "features.h"

#include <iostream>
#include <vector>



//   BiddingHistory   BiddingHistory   BiddingHistory   BiddingHistory   BiddingHistory   BiddingHistory   BiddingHistory
//
//   This is the information available about the current state of the auction that bidders
//   can use in forming their bids.
//

class BiddingHistory
{
 public:
  typedef SCALAR   Scalar;

 private:
  std::vector<Scalar>  const& mPayoffHistory;
  std::vector<Feature> const& mAcceptedFeatures;
  std::vector<Feature> const& mRejectedFeatures;

 public:
  BiddingHistory (std::vector<Scalar>  const& payoffs,
		  std::vector<Feature> const& accepted,
		  std::vector<Feature> const& rejected)
    : mPayoffHistory(payoffs), mAcceptedFeatures(accepted), mRejectedFeatures(rejected) { }
  
  std::vector<Scalar>  const&  payoff_history()    const { return mPayoffHistory; }
  std::vector<Feature> const&  rejected_features() const { return mRejectedFeatures; }
  std::vector<Feature> const&  accepted_features() const { return mAcceptedFeatures;}

  void print_to (std::ostream & os) const;
};


inline
std::ostream&
operator<< (std::ostream& os, BiddingHistory const& state)
{
  state.print_to(os);
  return os;
}


//   BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  
//
//      This is the 0/1 vector from JRSS paper, held separately within each expert
//

class BidHistory 
{
  std::vector<int> mResults;
  int mBidsSinceLastSuccess;
  int mNumSuccessfulBids;
  
public:
    BidHistory ()    : mResults(), mBidsSinceLastSuccess(0), mNumSuccessfulBids(0) { }
  
  std::vector<int> const&  results()                        const { return mResults; }
  std::pair<int,int>       bid_results_summary()            const { int n ((int)mResults.size());
                                                                    return std::make_pair(mNumSuccessfulBids, n-mNumSuccessfulBids); }
  int                      number_bids_since_last_success() const { return mBidsSinceLastSuccess; }

  void                     append_bid_outcome (bool success);
};


//   FiniteBidder    FiniteBidder    FiniteBidder    FiniteBidder    FiniteBidder    FiniteBidder    FiniteBidder   

template <class Stream>  // source must be able to tell it the number remaining
class FiniteBidder
{
  
public:
  
  std::string name() const { return "Finite bidder"; }
  
  SCALAR bid (bool, SCALAR alpha, Stream const& stream, BidHistory const&, BiddingHistory const&) const
  {
    const SCALAR maxbid = 0.25;
    int n (stream.number_remaining());
    if (n>0)
      { SCALAR b = alpha/(SCALAR)n;
      return (b < maxbid) ? b : maxbid;
    }
    else
      return 0.0;
  }
  
}; 

template <class Stream>
FiniteBidder<Stream>
make_finite_bidder(Stream& stream)
{
  return FiniteBidder<Stream>(stream);
}


////  FitBidder    FitBidder   FitBidder   FitBidder   FitBidder   FitBidder   FitBidder   FitBidder   FitBidder

/*  Places bid after any variable is added to the model. */

class FitBidder
{
 private:
  SCALAR      mBidAmount;
  std::string mSignature;
 public:
  
  FitBidder (SCALAR bid, std::string signature)    : mBidAmount(bid), mSignature(signature) {}
  
  std::string name() const { return "Fit Bidder [" + mSignature + "]"; }
  
  template <class Stream>
    SCALAR bid (bool lastBidAccepted, SCALAR, Stream const&, BidHistory const&, BiddingHistory const& state)
  {
    std::vector<SCALAR> payoffs (state.payoff_history());
    if(!payoffs.empty() && payoffs.back() > 0)                       // last variable was added
    { if (lastBidAccepted)                                           // dont bid this round if last added was calibration
	return 0;
      else
	return mBidAmount;
    }
    else return 0;                                                   // dont bid when last not entered into auction
  }
	
};



////  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder

template <class Stream>
class GeometricBidder
{
  const SCALAR mRate;
  
 public:
  
  GeometricBidder (SCALAR rate)
    : mRate(rate) {}
  
  std::string name() const { return "Geo Bidder"; }
  
  SCALAR bid (bool, SCALAR alpha, Stream const& stream, BidHistory const&, BiddingHistory const&) const
  {
    if (stream.number_remaining()>0)
      return alpha * mRate; 
    else
      return (SCALAR)0.0;
  }
  
};




////  UniversalBidder  UniversalBidder  UniversalBidder  UniversalBidder  UniversalBidder  UniversalBidder

template <class Stream>
class UniversalBidder
{
 
public:
  UniversalBidder () {}    
  
  std::string name() const { return "Univ Bidder"; }
  
  SCALAR bid (bool, SCALAR alpha, Stream const& stream, BidHistory const& history, BiddingHistory const&) const
  {
    if (stream.number_remaining()>0)
      return alpha * (SCALAR)slowUniversalPDF(1+history.number_bids_since_last_success());   // add 1; cannot call with zero
    else
      return (SCALAR)0.0;
  }

};

////  UniversalBoundedBidder  UniversalBoundedBidder  UniversalBoundedBidder  UniversalBoundedBidder 

// mixes universal with finite/bidder; keeps bid above a minimal threshold

namespace {
  SCALAR max(SCALAR a, SCALAR b) { return a>b ? a : b; }
}

template <class Stream>
class UniversalBoundedBidder
{
 
public:
  UniversalBoundedBidder () {}    
  
  std::string name() const { return "Univ Bd Bidder"; }
  
  SCALAR bid (bool, SCALAR alpha, Stream const& stream, BidHistory const& history, BiddingHistory const&) const
  {
    const SCALAR maxbid = 0.25;
    int n (stream.number_remaining());
    if (n>0)
    { SCALAR fixedBid= (SCALAR)1.0/(SCALAR)n;
      SCALAR bid = alpha * max( fixedBid, (SCALAR)universalPDF(1+history.number_bids_since_last_success()) );
      return (bid < maxbid) ? bid : maxbid;
    }
    else
      return 0.0;
  }

};

#endif
