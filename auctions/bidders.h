// $Id: bidders.h,v 3.6 2008/01/21 23:27:26 bob Exp $

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

  19 Jan 08 ... Revised to simplified policy class, without an ABC
  16 Jan 08 ... Add universal bidder; make payoff conform to JRSS results. No pvalue.
   6 Apr 04 ... Smarter constant bidder spreads wealth evenly over the rest.
  25 Mar 04 ... Next version
   6 Aug 03 ... Use objects to have one class so can put them in one collection.
  29 Jul 03 ... Created.
*/

// universal PDF 
#include "coding.h"
#include "debug.h"

#include <iostream>
#include <vector>
#include <deque>


//   BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  BidHistory  
/*
 This is the 0/1 vector from JRSS paper
 */

class BidHistory 
{
  std::vector<int> mResults;
  int mBidsSinceLastSuccess;
  int mNumSuccessfulBids;
  
public:
    BidHistory ()
    : mResults(), mBidsSinceLastSuccess(0), mNumSuccessfulBids(0) { }
  
  std::vector<int> const&  results()                       const { return mResults; }
  std::pair<int,int>       bid_results_summary()           const { int n (mResults.size()); return std::make_pair(mNumSuccessfulBids, n-mNumSuccessfulBids); }
  int                      number_bids_since_last_sucess() const { return mBidsSinceLastSuccess; }

  void                     append_bid_outcome (bool success);

};
  
//   FiniteBidder    FiniteBidder    FiniteBidder    FiniteBidder    FiniteBidder    FiniteBidder    FiniteBidder   

template <class Stream>  // source must be able to tell it the number remaining
class FiniteBidder
{
  
public:
  
  std::string name() const { return "Finite bidder"; }
  
  double bid (double alpha, Stream const& stream, BidHistory const&, std::deque<double> const&) const
  {
    int n (stream.number_remaining());
    if (n>0)
      return alpha/n;
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


class FitBidder
{
 private:
  int mDelayInterval;  // set randomly?
  int mCountDown;
  
 public:
  
  FitBidder (int delay)    : mDelayInterval(delay),mCountDown(delay) {}

  
  std::string name() const { return "Fit bidder"; }
  
  template <class Stream>
    double bid (double, Stream const&, BidHistory const&, std::deque<double> const& payoffs)
  {
    debugging::debug(3) << "BIDR: fit bidder with countdown " << mCountDown << "/" << mDelayInterval << std::endl;
    if(!payoffs.empty() && payoffs.back() > 0) // last variable successful
    {  --mCountDown;
       if(0 == mCountDown)
       { mCountDown = mDelayInterval;
	 return .25;
       }
       else
	 return 0.05;
    }
    else return 0.0;  // dont bid
  }
  
};



////  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder

template <class Stream>
class GeometricBidder
{
  const double mRate;
  
 public:
  
  GeometricBidder (double rate)
    : mRate(rate) {}
  
  std::string name() const { return "Geometric bidder"; }
  
  double bid (double alpha, Stream const& stream, BidHistory const&, std::deque<double> const&) const
  {
    if (stream.number_remaining()>0)
      return alpha * mRate; 
    else
      return 0.0;
  }
  
};




////  UniversalBidder  UniversalBidder  UniversalBidder  UniversalBidder  UniversalBidder  UniversalBidder

template <class Stream>
class UniversalBidder
{
 
public:
  UniversalBidder () {}    
  
  std::string name() const { return "Universal bidder"; }
  
  double bid (double alpha, Stream const& stream, BidHistory const& history, std::deque<double> const&) const
  {
    if (stream.number_remaining()>0)
      return alpha * universalPDF(history.number_bids_since_last_sucess());
    else
      return 0.0;
  }

};



////  UniversalBoundedBidder  UniversalBoundedBidder  UniversalBoundedBidder  UniversalBoundedBidder 

// mixes universal with finite/bidder minimum bid
// keeps bid above a minimal threshold determined

namespace {
  double max(double a, double b) { return a>b ? a : b; }
}

template <class Stream>
class UniversalBoundedBidder
{
 
public:
  UniversalBoundedBidder () {}    
  
  std::string name() const { return "Universal bounded bidder"; }
  
  double bid (double alpha, Stream const& stream, BidHistory const& history, std::deque<double> const&) const
  {
    int n (stream.number_remaining());
    if (n>0)
    { double fixedBid=1.0/(double)n;
      return alpha * max( fixedBid,
			  universalPDF(history.number_bids_since_last_sucess())
			  );
    }
    else
      return 0.0;
  }

};


#endif
