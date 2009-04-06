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

#include <iostream>
#include <vector>

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

template <class Stream>  // source must be able to tell it the number remaining
class FiniteBidder
{
  
public:
  
  std::string name() const { return "Finite bidder"; }
  
  double bid (double alpha, Stream const& stream, BidHistory const&) const
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


////  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder  GeometricBidder

template <class Stream>
class GeometricBidder
{
  const double mRate;
  
 public:
  
  GeometricBidder (double rate)
    : mRate(rate) {}
  
  std::string name() const { return "Geometric bidder"; }
  
  double bid (double alpha, Stream const& stream, BidHistory const&) const
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
  
  double bid (double alpha, Stream const& stream, BidHistory const& history) const
  {
    if (stream.number_remaining()>0)
      return alpha * universalPDF(history.number_bids_since_last_sucess());
    else
      return 0.0;
  }

};


#endif
