/* 
 *  expert.h
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *

Experts combine the features of a bidder with those of a stream.  The
expert serves as a host class, enforcing certain procedural rules.
It's up to the user to make sure that the bidder and stream are
compatible.

The auction only 'sees' the experts and does not interact with the
bidder or stream of features directly.

A bidder assigns a bid to the output of a stream of candidate features
that can be used as explanatory variables in the auction.  Each stream
includes a typedef of its result type.  Some will be features, some
will be collections of features.  The expert never holds the features
explicitly.  It leaves them in the stream until a bid is accepted and
then it pops the feature off of the stream.

 */
#ifndef _EXPERTS_H_
#define _EXPERTS_H_


#include <assert.h>
#include <deque>

#include "debug.h"

#include "feature_streams.h"
#include "bidders.h"


// need ABC since will need to have a collection of experts
class ExpertABC
{
  typedef     std::vector<Feature>  FeatureVector;
  
protected:
  int         mPriority;             //  higher jumps to top of auction
  double      mAlpha;
  double      mCurrentBid;
  bool        mLastBidAccepted;
  BidHistory  mBidHistory;
  
public:
  virtual ~ExpertABC () { }
  
 ExpertABC(int priority, double alpha)
   : mPriority(priority), mAlpha(alpha), mCurrentBid(0.0), mLastBidAccepted(false), mBidHistory() {}

  int                    priority()     const { return mPriority; }
  double                 alpha()        const { return mAlpha; }
  double                 current_bid()  const { return mCurrentBid; }
  std::pair<int,int>     performance()  const { return mBidHistory.bid_results_summary(); }

  void                   payoff (double w);     // positive -> added, negative -> rejected, zero -> predictor conditionally singular 
  
  virtual double         place_bid (std::deque<double> const& auctionPayoffHistory, FeatureVector const& used, FeatureVector const& skipped) = 0; 
  virtual std::string    name()               const = 0;
  virtual std::string    feature_name()       const = 0;
  virtual FeatureVector  feature_vector()     = 0;
  virtual void           bid_accepted()       { mLastBidAccepted = true; }
  virtual void           bid_declined()       { mLastBidAccepted = false; }
  

 protected:
  double                 max_bid      ()     const { return  (mAlpha>0.0) ? mAlpha/(1.0+mAlpha) : 0.0; }  // bid < 1.0
  virtual bool           has_feature  (FeatureVector const& used, FeatureVector const& skipped) = 0;
};



template <class Bidder, class Stream> 
class Expert : public ExpertABC
{

private:
  Bidder      mBidder;
  Stream      mStream;
  
public:
  virtual ~Expert () { };
  
  Expert (int priority, double alpha, Bidder b, Stream s)
    : ExpertABC(priority, alpha), mBidder(b), mStream(s) { }
  
  Bidder const&    bidder()       const { return mBidder; }
  Stream const&    stream()       const { return mStream; }
  std::string      name()         const { return mBidder.name() + "/" + mStream.name(); } // stream must have a name
  
  double           place_bid (std::deque<double> const& auctionPayoffHistory, FeatureVector const& used, FeatureVector const& skipped);
  std::string      feature_name()   const     { return mStream.feature_name(); }       
  FeatureVector    feature_vector()           { return mStream.pop(); }                // stream pop must return feature *vector*

 protected:
  bool             has_feature(FeatureVector const& used, FeatureVector const& skipped) { return mStream.has_feature(used, skipped); }
};


inline
std::ostream&
operator<< (std::ostream& os, ExpertABC const* expert)
{
  os << expert->name() << " with alpha=" << expert->alpha() << " ";
  return os;
}

inline
std::ostream&
operator<< (std::ostream& os, std::vector<ExpertABC*> const& experts)
{
  for (int i=0; i<(int)experts.size(); ++i)
    os << "      " << experts[i] << std::endl;
  return os;
}

template<class Bidder, class Stream>
inline
Expert<Bidder,Stream>*
make_expert(int priority, double alpha, Bidder b, Stream s)
{
  return new Expert<Bidder, Stream>(priority, alpha, b, s); 
}


////     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE 
// .Template is here since so little

template<class Bidder, class Stream>
double
  Expert<Bidder,Stream>::place_bid (std::deque<double> const& auctionPayoffHistory, FeatureVector const& used, FeatureVector const& skipped)
{
  //  debugging::debug(0) << "XPRT: " << name() << " gets bid: mAlpha=" << mAlpha << std::endl;
  if ( (mAlpha>0.0) && (has_feature(used, skipped)) )
  { double b (mBidder.bid(mLastBidAccepted, mAlpha, mStream, mBidHistory, auctionPayoffHistory)); 
    double m (max_bid()); 
    mCurrentBid = (b<m) ? b:m;
  }    
  else
    mCurrentBid = 0.0;
  return mCurrentBid;
}


#endif
