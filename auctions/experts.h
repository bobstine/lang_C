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

  3 Mar 2010 ... Move to having a wrapper class with a ref counted pointer.
  
  
*/

#ifndef _EXPERTS_H_
#define _EXPERTS_H_


#include <assert.h>
#include <map>

#include "debug.h"

#include "feature_streams.h"
#include "bidders.h"


// need ABC since have a collection of experts due to templating

class Expert;

class ExpertABC
{
  friend class Expert;

  typedef     std::vector<Feature>               FeatureVector;
  typedef     std::map<std::string, std::string> Attributes;
  
protected:
  int         mRefCount;
  int         mPriority;             //  higher jumps to top of auction
  double      mAlpha;
  double      mCurrentBid;
  bool        mLastBidAccepted;
  BidHistory  mBidHistory;
  
public:
  virtual ~ExpertABC () { }
  
 ExpertABC()
   : mRefCount(1),
     mPriority(0), mAlpha(0), mCurrentBid(0.0), mLastBidAccepted(false), mBidHistory() {}

 ExpertABC(int priority, double alpha)
   : mRefCount(1),
     mPriority(priority), mAlpha(alpha), mCurrentBid(0.0), mLastBidAccepted(false), mBidHistory() {}

  int                    priority()                 const { return mPriority; }
  double                 alpha()                    const { return mAlpha; }
  double                 increment_alpha(double a)        { mAlpha += a; return mAlpha; }
  double                 current_bid()              const { return mCurrentBid; }
  std::pair<int,int>     performance()              const { return mBidHistory.bid_results_summary(); }

  void                   payoff (double w);     // positive -> added, negative -> rejected, zero -> predictor conditionally singular 
  
  virtual double         place_bid (AuctionState const& state) = 0; 
  virtual std::string    name()                     const = 0;
  virtual std::string    feature_name()             const = 0;
  virtual FeatureVector  feature_vector()                 = 0;
  virtual void           bid_accepted()                       { mLastBidAccepted = true; }
  virtual void           bid_declined()                       { mLastBidAccepted = false; }

  virtual void           print_to(std::ostream& os) const     { os << "Expert[" << mRefCount <<"]: " << name() << " with alpha " << mAlpha; }

 protected:
  double                 max_bid      ()     const { return  (mAlpha>0.0) ? mAlpha/(1.0+mAlpha) : 0.0; }  // bid < 1.0
  virtual bool           has_feature  (AuctionState const& state) = 0;
};


// Stream expert combines bidder with a stream source for features

template <class Bidder, class Stream> 
class StreamExpert : public ExpertABC
{

private:
  Bidder      mBidder;
  Stream      mStream;
  
public:
  virtual ~StreamExpert () { };
  
  StreamExpert (int priority, double alpha, Bidder b, Stream s)
    : ExpertABC(priority, alpha), mBidder(b), mStream(s) { }
  
  Bidder const&    bidder()       const { return mBidder; }
  Stream const&    stream()       const { return mStream; }
  std::string      name()         const { return mBidder.name() + "/" + mStream.name(); } // stream must have a name
  
  double           place_bid (AuctionState const& state);
  std::string      feature_name()                const     { return mStream.feature_name(); }       
  FeatureVector    feature_vector()                        { return mStream.pop(); }      // stream pop must return feature *vector*

 protected:
  bool             has_feature(AuctionState const& state) { return mStream.has_feature(state.accepted_features(), state.rejected_features()); }
};



////     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE     TEMPLATE 


template<class Bidder, class Stream>
double
  StreamExpert<Bidder,Stream>::place_bid (AuctionState const& state)
{
  //  debugging::debug(0) << "XPRT: " << name() << " gets bid: mAlpha=" << mAlpha << std::endl;
  if ( (mAlpha>0.0) && (has_feature(state))  )
  { double b (mBidder.bid(mLastBidAccepted, mAlpha, mStream, mBidHistory, state)); 
    double m (max_bid()); 
    mCurrentBid = (b<m) ? b:m;
  }    
  else
    mCurrentBid = 0.0;
  return mCurrentBid;
}


////  Envelope    Envelope    Envelope    Envelope    Envelope    Envelope    Envelope    Envelope    Envelope    

class Expert
{
 private:
  ExpertABC *mpExpert;

 public:
  ~Expert() { if(mpExpert && (--mpExpert->mRefCount <= 0)) delete mpExpert; }
  
  // empty
    Expert() : mpExpert(NULL) {  }
  
  // feature-specific expert
  template <class Bidder>
    Expert(Bidder const& b, Feature const& f);
  
  // stream expert
  template <class Bidder, class Stream>
    Expert(int priority, double alpha, Bidder const& b, Stream const& s)  { mpExpert = new StreamExpert<Bidder,Stream> (priority, alpha, b, s); }

  // copy
  Expert(Expert const& e)    : mpExpert(e.mpExpert)   { ++e.mpExpert->mRefCount;  }  

  // assign
  Expert&    operator=(Expert const& e);

  ExpertABC* operator->()                 const       { return mpExpert; }

  bool       operator==(Expert const& e)  const       { return mpExpert == e.mpExpert; }
  bool       operator!=(Expert const& e)  const       { return mpExpert != e.mpExpert; }
  bool       empty()                      const       { return mpExpert == NULL; }       
};

inline
std::ostream&
operator<< (std::ostream& os, Expert const& expert)
{
  expert->print_to(os);
  return os;
}

inline
std::ostream&
operator<< (std::ostream& os, std::vector<Expert> const& experts)
{
  for (int i=0; i<(int)experts.size(); ++i)
    os << "      " << experts[i] << std::endl;
  return os;
}

#endif
