/* $Id: experts.h,v 1.9 2008/01/28 23:21:47 bob Exp $
 *  expert.h
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *

Experts combine the features of a bidder with those of a stream.  The expert
serves as a host class, enforcing certain procedural rules.  It's up to the user
to make sure that the bidder and stream are compatible.

A bidder assigns a bid to the output of a stream of candidate features that can be 
used as explanatory variables in the auction.  Each stream includes a typedef of
its result type.  Some will be features, some will be collections of features.  The
expert never holds the features explicitly.  It leaves them in the stream until a bid
is accepted and then it pops the feature off of the stream.

 */
#ifndef _EXPERTS_H_
#define _EXPERTS_H_


#include "feature_streams.h"
#include "bidders.h"

// need ABC since will need to have a collection of experts
class ExpertABC
{
  
protected:
  double      mAlpha;
  double      mCurrentBid;
  BidHistory  mBidHistory;
  
public:
  virtual ~ExpertABC () { }
  
 ExpertABC(double alpha)
    : mAlpha(alpha), mCurrentBid(0.0), mBidHistory() { }
  
  double                alpha()        const { return mAlpha; }
  std::pair<int,int>    performance()  const { return mBidHistory.bid_results_summary(); }
  double                place_bid ()         { if ((0.0 == mCurrentBid) && (mAlpha>0.0)) mCurrentBid = get_new_bid(); return mCurrentBid; }
  void                  bid_accepted()       { mCurrentBid = 0.0; }
  void                  bid_declined()       { /* do nothing at this point */ }
  void                  payoff (double w);     /* negative means rejected */
  
  virtual std::string             name()         const = 0;
  virtual Features::FeatureVector features()           = 0;
  virtual double                  get_new_bid()        = 0;
};

  
template <class Bidder, class Stream> 
class Expert : public ExpertABC
{

private:
  Bidder      mBidder;
  Stream      mStream;
  
public:
  virtual ~Expert () { };
  
  Expert (double alpha, Bidder b, Stream s)
    : ExpertABC(alpha), mBidder(b), mStream(s) { }
  
  Bidder              bidder()       const { return mBidder; }
  Stream              stream()       const { return mStream; }
  std::string         name()         const { return mBidder.name() + "/" + mStream.name(); }
  
  Features::FeatureVector       features()           { return mStream.pop(); }   // pop must return feature *vector*
  double                        get_new_bid ();

protected:
  double              max_bid ()     const { return mAlpha/(1.0-mAlpha); }
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
make_expert(double alpha, Bidder b, Stream s)
{
  return new Expert<Bidder, Stream>(alpha, b, s); 
}


// .Template is here since so little

template<class Bidder, class Stream>
double 
Expert<Bidder, Stream>::get_new_bid () 
{ 
  std::cout << "XPRT: " << name() << " getting new bid, mAlpha = " << mAlpha 
  << "; stream has " << mStream.number_remaining() << " elements left.\n";
  if (mStream.has_feature())
  { double b (mBidder.bid(mAlpha, mBidHistory)); 
    double m (max_bid()); 
    return (b<m) ? b:m;
  } else
    return 0.0;
}


#endif
