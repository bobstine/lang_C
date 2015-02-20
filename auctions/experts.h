// -*- mode: c++; fill-column: 100; -*-
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

#ifndef _EXPERTS_H
#define _EXPERTS_H_

#include "debug.h"

#include "feature_streams.h"
#include "bidders.h"

#include <map>


// need ABC since have a collection of experts due to templating

enum    ExpertRole       { source, scavenger, beam, calibrate, spawn, custom }; 

class Expert;

class ExpertABC
{
  friend class Expert;
  
  typedef     std::pair<std::string, FeatureABC::Iterator>   NamedIterator;
  typedef     std::vector<NamedIterator>                     NamedIteratorVector;
  typedef     std::map<std::string, std::string>             Attributes;
  
protected:
  const std::string mName;
  const ExpertRole  mRole;
  const bool        mPurgable;          // from auction when finished
  const int         mSkip;              // # leading context data cases to skip past
  int               mRefCount;
  double            mAlpha;
  double            mCurrentBid;
  bool              mLastBidAccepted;
  BidHistory        mBidHistory;
  
public:
  virtual ~ExpertABC () { }
  
  ExpertABC()
    : mName("none"), mRole(source), mPurgable(true), mSkip(0), mRefCount(1),  mAlpha(0), mCurrentBid(0.0), mLastBidAccepted(false), mBidHistory() {}
  
  ExpertABC(std::string name, ExpertRole role, bool purgable, int skip, double alpha)
    : mName(name), mRole(role), mPurgable(purgable), mSkip(skip), mRefCount(1), mAlpha(alpha), mCurrentBid(0.0), mLastBidAccepted(false), mBidHistory() {}
  
  std::string            name()                             const { return mName; }
  std::string            name(unsigned int maxlen)          const { if(mName.size()<maxlen) return mName; else return mName.substr(0,maxlen); } 
  int                    priority()                         const { if (mRole == calibrate) return 1; else return 0; }
  ExpertRole             role()                             const { return mRole; }
  bool                   is_purgable()                      const { return mPurgable; }
  int                    skip()                             const { return mSkip; }
  double                 alpha()                            const { return mAlpha; }
  double                 increment_alpha(double a)                { mAlpha += a; return mAlpha; }
  double                 current_bid()                      const { return mCurrentBid; }
  std::pair<int,int>     performance()                      const { return mBidHistory.bid_results_summary(); }
  bool                   finished()                               { return (mAlpha < 1.0e-10) || (0 == number_of_remaining_features());}
  
  void                   payoff (double w);     // positive -> added, negative -> rejected, zero -> predictor conditionally singular 
  
  virtual std::string    description()                      const = 0;  
  virtual double         place_bid(BiddingHistory const& state)  = 0; 
  virtual std::string    first_feature_name()               const = 0;   
  virtual FeatureVector  feature_vector()                         = 0;
  
  NamedIteratorVector    convert_to_model_iterators(FeatureVector const& fv) const;    // convert to vector of (name, begin) pairs

  virtual void           model_adds_current_variable()            { }
  virtual void           bid_accepted()                           { mLastBidAccepted = true; }
  virtual void           bid_declined()                           { mLastBidAccepted = false; }

  virtual void           print_to(std::ostream& os) const;

  double                 max_bid()           const                { return  (mAlpha>0.0) ? mAlpha/(1.0+mAlpha) : 0.0; }  // bid < 1.0
  virtual bool           has_feature()                  = 0;
  virtual int            number_of_remaining_features() = 0;
  
private:
  std::string            role_string() const;
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
  
  StreamExpert (std::string name, ExpertRole role, bool purgable, int skip, double alpha, Bidder b, Stream s)
    : ExpertABC(name, role, purgable, skip, alpha), mBidder(b), mStream(s) { }

  std::string         description()  const { return mBidder.name()+"/"+mStream.name(); }
  Bidder const&       bidder()       const { return mBidder; }
  Stream const&       stream()       const { return mStream; }

  double              place_bid (BiddingHistory const& state);
  void                model_adds_current_variable()           { /* placeholder */ }

  std::string         first_feature_name()          const     { return mStream.first_feature_name(); }
  FeatureVector       feature_vector()                        { return mStream.pop_feature_vector(); }      // stream pop must return feature *vector*

  virtual void        print_to(std::ostream& os)    const;

  bool                has_feature()                           { return mStream.has_feature_vector(); }
  int                 number_of_remaining_features()          { return mStream.number_remaining(); }
};



////  Envelope    Envelope    Envelope    Envelope    Envelope    Envelope    Envelope    Envelope    Envelope    

class Expert
{
 private:
  ExpertABC *mpExpert;

 public:
  ~Expert() { if(mpExpert && (--mpExpert->mRefCount <= 0)) delete mpExpert; }
  
  // empty
  Expert() : mpExpert(NULL) {  }
  
  // stream expert
  template <class Bidder, class Stream>
  Expert(std::string name, ExpertRole role, bool purgable, int skip, double alpha, Bidder const& b, Stream const& s)
  { mpExpert = new StreamExpert<Bidder,Stream> (name, role, purgable, skip, alpha, b, s); }

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
  os << "Experts ..." << std::endl;
  for (int i=0; i<(int)experts.size(); ++i)
    os << "\t" << experts[i] << std::endl;
  return os;
}

#endif
