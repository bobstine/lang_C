// -*- mode: c++; fill-column: 80; -*-
#ifndef _FEATURE_STREAMS_H_
#define _FEATURE_STREAMS_H_

/*
 *  feature_stream.h
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *
 
 Feature streams implement an abstract protocol and deliver upon request the 'next'
 feature.  The regulator template wrapper-class enforce the common protocol for
 checking whether the stream is (a) empty and (b) has a non-trivial feature (eg,
 one that is not a constant).  Template objects in the feature stream object
 provide the data source that the stream uses to build the supplied features.
 These might be a model, a list of variables, a file, and so forth.

 Feature streams (along with bidders) are held in experts that participate in the
 auction. Experts call 
        has_feature()
 before placing any bids.  The regulated stream coordinates a sequence of calls
 to more primitive methods. The bidder at higher level may ask for the number
 remaining (number_remaining).  If the stream has a feature, then a winning
 expert will call
        pop()
 which return a feature vector.  pop() must (a) run very fast and (b) return a
 vector of features.  Heavy lifting required in order to be able to pop() must
 be done in the method build_next_feature().  In order to support some styles of
 bidding, the stream will need to implement
        number_remaining().
 
 Stream properties...
    Streams should be *lightweight*.  They will be copied heavily in the auction.
    Basically act as a stack/queue, a type of iterator really.  The stream itself
    does *not* hold data, just the rules to build new variables.

    Indexing. Some streams use indices to keep track of their position in a list
    of base features.  These indices should refer to the current head feature or
    the most recent head.

 Types of streams ...
    Finite           chooses variables from a fixed set of columns as a queue
    Fit              builds features depending on state of model (such as just added var)
    Interaction      interactions among features from a source of fixed size
    Feature-product  interactions between a feature and a fixed set (counts down, as vars in model)
    Cross-product    interactions between fixed set of features and a set of increasing size
    Lag              lags of a given feature
    Polynomial       bundle of several powers at once
    Subspace         several variables as a bundle


  
*/

#include "features.h"
#ifdef THREADED
#include "threaded.h"
#endif

// polynomial
#include "function_utils.h"
#include "debug.h"

// operator
#include <functional>
// for finite streams
#include <deque>     
#include <queue>
#include <iostream>
#include <sstream>

//  Regulated Stream    Regulated Stream    Regulated Stream    Regulated Stream    Regulated Stream 

/*
  A regulated stream injects the method 'has_feature' into a stream object.
  That stream object must implement the three functions empty,
  current_feature_is_ok and build_next_feature.  The stream will then have the
  chance participate in the auction and submit a feature to the model via 'pop'.

  The regulated stream provides a 'consistent interface' for all streams without needing
  an abstract base class.  (Works since never have a collection of streams; streams are
  hidden in experts, for which we *do* have an abstract base class.)
*/
  

#ifndef THREADED

template<class Stream>
class RegulatedStream: public Stream
{
public:
  RegulatedStream(Stream const& s): Stream(s) { }
  
  bool has_feature (FeatureVector const& accepted, FeatureVector const& rejected)
  {
    if (Stream::has_feature_ready())
      return true;
    else
      if (Stream::can_build_more_features())             // should be able to get one, so
	Stream::build_next_feature(accepted, rejected);  // advance position using current state
      else
	debugging::debug("RGST",3) << "regulated stream '" << Stream::name() <<"' cannot build more features.\n";
    return Stream::has_feature_ready();                  // may not have been able to build one after all
  }
};

#else

template<class Stream>
class RegulatedStream: public Stream
{  
private:
  Threaded< RegulatedStream<Stream> > m_thread;

public:
  RegulatedStream(Stream const& s)  : Stream(s), m_thread() { }
  
  bool has_feature (FeatureVector const& used, FeatureVector const& skipped)
  {
    while(m_thread.done() && !Stream::empty())
    {
      if (Stream::current_feature_is_okay(used,skipped))  // chance to check feature before bidding in context of model or auction
	return true;
      else
	m_thread(this);
    }
    debugging::debug("RGST",3) << "'has_feature' returns false for regulated stream '" << Stream::name() <<"'.\n";
    return false;
  }

  void
  operator()()
  {
    Stream::pop();
  }
};
#endif


//  BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream

class BaseStream
{
private:
  std::string   mName;
  FeatureVector mHead;

public:
  BaseStream (std::string name) : mName(name), mHead() { }
  
  std::string    name()                            const { return mName; }
  std::string    feature_name()                    const { if (mHead.empty()) return std::string(""); else return mHead[0]->name(); }
  void           print_to(std::ostream& os)        const { os <<  mName << " @ " << feature_name() << std::endl;  }
  bool           has_feature_ready()               const { return !mHead.empty(); }

  void           set_head(Feature const& f)              { mHead.clear(); mHead.push_back(f); }
  void           set_head(FeatureVector const& fv)       { mHead = fv; }
  
  FeatureVector  pop()                                   { assert (!mHead.empty()); FeatureVector z (mHead); mHead.clear(); return z; }

  // utilities used in checking features
  bool indicators_from_same_parent  (Feature const& f1, Feature const& f2) const;
  bool found_name_in_feature_vector (std::string const& name, FeatureVector const& vec, std::string const& vecName) const;
};


//  FiniteStream     FiniteStream     FiniteStream     FiniteStream     FiniteStream     FiniteStream     FiniteStream     FiniteStream     

class FiniteStream:  public BaseStream
{
  std::deque<Feature>  mFeatures;  
  
public:
  
  FiniteStream(std::string const& name, FeatureVector const& features)
    : BaseStream("FiniteStream:" + name) { insert_features(features); }
  
  int   number_remaining()           const { return mFeatures.size(); }
  bool  can_build_more_features()    const { return number_remaining()>0; }

  void  build_next_feature(FeatureVector const&, FeatureVector const&);

  void  print_features_to (std::ostream& os) const;

private:
  void  insert_features (FeatureVector const& features);
};


inline
RegulatedStream< FiniteStream >
make_finite_stream (std::string const& name, FeatureVector const& features)
{
  return RegulatedStream< FiniteStream >(FiniteStream(name, features));
}



//  LagStream     LagStream     LagStream     LagStream     LagStream     LagStream     LagStream     LagStream

class LagStream: public BaseStream
{
  const Feature      mFeature;       // construct lags of this feature
  const int          mMaxLag;
  const int          mBlockSize;     // blocking factor used if longitudinal
  int                mLag;
  int                mCyclesLeft;    // cycle through the lags
  
public:
  
  LagStream(std::string const& name, Feature const& f, int maxLag, int blockSize, int cycles)
    :  BaseStream("LagStream:" + name), mFeature(f), mMaxLag(maxLag), mBlockSize(blockSize), mLag(0), mCyclesLeft(cycles-1) {  }
  
  int   number_remaining()                 const   { return  mMaxLag - mLag + mCyclesLeft * mMaxLag; }
  bool  can_build_more_features()          const   { return  (!mFeature->is_constant()) && (number_remaining() > 0); } 

  void  build_next_feature (FeatureVector const& accepted, FeatureVector const&);
};


inline
RegulatedStream< LagStream >
make_lag_stream (std::string const& name, Feature const& f, int maxLag, int blockSize,  int numberCycles)
{
  return RegulatedStream< LagStream >(LagStream(name, f, maxLag, blockSize, numberCycles));
}



//  NeighborhoodStream     NeighborhoodStream     NeighborhoodStream     NeighborhoodStream     NeighborhoodStream     NeighborhoodStream

template<class Source>
class NeighborhoodStream: public BaseStream
{
  Source const&      mSource;
  const std::string  mSignature;    // look for this in name of variable before using
  IntegerColumn      mIndexColumn;
  int                mPos;
  
public:

  NeighborhoodStream(std::string const& name, Source const& src, std::string sig, IntegerColumn const& indices)
    :  BaseStream("NhoodStream:" + name), mSource(src), mSignature(sig), mIndexColumn(indices), mPos(0) {  }
  
  int   number_remaining()            const   { return (mSource.size()-mPos); }
  bool  can_build_more_features()     const   { return number_remaining() > 0; }

  void  build_next_feature(FeatureVector const& accepted, FeatureVector const& rejected);
};


template <class Source>
RegulatedStream< NeighborhoodStream<Source> >
make_neighborhood_stream (std::string const& name, Source const& src, std::string signature, IntegerColumn const& col)
{
  return RegulatedStream< NeighborhoodStream<Source> >(NeighborhoodStream<Source>(name, src, signature, col));
}



//  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream

template<class Model>
class FitStream
{
  mutable int            mLastQ;           // used to detect change in model size
  int                    mPower;
  Model          const&  mModel;
  std::string            mSignature;       // prefix for variable names so that it can recognize them
  Column                 mFit;             // holds fit values from model
  int                    mSkip;            // context rows to pad when returning fit
  FeatureVector          mHead;
  
public:
  
  FitStream(Model const& model, int power, std::string s, int skip)
    : mLastQ(0), mPower(power), mModel(model), mSignature(s), mFit(), mSkip(skip), mHead() {  }
  
  std::string     name()                       const { return mSignature; }
  bool              has_feature_ready()          const { return !mHead.empty(); }
  std::string     feature_name()               const { if (mHead.size() == 0) return std::string(""); else return mHead[0]->name(); }
  FeatureVector   pop()                              { FeatureVector z (mHead); mHead.clear(); return z; }

  void            print_to(std::ostream& os)   const { os << "FitStream " << name(); if(empty()) os<<" is empty."; else os<<"(skip="<<mSkip<<")"; }
  
protected:                                 // expert calls these methods following regulated stream protocol, allowing to grab fit
  bool            empty()                      const { return (mHead.size() == 0) && (mLastQ == mModel.q()); }
  void            build_next_feature();
  bool            current_feature_is_okay(FeatureVector const& used, FeatureVector const&);
};


template <class Model>
RegulatedStream< FitStream<Model> >
make_fit_stream (Model const& m, int powers, std::string signature, int skip)
{
  return RegulatedStream< FitStream<Model> >(FitStream<Model>(m,powers,signature,skip));
}


//  InteractionStream  InteractionStream  InteractionStream  InteractionStream  InteractionStream  InteractionStream  

template<class Source>
class InteractionStream
{
private:
  bool            mDiag;
  std::string     mName;
  Source const&   mSource;                     
  unsigned        mPos1, mPos2;
  FeatureVector   mHead;
  
public:
  
  InteractionStream(std::string name, Source const& src, bool useSquares)
    : mDiag(useSquares), mName(name), mSource(src), mPos1(0), mPos2(0), mHead() { set_initial_position(); }
  
  std::string     name()                       const { return mName; }
    bool              has_feature_ready()          const { return !mHead.empty(); }
  std::string     feature_name()               const { if(mHead.size()==0) return std::string(""); else return mHead[0]->name(); }
  int             number_remaining()           const;
  FeatureVector   pop()                              { FeatureVector z (mHead); mHead.clear(); return z; }
  
  void            print_to(std::ostream& os)   const { os << mName; if(empty()) os<<" is empty."; else os << " @ " << mPos1 << " x " << mPos2 << " "; }
   
protected:
  bool  empty ()                               const { return (mHead.size() == 0) && (number_remaining() == 0); }
  void  build_next_feature();
  bool  current_feature_is_okay    (FeatureVector const& used, FeatureVector const& skipped)   const;

private:
  void  set_initial_position();
  void  increment_positions();
  void  inc_counters();
};


template <class Source>
RegulatedStream< InteractionStream<Source> >
make_interaction_stream (std::string const& name, Source const& s, bool useSquares)
{
  return RegulatedStream< InteractionStream<Source> >(InteractionStream<Source>(name, s, useSquares));
}



//  FeatureProductStream    FeatureProductStream    FeatureProductStream    FeatureProductStream    FeatureProductStream    

class BidOrder
{
public:
  bool operator()(Feature const& a, Feature const& b) const { return (a->entry_bid() < b->entry_bid()); }
};

class FeatureProductStream 
{
  std::string    mName;
  Feature        mFeature;
  std::priority_queue<Feature, FeatureVector, BidOrder> mQueue;             // Make a priority queue out of a fixed source list of features
  FeatureVector  mHead;
  
public:
  FeatureProductStream(std::string name, Feature f, FeatureVector const& src)
    : mName(""), mFeature(f), mHead()  { mName = name + ":" + f->name() + " x Source"; initialize_queue(src);  }
  
  std::string     name()                       const { return mName; }  
  bool              has_feature_ready()          const { return !mHead.empty(); }
  std::string     feature_name()               const { if(mHead.size()==0) return std::string(""); else return mHead[0]->name(); }
  int             number_remaining()           const { return mQueue.size(); }
  FeatureVector   pop()                              { FeatureVector z (mHead); mHead.clear(); return z; }

  void            print_to(std::ostream& os)   const { os << "FPST: " << name(); if(empty()) os<<" is empty."; else os << " # " << mQueue.size() << " "; }
  
protected:
  bool            empty()                      const { return mQueue.empty(); }
  bool            current_feature_is_okay(FeatureVector const& used, FeatureVector const& skipped);  
  void            build_next_feature();

private:
  void            initialize_queue(FeatureVector const& s);

};


template <class Source>
RegulatedStream< FeatureProductStream >
make_feature_product_stream (std::string name, Feature f, Source const& Src)
{
  return RegulatedStream< FeatureProductStream >(FeatureProductStream(name, f, Src));
}



//  CrossProductStream    CrossProductStream    CrossProductStream    CrossProductStream

/*  Allows combination of two dynamically growing sources. You *must* guarantee
    that the sources remain "alive" for the duration of the application.

    Suppose the fast source has 4 elements.  Then

    Position vector {4,2,0} indicates that
            var 0 of the slow source has been crossed with 0,1,2,3 of fast (done with var 0 for now)
	    var 1                    has been crossed with 0,1     of fast, next with third in fast
	    var 2                    has not been crossed with any
*/
  

template<class Source1, class Source2>
class CrossProductStream 
{
  std::string      mName;
  Source1 const&   mSlowSource;            // grows slowly
  Source2 const&   mFastSource;            // grows faster
  mutable int              mSlowPos;
  mutable std::vector<int> mPos;           // one indexing element for each feature in the slow source
  FeatureVector    mHead;
  
public:
    
  CrossProductStream(std::string name, Source1 const& slowSrc, Source2 const& fastSrc)
    : mName(name), mSlowSource(slowSrc), mFastSource(fastSrc), mSlowPos(0), mPos(), mHead()  { update_position_vector(); }
  
  std::string     name()                       const  { return mName; }  
  bool              has_feature_ready()          const { return !mHead.empty(); }
  std::string     feature_name()               const  { if(mHead.size()==0) return std::string(""); else return mHead[0]->name(); }
  int             number_remaining()           const;
  FeatureVector   pop()                               { FeatureVector z (mHead); mHead.clear(); return z; }

  void            print_to(std::ostream& os)   const;           //   Note the mutable items since sources may change
  
  // protected:
  bool            empty()                      const  { return (mHead.size() == 0) && (number_remaining() == 0); }
  bool            current_feature_is_okay(FeatureVector const& used, FeatureVector const& skipped);
  void            build_next_feature();
  
private:
  void            update_position_vector()     const;
};


template <class Source1, class Source2>
RegulatedStream< CrossProductStream<Source1, Source2> >
make_cross_product_stream (std::string const& name, Source1 const& slowSrc, Source2 const& fastSrc)
{
  return RegulatedStream< CrossProductStream<Source1, Source2> >(CrossProductStream<Source1,Source2>(name, slowSrc, fastSrc));
}



//  PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream
//
//      This stream forms polynomials from features in a *dynamically growing* list. 
//      Canonical example of a stream that builds polynomials from rejected model features.


template<class Source>
class PolynomialStream 
{
  std::string     mName;
  Source const&   mSource;  
  int             mPos;
  int             mDegree;
  FeatureVector   mHead;
  
public:
    
  PolynomialStream(std::string name, Source const& src, int degree)
    : mName(name), mSource(src), mPos(0), mDegree(degree), mHead() { }
  
  std::string     name()                       const { return mName; }
  bool              has_feature_ready()          const { return !mHead.empty(); }
  std::string     feature_name()               const { if (mHead.size()==0) return std::string(""); else return mHead[0]->name(); }
  int             number_remaining()           const { return (mSource.size()-mPos); }
  FeatureVector   pop()                               { FeatureVector z (mHead); mHead.clear(); return z; }

  
  void            print_to(std::ostream& os)   const { os << "PLST: " << name(); if(empty()) os<<" is empty."; else os<< " stream @ " << mPos<<"/"<< mSource.size() ; }
  
protected:
  bool            empty()                      const { return  (number_remaining() == 0); }
  void            build_next_feature();
  bool            current_feature_is_okay(FeatureVector const&, FeatureVector const&);
};


template <class Source>
RegulatedStream< PolynomialStream<Source> >
make_polynomial_stream (std::string const& name, Source const& src, int degree = 3)
{
  return RegulatedStream< PolynomialStream<Source> >(PolynomialStream<Source>(name, src, degree));
}


//  Subspace    Subspace    Subspace    Subspace    Subspace    Subspace    Subspace    Subspace    Subspace    Subspace    
//
//        This stream transforms a "bundle" of features identified into some form or reduced
//        subspace, such as PC, SVD, or RKHS. The stream accumulates a bundle of features of
//        the indicated size that satisfies the input predicate, then applies a transformation.
//               These classes should act like these:
//                    std::unary_function<std::vector<Feature>,std::vector<Feature>> Transformation, as given by adapter functions
//                    std::unary_function<Feature const&, bool>                      Predicate;


template<class Source, class Pred, class Trans>
class SubspaceStream 
{
public:
  
private:
  std::string         mName;
  Source const&       mSource;  
  int                 mPos; 
  int                 mBundleSize;
  Pred                mPredicate;       // is the current feature okay for use (hold this as object, not reference)
  Trans               mTransformation;
  FeatureVector       mHead;
  
public:
  SubspaceStream(std::string name, Source const& src, int bundleSize, Pred pred, Trans trans)  // not const ref to function classes
    : mName(name), mSource(src), mPos(0), mBundleSize(bundleSize), mPredicate(pred), mTransformation(trans), mHead() { }
  
  std::string     name()                       const { return mName; }
  bool              has_feature_ready()          const { return !mHead.empty(); }
  std::string     feature_name()               const { if (mHead.size()==0) return std::string(""); else return mHead[0]->name(); }
  int             number_remaining()           const { return (mSource.size()-mPos); }  
  FeatureVector   pop()                               { FeatureVector z (mHead); mHead.clear(); return z; }
  
  void            print_to(std::ostream& os)   const { os << "SUBS: " << name(); if(empty()) os << " is empty."; else os << " stream @ " << mPos ; }

protected:
  bool            empty()                      const { return ((number_remaining()+(int)mHead.size())<mBundleSize);} //too few to fill
  void            build_next_feature();
  bool            current_feature_is_okay (FeatureVector const&, FeatureVector const&) const { return (mHead.size() != 0);  }

};


template <class Source, class Pred, class Trans>
RegulatedStream< SubspaceStream<Source,Pred,Trans> >
make_subspace_stream (std::string const& name, Source const& src, int bundleSize, Pred pred, Trans trans)
{
  return RegulatedStream< SubspaceStream<Source,Pred,Trans> >(SubspaceStream<Source,Pred,Trans>(name, src, bundleSize, pred, trans));
}



//  Predicate   Predicate   Predicate   Predicate   Predicate   Predicate   Predicate   Predicate   Predicate

class FeatureAcceptancePredicate : public std::unary_function<Feature const&,bool>
{
public:
  bool operator()(Feature const& f) const;
};




///////////////////////////////////////////////////////////////////////

#include "feature_streams.Template.h"

#endif
