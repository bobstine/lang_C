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
	
 before placing any bids.  The regulated class stream coordinates a sequence of
 calls to primitive methods and handles access to the underlying model via the
 call to

        can_make_more_features(accepted, rejected)

  This must the the *only* access to these lists in order to keep the thread
  code away from them. If the stream has/makes a feature, then a winning expert
  will call
 
        pop()

 which must return a feature vector.  pop() must (a) run very fast and (b)
 return a vector of features.  Heavy lifting required in order to be able to
 pop() must be done in the method build_next_feature().  Certain classes of
 bidders in the calling expert may ask for the number of remaining features in
 order to determine how much to bid.  In order to support such bidding, the
 stream will need to implement

        number_remaining()
 
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

template<class Stream, class Model>
class RegulatedStream
{
private:
  Stream       mStream;
  Model const& mModel;
  
public:
  RegulatedStream(Stream const& s, Model const& m): mStream(s), mModel(m) { }
  
  bool has_feature ()
  {
    if (mStream.has_feature_ready())
      return true;
    else if (mStream.can_build_more_features(mModel.accepted_features(), mModel.rejected_features()))
      mStream.build_next_feature();                    // advance position using current state
    else
      debugging::debug("RGST",3) << "regulated stream '" << mStream.name() <<"' cannot build more features.\n";
    return mStream.has_feature_ready();                  // may not have been able to build one after all
  }

  FeatureVector pop() { return mStream.pop(); }

  int number_remaining() { return mStream.number_remaining(); }
  
};

#else

template<class Stream>
class RegulatedStream: public Stream
{  
private:
  Threaded< RegulatedStream<Stream> > m_thread;

public:
  RegulatedStream(Stream const& s)  : Stream(s), m_thread() { }
  
  bool has_feature ()
  {
    if(m_thread.done() && Stream::has_feature_ready())
      return true;
    else if (Stream::can_build_more_features())
      m_thread( boost::bind( &Stream::build_next_feature, this ) );
    else
      debugging::debug("RGST",3) << "threaded, regulated stream '" << Stream::name() <<"' cannot build more features.\n";
    return false;
  }
};

#endif


//  BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream

class BaseStream
{
public:
  typedef       FeatureList::const_iterator    FeatureIterator;
  
private:
  std::string      mName;
  FeatureVector    mHead;                            // holds results of most recent build; cleared by pop

public:
  virtual ~BaseStream() { }
  
  BaseStream (std::string name) : mName(name), mHead() { }
  
  std::string    name()                            const { return mName; }
  std::string    feature_name()                    const { if (mHead.empty()) return std::string(""); else return mHead[0]->name(); }
  void           print_to(std::ostream& os)        const { os <<  mName << " @ " << feature_name() << std::endl;  }
  bool           has_feature_ready()               const { return !mHead.empty(); }

  // these are not here, but that's the idea
  //  virtual bool   can_build_more_features(FeatureList const& accepted, FeatureList const& rejected) = 0;
  //  virtual void   build_next_feature()                    = 0;
  
  void           set_head(Feature const& f)              { mHead.clear(); mHead.push_back(f); }
  void           set_head(FeatureVector const& fv)       { mHead = fv; }
  
  FeatureVector  pop()                                   { assert (!mHead.empty()); FeatureVector z (mHead); mHead.clear(); return z; }

  // utilities used in checking features
  bool           indicators_from_same_parent  (Feature const& f1, Feature const& f2) const;
 
  std::string    last_name_in_list (FeatureList const& r) const;

  template< class Collection >
  bool found_name_among_features (std::string const& name, Collection const& features, std::string const& description) const;
};



//  FiniteStream     FiniteStream     FiniteStream     FiniteStream     FiniteStream     FiniteStream     FiniteStream     FiniteStream     

//  Builds revolving deque of features from given feature vector (typically read from a file)

class FiniteStream:  public BaseStream
{
  std::deque<Feature>  mFeatures;  
  
public:
  
  FiniteStream(std::string const& name, FeatureVector const& source)
    : BaseStream("FiniteStream:" + name) { insert_features(source); }
  
  int   number_remaining()                                              const { return mFeatures.size(); }
  bool  can_build_more_features(FeatureList const&, FeatureList const&) const { return number_remaining()>0; }

  void  build_next_feature();

  void  print_features_to (std::ostream& os) const;

private:
  void  insert_features (FeatureVector const& features);
};


template<class Model>
RegulatedStream< FiniteStream, Model >
make_finite_stream (Model const& m, std::string const& name, FeatureVector const& source)
{
  return RegulatedStream< FiniteStream, Model >(FiniteStream(name, source), m);
}



//  LagStream     LagStream     LagStream     LagStream     LagStream     LagStream     LagStream     LagStream

//  Build sequence of lags from a single feature

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
  
  int   number_remaining()                                              const   { return  mMaxLag - mLag + mCyclesLeft * mMaxLag; }
  bool  can_build_more_features(FeatureList const&, FeatureList const&) const   { return  number_remaining() > 0; } 

  void  build_next_feature ();
};


template< class Model >
RegulatedStream< LagStream, Model >
make_lag_stream (Model const& m, std::string const& name, Feature const& f, int maxLag, int blockSize, int numberCycles)
{
  return RegulatedStream< LagStream, Model >(LagStream(name, f, maxLag, blockSize, numberCycles), m);
}



//  NeighborhoodStream     NeighborhoodStream     NeighborhoodStream     NeighborhoodStream     NeighborhoodStream     NeighborhoodStream

//  Build 'neighboring' features to those in a given source collection

template<class Source>
class NeighborhoodStream: public BaseStream
{
  typename Source::const_iterator mpFeature;
  const std::string               mSignature;    // look for this in name of variable before using
  IntegerColumn                   mIndexColumn;
  int                             mRemain;
  
public:

  NeighborhoodStream(std::string const& name, Source const& src, std::string sig, IntegerColumn const& indices)
    :  BaseStream("NhoodStream:" + name), mpFeature(src.begin()), mSignature(sig), mIndexColumn(indices), mRemain(src.size()) {  }
  
  int   number_remaining()                                              const   { return mRemain; }
  bool  can_build_more_features(FeatureList const&, FeatureList const&) const   { return mRemain > 0; }

  void  build_next_feature();
};


template <class Source, class Model>
RegulatedStream< NeighborhoodStream<Source>, Model>
make_neighborhood_stream (Model const& m, std::string const& name, Source const& src, std::string signature, IntegerColumn const& col)
{
  return RegulatedStream< NeighborhoodStream<Source>, Model >(NeighborhoodStream<Source>(name, src, signature, col), m);
}



//  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream

template<class Model>
class FitStream : public BaseStream
{
  mutable int            mLastQ;           // used to detect change in model size
  int                    mPower;
  Model          const&  mModel;
  std::string            mSignature;       // prefix for variable names so that it can recognize them
  Column                 mFit;             // holds fit values from model
  int                    mSkip;            // context rows to pad when returning fit
  
public:
  
  FitStream(Model const& model, int power, std::string s, int skip)
    : BaseStream("FitStream:" + s), mLastQ(0), mPower(power), mModel(model), mSignature(s), mFit(), mSkip(skip) {  }
  
  int    number_remaining() const { return -1; }
  
  bool   can_build_more_features(FeatureList const& accepted, FeatureList const&)  const;
  void   build_next_feature();
};


template <class Model>
RegulatedStream< FitStream<Model>, Model >
make_fit_stream (Model const& m, int powers, std::string signature, int skip)
{
  return RegulatedStream< FitStream<Model>, Model >(FitStream<Model>(m, powers, signature, skip), m);
}



//  InteractionStream  InteractionStream  InteractionStream  InteractionStream  InteractionStream  InteractionStream  

template<class Source>
class InteractionStream : public BaseStream
{
private:
  typedef typename Source::const_iterator Iter;

  Source const& mSource;
  bool          mIncludeDiagonal;
  Iter          mpDiagFeature, mpColFeature;
  int           mRemain;
  
public:
  
  InteractionStream(std::string name, Source const& src, bool useSquares)
    : BaseStream("InteractionStream:"+name), mIncludeDiagonal(useSquares), mSource(src),
      mpDiagFeature(src.begin()), mpColFeature(src.end()), mRemain(initial_count(src.size())) { if(!mIncludeDiagonal) ++mpColFeature;  }
  
  int   number_remaining()                                               const { return mRemain; }

  bool  can_build_more_features (FeatureList const&, FeatureList const&) const { return (mRemain > 0) && find_next_position(); }

  void  build_next_feature()                                                   { assert(mpColFeature != mSource.end()); set_head(Feature(*mpDiagFeature, *mpColFeature)); }
      
private:
  int   initial_count(int k)         const { return (k*k-k)/2 + (mIncludeDiagonal?k:0); }
  void  inc_pointers();
  bool  find_next_position();
};


template <class Source, class Model>
RegulatedStream< InteractionStream<Source>, Model >
make_interaction_stream (Model const& m, std::string const& name, Source const& s, bool useSquares)
{
  return RegulatedStream< InteractionStream<Source>, Model >(InteractionStream<Source>(name, s, useSquares), m);
}



//  FeatureProductStream    FeatureProductStream    FeatureProductStream    FeatureProductStream    FeatureProductStream    

class BidOrder
{
public:
  bool operator()(Feature const& a, Feature const& b) const { return (a->entry_bid() < b->entry_bid()); }
};

class FeatureProductStream : public BaseStream
{
  Feature        mFeature;
  std::priority_queue<Feature, FeatureVector, BidOrder> mQueue;             // Make a priority queue out of a fixed source list of features
  
public:

  FeatureProductStream(Feature f, FeatureList const& src)
    : BaseStream("FPStream:"+f->name()), mFeature(f) { initialize_queue(src);  }
  
  int   number_remaining()           const { return mQueue.size(); }
  
  bool  can_build_more_features(FeatureList const& accepted, FeatureList const&);

  void  build_next_feature();

private:
  void  initialize_queue(FeatureList const& s);

};


template <class Model>
RegulatedStream< FeatureProductStream, Model >
make_feature_product_stream (Model const& m, Feature f, FeatureList const& Src)
{
  return RegulatedStream< FeatureProductStream, Model >(FeatureProductStream(f, Src), m);
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
  

class CrossProductStream : public BaseStream
{
  FeatureIterator              mSlowIterator;    // traverses accepted list
  std::vector<FeatureIterator> mFastIterators;   // rejected list
  
public:
    
  CrossProductStream(std::string name)
    : BaseStream("CPStream:"+name), mSlowIterator(), mFastIterators()  {  }
  
  int   number_remaining() const { return -1; }

  bool  can_build_more_features(FeatureList const& accepted, FeatureList const& rejected) const;

  void  build_next_feature();

private:
  void  update_iterators();
};


template <class Model>
RegulatedStream< CrossProductStream, Model >
make_cross_product_stream (Model m, std::string const& name)
{
  return RegulatedStream< CrossProductStream, Model >(CrossProductStream(name), m);
}


//  ModelStreamID     ModelStreamID     ModelStreamID     ModelStreamID     ModelStreamID     ModelStreamID


enum ModelStreamID {acceptedStreamID, rejectedStreamID};


//  PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream
//
//      This stream forms polynomials from features in a *dynamically growing* list. 
//      Canonical example of a stream that builds polynomials from rejected model features.


class PolynomialStream : public BaseStream
{
private:
  ModelStreamID   mID;
  FeatureIterator mIterator;
  int             mDegree;
  
public:
    
  PolynomialStream(std::string name, ModelStreamID id, FeatureList const& source, int degree)
    : BaseStream("PolyStream:"+name), mID(id), mIterator(source.end()), mDegree(degree) { }

  int   number_remaining() const { return -1; }

  bool  can_build_more_features(FeatureList const& accepts, FeatureList const& rejects);

  void  build_next_feature();
  
};


template <class Model>
RegulatedStream< PolynomialStream, Model >
make_polynomial_stream (Model const& m, std::string const& name, ModelStreamID id, FeatureList const& src, int degree = 3)
{
  return RegulatedStream< PolynomialStream, Model>(PolynomialStream(name, id, src, degree), m);
}


//  Subspace    Subspace    Subspace    Subspace    Subspace    Subspace    Subspace    Subspace    Subspace    Subspace    
//
//        This stream transforms a "bundle" of features identified into some form or reduced
//        subspace, such as PC, SVD, or RKHS. The stream accumulates a bundle of features of
//        the indicated size that satisfies the input predicate, then applies a transformation.
//               These classes should act like these:
//                    std::unary_function<std::vector<Feature>,std::vector<Feature>> Transformation, as given by adapter functions
//                    std::unary_function<Feature const&, bool>                      Predicate;


template<class Pred, class Trans>
class SubspaceStream : BaseStream
{
public:
  
private:
  ModelStreamID   mID;
  FeatureIterator mIterator;
  FeatureVector   mBundle;
  int             mBundleSize;
  Pred            mPredicate;       // is the current feature okay for use (hold this as object, not reference)
  Trans           mTransformation;
  
public:
  SubspaceStream(std::string name, ModelStreamID id, FeatureList const& src, int bundleSize, Pred pred, Trans trans)  // not const ref to function classes
    : BaseStream("Subspace:"+name), mID(id), mIterator(src.end()), mBundle(), mBundleSize(bundleSize), mPredicate(pred), mTransformation(trans) { }
  
  int    number_remaining() const { return -1; }

  bool   can_build_more_features(FeatureList const& accepted, FeatureList const& rejected);
  
  void   build_next_feature();

};


template <class Pred, class Trans, class Model>
RegulatedStream< SubspaceStream<Pred,Trans>, Model >
make_subspace_stream (Model const& m, std::string const& name, ModelStreamID id,  FeatureList const& src, int bundleSize, Pred pred, Trans trans)
{
  return RegulatedStream< SubspaceStream<Pred,Trans>, Model >(SubspaceStream<Pred,Trans>(name, id, src, bundleSize, pred, trans), m);
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
