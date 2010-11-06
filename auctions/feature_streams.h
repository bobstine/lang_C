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
 calls to primitive methods and handles access to a source of changing collections
 of features via the call to

        can_make_more_features(accepted, rejected)

 The stream must not store these lists of features and can only access them
 during this call. If the stream has/makes a feature, then a winning expert will
 call
 
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
#include "feature_predicates.h"
#include "feature_transformations.h"

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

class NoModel;

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

  FeatureVector pop()                            { return mStream.pop(); }
  void          print_to(std::ostream &os) const { mStream.print_to(os); }

  // these cannot answer since have no count
  // int number_remaining() { return mStream.number_remaining(); }
  
};


template<class Stream>
class RegulatedStream<Stream, NoModel>
{
private:
  Stream       mStream;
  
public:
  RegulatedStream(Stream const& s): mStream(s) { }
  
  bool has_feature ()
  {
    if (mStream.has_feature_ready())
      return true;
    else if (mStream.can_build_more_features())
      mStream.build_next_feature();                    // advance position using current state
    else
      debugging::debug("RGST",3) << "regulated stream '" << mStream.name() <<"' cannot build more features.\n";
    return mStream.has_feature_ready();                  // may not have been able to build one after all
  }

  FeatureVector pop()                            { return mStream.pop(); }
  void          print_to(std::ostream &os) const { mStream.print_to(os); }

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



template< class Iterator, class Transform >
class FeatureStream
{  
private:
  std::string      mName;
  FeatureVector    mHead;         // holds results of most recent build; cleared by pop
  Iterator         mIterator;
  Transform        mTransform;

public:
  ~FeatureStream() { }

  FeatureStream (std::string name, Iterator it, Transform trans)             // fill the head at the start??? transform(colletion.begin)
    : mName(name), mHead(), mIterator(it), mTransform(trans) { }
  
  std::string    name()                            const { return mName; }
  std::string    feature_name()                    const { if (mHead.empty()) return std::string(""); else return mHead[0]->name(); }
  void           print_to(std::ostream& os)        const { os <<  mName << " @ " << feature_name() << " ";  }
  int            number_remaining()                const { return mIterator.number_remaining(); }

  bool           has_feature ();
  bool           can_build_more_features()
    { if(!mIterator.empty())
	++mIterator;
      else std::cout << "FSTR: can_build_more_feaures of " << mName << " finds iterator '" << mIterator << "' is empty.\n";
      return !mIterator.empty();
    }
  void           build_next_feature()                    { mHead = mTransform(*mIterator); }
  
  FeatureVector  pop()                                   { assert (!mHead.empty()); FeatureVector z (mHead); mHead.clear(); return z; }
};

template<class Iterator, class Transform>
std::ostream&
operator<<(std::ostream& os, FeatureStream<Iterator,Transform> const& s) { s.print_to(os); return os; }


// -----------------------------------------------------------------------------------------------------------------------------
//
//     Iterators     Iterators     Iterators     Iterators     Iterators     Iterators     Iterators     Iterators     Iterators
//
// -----------------------------------------------------------------------------------------------------------------------------

class BidOrder
{
public:
  bool operator()(Feature const& a, Feature const& b) const { return (a->entry_bid() < b->entry_bid()); }
};



class FeatureQueue;

class RefCountedQueue
{
public:
  typedef std::priority_queue<Feature, FeatureVector, BidOrder> Queue;
  Queue   mQueue;
  int     mRefCount;
  
  ~RefCountedQueue() {  }
  
  template<class Collection>
  RefCountedQueue(Collection const& c): mQueue(), mRefCount(1)
    { for (typename Collection::const_iterator it=c.begin(); it!=c.end(); ++it) if (! (*it)->is_constant() ) mQueue.push(*it); }
};


template< class Collection, class SkipPredicate >
class FeatureQueueIterator
{
  RefCountedQueue *mpQueue;
  SkipPredicate    mSkipPred;
public:
  ~FeatureQueueIterator() { if(--mpQueue->mRefCount == 0) delete mpQueue; }
  
  FeatureQueueIterator(Collection const& c, SkipPredicate p) : mpQueue(new RefCountedQueue(c)), mSkipPred(p) { }

  FeatureQueueIterator(FeatureQueueIterator const& queue) : mpQueue(queue.mpQueue), mSkipPred(queue.mSkipPred) { ++mpQueue->mRefCount; }

  int    number_remaining()             const { return mpQueue->mQueue.size(); }
  bool   empty()                        const { return mpQueue->mQueue.empty(); }
  
  FeatureQueueIterator&   operator++()        { mpQueue->mQueue.pop(); while( (!mpQueue->mQueue.empty()) && mSkipPred(mpQueue->mQueue.top()) ) mpQueue->mQueue.pop(); return *this; }

  Feature                 operator*()   const { return mpQueue->mQueue.top(); }
    
  void print_to(std::ostream& os)       const { os << "FeatureQueueIterator: Holds " << number_remaining() << " features, with reference count " << mpQueue->mRefCount; }

  //  RefCountedQueue::Queue *operator->()  const { return &mpQueue->mQueue; }  // others dont need access to underlying queue
};

template<class Collection, class Pred>
std::ostream&
operator<< (std::ostream& os, FeatureQueueIterator<Collection,Pred> const& queue) { queue.print_to(os); return os; }

  
template<class Collection, class SkipPredicate>                                   // DelayedIterator    waits for source to grow
class DelayedIterator
{
  typedef typename Collection::const_iterator Iterator;

  Collection const& mSource;         // someone else maintains
  Iterator          mIter;
  SkipPredicate     mSkipFeature;
  
public:
  DelayedIterator(Collection const& source, SkipPredicate pred)
    : mSource(source), mIter(source.begin()), mSkipFeature(pred) { }

  int   number_remaining()              const { debugging::debug("FSTR",2) << "Meaningless call to number_remaining() in delayed iterator.\n"; return 0; }
  bool  empty()                         const { return mIter == mSource.end(); }

  DelayedIterator& operator++()               { ++mIter; while( (mIter != mSource.end()) && mSkipFeature(*mIter)) ++mIter; return *this; }

  Feature          operator*()          const { return *mIter; }

  void  print_to(std::ostream& os)      const { os << "DelayedIterator @ "; if (empty()) os << " empty "; else os << (*mIter)->name() << " "; }
};

template <class Collection, class Pred>
std::ostream&
operator<< (std::ostream& os, DelayedIterator<Collection,Pred> const& it) { it.print_to(os); return os; }



template<class Collection, class SkipPredicate>                                   // CyclicIterator    repeats over and over though collection
class CyclicIterator
{
  typedef typename Collection::const_iterator Iterator;

  Collection const& mSource;         // someone else maintains
  Iterator          mIter;
  int               mSize;
  SkipPredicate     mSkipFeature;
  
public:
  CyclicIterator(Collection const& source, SkipPredicate pred)
    : mSource(source), mIter(source.begin()), mSize(source.size()), mSkipFeature(pred) { }
  
  int   number_remaining()              const { return mSize; }             // number not used in model
  bool  empty()                         const { return mSource.empty() || (mSize == 0); }

  CyclicIterator& operator++();
  Feature         operator*()           const { return *mIter; }

  void  print_to(std::ostream& os)      const { os << "CyclicIterator @ "; if (empty()) os << " empty "; else os << *mIter << " "; }
};

template <class Collection, class Pred>
std::ostream&
operator<< (std::ostream& os, CyclicIterator<Collection,Pred> const& it) { it.print_to(os); return os; }



class LagIterator
{
  const Feature   mFeature;       // construct lags of this feature
  const int       mBlockSize;     // blocking factor used if longitudinal
  int             mRemaining;
  int             mLag;
  int             mMaxLag;    // cycle through the lags
  
public:  
  LagIterator(Feature const& f, int maxLag, int cycles, int blockSize)
    :  mFeature(f), mBlockSize(blockSize), mRemaining(1+maxLag*cycles), mLag(0), mMaxLag(maxLag) {  }    // 1+ for initial increment
  
  int   number_remaining()         const   { return  mRemaining; }
  bool  empty()                    const   { return  mRemaining == 0; }

  LagIterator&  operator++();
  Feature       operator*()        const   { return  Feature(mFeature,mLag,mBlockSize); }

  void  print_to(std::ostream& os) const { os << "LagIterator @ "; if (empty()) os << " empty "; else os << " lag " << mLag << "/" << mMaxLag << " with " << mRemaining << " left. "; }
};

inline
std::ostream&
operator<< (std::ostream& os, LagIterator const& it) { it.print_to(os); return os; }



template< class Model >
class ModelIterator
{
  Model const& mModel;    // maintained by someone else
  int          mLastQ;
public:
  ModelIterator(Model const& m): mModel(m), mLastQ(0) {}

  bool   empty()                   const;

  ModelIterator&  operator++()           { return *this; }

  Model const&    operator*()            { mLastQ = mModel.q(); return mModel; }

  void  print_to(std::ostream& os) const { os << "ModelIterator, last q=" << mLastQ << "; model @ " << mModel.q() << " "; }
};
 
template <class Model>
std::ostream&
operator<< (std::ostream& os, ModelIterator<Model> const& it) { it.print_to(os); return os; }



template< class Collection, class SkipPred >
class BundleIterator
{
  typedef typename Collection::const_iterator Iterator;
  
  Collection const&  mSource;      // maintained by someone else
  int                mBundleSize;
  SkipPred           mSkipPred;
  FeatureVector      mBundle;
  Iterator           mIter;        // position of end of last bundle
public:
  BundleIterator(Collection const& source, int bundleSize, SkipPred pred) : mSource(source), mBundleSize(bundleSize), mSkipPred(pred), mBundle(), mIter(mSource.end()) { }

  bool     empty()      const     { std::cout << "  Checking empty; size=" << mBundle.size() << " and result is " << (mIter==mSource.end()) << std::endl; return mIter == mSource.end(); }

  BundleIterator& operator++()
    { while((int)mBundle.size() < mBundleSize && mIter != mSource.end())
      { std::cout << " incrementing\n";
	if (!mSkipPred(*mIter))
	{ std::cout << " bundle grows \n";
	  mBundle.push_back(*mIter);
	}
	++mIter;
      }
      return *this;
    }
  FeatureVector   operator*()     { assert((int)mBundle.size() == mBundleSize);  FeatureVector z (mBundle); mBundle.clear(); return z; }
      
  void print_to(std::ostream& os) const { os << "BundleIterator, sized " << mBundle.size() << "/" << mBundleSize; }
};

template <class Collection, class Pred>
std::ostream&
operator<< (std::ostream& os, BundleIterator<Collection,Pred> const& it) { it.print_to(os); return os; }


    
// -----------------------------------------------------------------------------------------------------------------------------
//
//    make__stream     make__stream     make__stream     make__stream     make__stream     make__stream
//
// -----------------------------------------------------------------------------------------------------------------------------

template<class Collection, class Pred>
FeatureStream< CyclicIterator<Collection, Pred>, Identity >
make_finite_stream (std::string const& name, Collection const& source, Pred pred)
{
  return FeatureStream< CyclicIterator<Collection, Pred>, Identity >
    ("CyclicStream::"+name, CyclicIterator<Collection,Pred>(source, pred), Identity());
}


inline
FeatureStream< LagIterator, Identity >
make_lag_stream (std::string const& name, Feature const& f, int maxLag, int blockSize, int numberCycles)
{
  return FeatureStream< LagIterator, Identity >("LagStream::"+name, LagIterator(f, maxLag, numberCycles, blockSize), Identity());
}


template <class Collection>
FeatureStream< DelayedIterator<Collection, SkipIfDerived>, BuildPolynomialFeature>
make_polynomial_stream (std::string const& name, Collection const& src, int degree)
{
  std::cout << "TEST: make_polynomial_stream of degree " << degree << std::endl;
  return FeatureStream< DelayedIterator<Collection, SkipIfDerived>, BuildPolynomialFeature>
    ("Polynomial::"+name, DelayedIterator<Collection,SkipIfDerived>(src, SkipIfDerived()), BuildPolynomialFeature(degree));
}


template <class Collection>
FeatureStream< DelayedIterator<Collection, SkipIfDerived>, BuildNeighborhoodFeature>
make_neighborhood_stream (std::string const& name, Collection const& src, IntegerColumn const& col)
{
  std::cout << "TEST: make_neighborhood_stream with indices " << col << std::endl;
  return FeatureStream< DelayedIterator<Collection, SkipIfDerived>, BuildNeighborhoodFeature>
    ("Neighborhood::"+name, DelayedIterator<Collection,SkipIfDerived>(src, SkipIfDerived()), BuildNeighborhoodFeature(col));
}


template <class Collection>
FeatureStream< FeatureQueueIterator<Collection, SkipIfRelated>, BuildProductFeature>
make_feature_product_stream (std::string const& name, Collection const& c, Feature const& f)
{
  std::cout << "FPRS: make_feature_product_stream from feature " << f->name() << std::endl;
  return FeatureStream< FeatureQueueIterator<Collection,SkipIfRelated>, BuildProductFeature>
    ("Feature-product::"+name, FeatureQueueIterator<Collection, SkipIfRelated>(c, SkipIfRelated(f)), BuildProductFeature(f));
}


template <class Model>
FeatureStream< ModelIterator<Model>, BuildCalibrationFeature<Model> >
make_calibration_stream (std::string const& name, Model const& model, int degree, int skip)
{
  std::cout << "FPRS: make_calibration_stream of degree " << degree << " with initial skip of " << skip << " cases.\n";
  return FeatureStream< ModelIterator<Model>, BuildCalibrationFeature<Model> >
    ("Calibration::"+name, ModelIterator<Model>(model), BuildCalibrationFeature<Model>(degree,skip));
}


template <class Collection, class Trans>
FeatureStream< BundleIterator<Collection, SkipIfInBasis>, Trans >
make_subspace_stream (std::string const& name, Collection const& src, Trans const& trans, int bundleSize)
{
  std::cout << "FPRS: make_subspace_stream with bundle size " << bundleSize << std::endl;
  return FeatureStream< BundleIterator<Collection,SkipIfInBasis>, Trans >
    ("Subspace::"+name, BundleIterator<Collection,SkipIfInBasis>(src, bundleSize, SkipIfInBasis()), trans);
}

//  BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream
//


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
  
  int   number_remaining() const { assert(false); return -1; }                                    // here for interface only

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




///////////////////////////////////////////////////////////////////////

#include "feature_streams.Template.h"

#endif
