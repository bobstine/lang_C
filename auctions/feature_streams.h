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
 
 Feature streams implement an abstract protocol and deliver upon request the
 'next' feature.  Each stream combines an iterator that traverses some
 underlying collection of features (be they in a list or generated by a model)
 and a tranformation that operates on the result of the iterator.  The iterator
 can return whatever, so long as it cooperates with the transformation to
 produce a feature vector.  A threaded feature stream runs in a separate thread.

 Feature streams (along with bidders) are held in experts that participate in the
 auction. Experts call
 
        has_feature_vector()
	
 before placing any bids. If the stream has a feature, then a winning
 expert will call
 
        pop_feature_vector()

 which must return a feature vector.  pop must (a) run very fast and (b)
 return a vector of features.  Heavy lifting required in order to be able to
 pop must be done in the transformation class.  Certain classes of bidders in
 the calling expert may ask for the number of remaining features in order to
 determine how much to bid.  In order to support such bidding, the stream will
 need to implement (usually via its iterator)

        number_remaining()
 
 Stream properties...
    Streams should be *lightweight*.  They will be copied heavily in the auction.

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
#include "feature_iterators.h"
#include "feature_transformations.h"

#include "thread_functions.h"
#include "debug.h"

#include <iostream>
#include <memory>   // shared_ptr

using debugging::debug;

template<class Iterator, class Op>
class FeatureStream
{
  typedef FeatureTransformation<Op> Transform;
  
private:
  std::string     mName;
  Iterator        mIterator;
  Transform       mTransform;
  FeatureVector   mFeatureVector;
  
public:
  ~FeatureStream() { }

  FeatureStream (std::string name, Iterator it, Op op)
    : mName(name), mIterator(it), mTransform(FeatureTransformation<Op>(op)) { try_to_make_features(); }

  std::string    name()                          const { return mName; }
  bool           cannot_generate_more_features() const { return (0 == number_remaining()); }
  int            number_remaining()              const { return mIterator.number_remaining(); }

  bool           has_feature_vector()            const { return mFeatureVector.size()>0; }
  std::string    first_feature_name()            const { if(has_feature_vector()) return mFeatureVector[0]->name(); else return std::string("empty"); }
  
  FeatureVector  pop_feature_vector()                  { FeatureVector fv = std::move(mFeatureVector); try_to_make_features(); return fv; }

  void           print_to(std::ostream& os)      const { os << "feature_stream `" << mName << "' with " << number_remaining() << " remaining features "; }

private:
  void  try_to_make_features()
  { if(mIterator.points_to_valid_data())
    { mFeatureVector = mTransform(*mIterator);
      ++mIterator;
    }
  }
};

template<class Iterator, class Transform>
std::ostream&
operator<<(std::ostream& os, FeatureStream<Iterator,Transform> const& s) { s.print_to(os); return os; }


//     ThreadedFeatureStream     ThreadedFeatureStream     ThreadedFeatureStream     ThreadedFeatureStream     ThreadedFeatureStream     

template<class Iterator, class Op>
class ThreadedFeatureStream
{
  typedef thread_function<FeatureTransformation<Op>> Transform;
  
private:
  std::string       mName;
  Iterator          mIterator;
  mutable bool      mHaveStartedThread;
  std::shared_ptr<Transform> mpTransform;
  
public:
  ~ThreadedFeatureStream() { }

  ThreadedFeatureStream (std::string name, Iterator it, Op op)
    : mName(name), mIterator(it), mHaveStartedThread(false), mpTransform(new Transform("TFS", op)) { }

  std::string    name()                          const { return mName; }
  bool           cannot_generate_more_features() const { return (0 == number_remaining()); }
  int            number_remaining()              const { return mIterator.number_remaining(); }
  
  bool           has_feature_vector()
    { if (mHaveStartedThread)
      	return mpTransform->finished();
      else
      { start_thread();
	return false;
      }
    }	 
  std::string    first_feature_name()            const { if(has_feature_vector()) return mpTransform->result[0].name(); else return std::string("empty"); }

  FeatureVector  pop_feature_vector()
    { assert(mHaveStartedThread && mpTransform->finished());
      FeatureVector fv = mpTransform->result();
      ++mIterator;
      start_thread();
      return fv;
    }

  void           print_to(std::ostream& os)      const { os << "threaded_feature_stream `" << mName << "' with " << number_remaining() << " remaining features "; }

private:
  void start_thread() 
    { assert (mpTransform->finished());
      if (mIterator.points_to_valid_data())
      { (*mpTransform)(*mIterator);
	mHaveStartedThread = true;
      }
      else mHaveStartedThread = false;
    }
};


// -----------------------------------------------------------------------------------------------------------------------------
//
//    make__stream     make__stream     make__stream     make__stream     make__stream     make__stream
//
// -----------------------------------------------------------------------------------------------------------------------------

template<class Collection, class Pred>
FeatureStream< CyclicIterator<Collection, Pred>, Identity>
make_finite_stream (std::string const& name, Collection const& source, Pred pred)
{
  debug("FSTR",3) << "make_finite_stream (cyclic) " << name << " from " << source.size() << " features." << std::endl;
  return FeatureStream< CyclicIterator<Collection, Pred>, Identity>
    ("CyclicStream::"+name, CyclicIterator<Collection,Pred>(source, pred), Identity());
}


template<class Collection, class Pred, class Operator>
FeatureStream<DynamicIterator<Collection, Pred>, Operator>
make_dynamic_stream (std::string const& name, Collection const& source, Pred pred, Operator op)
{
  debug("FSTR",3) << "make_dynamic_stream " << name << std::endl;
  return FeatureStream< DynamicIterator<Collection, Pred>, Operator >
    ("DynamicStream::"+name, DynamicIterator<Collection,Pred>(source, pred), op);
}


inline  
FeatureStream<LagIterator, Identity>
make_lag_stream (std::string const& name, Feature const& f, int maxLag, int numberCycles, int blockSize)
{
  debug("FSTR",3) << "make_lag_stream " << name << " from feature " << f << std::endl;
  return FeatureStream<LagIterator, Identity>
    ("LagStream::"+name, LagIterator(f, maxLag, numberCycles, blockSize), Identity());
}


template <class Collection>
FeatureStream< DynamicIterator<Collection, SkipIfDerived>, BuildPolynomialFeatures >
make_polynomial_stream (std::string const& name, Collection const& src, int degree)
{
  debug("FSTR",3) << "make_polynomial_stream " << name << std::endl;
  return FeatureStream< DynamicIterator<Collection, SkipIfDerived>, BuildPolynomialFeatures >
    ("Polynomial::"+name, DynamicIterator<Collection,SkipIfDerived>(src, SkipIfDerived()), BuildPolynomialFeatures(degree));
}


template <class Collection>
FeatureStream< DynamicIterator<Collection, SkipIfDerived>, BuildNeighborhoodFeature>
make_neighborhood_stream (std::string const& name, Collection const& src, IntegerColumn const& col)
{
  debug("FSTR",3) << "make_neighborhood_stream " << name << std::endl;
  return FeatureStream< DynamicIterator<Collection, SkipIfDerived>, BuildNeighborhoodFeature>
    ("Neighborhood::"+name, DynamicIterator<Collection,SkipIfDerived>(src, SkipIfDerived()), BuildNeighborhoodFeature(col));
}


template <class Collection>
FeatureStream< QueueIterator<Collection, SkipIfRelated>, BuildProductFeature>
make_feature_product_stream (std::string const& name, Feature const& f, Collection const& c)
{
  debug("FSTR",3) << "make_feature_product_stream from feature " << f->name() << std::endl;
  return FeatureStream< QueueIterator<Collection,SkipIfRelated>, BuildProductFeature>
    ("Feature-product::"+name, QueueIterator<Collection, SkipIfRelated>(c, SkipIfRelated(f)), BuildProductFeature(f));
}


template <class Model>
FeatureStream< ModelIterator<Model>, BuildCalibrationFeature<Model> >
make_calibration_stream (std::string const& name, Model const& model, int gap, std::string signature, int skip, bool binary)
{
  debug("FSTR",2) << "make_calibration_stream; gap between = " << gap << "  initial skip = " << skip << " cases      binary = " << binary << std::endl;
  return FeatureStream< ModelIterator<Model>, BuildCalibrationFeature<Model> >
    ("Calibration::"+name, ModelIterator<Model>(model, gap), BuildCalibrationFeature<Model>(3,signature,skip, binary));  // 3 = cubic
}



template <class Collection, class Trans>
FeatureStream< BundleIterator<Collection, SkipIfInBasis>, Trans >
make_subspace_stream (std::string const& name, Collection const& src, Trans const& trans, int bundleSize)
{
  debug("FSTR",3) << "make_subspace_stream with bundle size " << bundleSize << std::endl;
  return FeatureStream< BundleIterator<Collection,SkipIfInBasis>, Trans>
    ("Subspace::"+name, BundleIterator<Collection,SkipIfInBasis>(src, bundleSize, SkipIfInBasis()), trans);
}


template <class Collection, class Trans>
ThreadedFeatureStream< BundleIterator<Collection, SkipIfInBasis>, Trans >
make_threaded_subspace_stream (std::string const& name, Collection const& src, Trans const& trans, int bundleSize)
{
  debug("FSTR",3) << "make_threaded_subspace_stream with bundle size " << bundleSize << std::endl;
  return ThreadedFeatureStream< BundleIterator<Collection,SkipIfInBasis>, Trans>
    ("Subspace::"+name, BundleIterator<Collection,SkipIfInBasis>(src, bundleSize, SkipIfInBasis()), trans);
}


template <class Collection>
FeatureStream< InteractionIterator<Collection, SkipIfRelatedPair>, Identity>
make_interaction_stream (std::string const& name, Collection const& src, bool useSquares)
{
  debug("FSTR",3) << "make_interaction_stream (static) " << std::endl;
  return FeatureStream< InteractionIterator<Collection,SkipIfRelatedPair>, Identity>
    ("Interaction::"+name, InteractionIterator<Collection,SkipIfRelatedPair>(src, useSquares, SkipIfRelatedPair()), Identity());
}


inline
FeatureStream< CrossProductIterator<SkipIfRelatedPair>, Identity >
make_cross_product_stream (std::string const& name, FeatureVector const& slow, FeatureVector const& fast)
{
  debug("FSTR",3) << "make_interaction_stream (static) " << std::endl;
  return FeatureStream< CrossProductIterator<SkipIfRelatedPair>, Identity>
    ("CrossProduct::"+name, CrossProductIterator<SkipIfRelatedPair>(slow, fast, SkipIfRelatedPair()), Identity());
}


///////////////////////////////////////////////////////////////////////

#endif
