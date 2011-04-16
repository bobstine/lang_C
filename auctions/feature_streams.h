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
 produce a feature vector.

 Feature streams (along with bidders) are held in experts that participate in the
 auction. Experts call
 
        has_feature()
	
 before placing any bids. If the stream has/makes a feature, then a winning
 expert will call
 
        pop()

 which must return a feature vector.  pop() must (a) run very fast and (b)
 return a vector of features.  Heavy lifting required in order to be able to
 pop() must be done in the transformation class.  Certain classes of bidders in
 the calling expert may ask for the number of remaining features in order to
 determine how much to bid.  In order to support such bidding, the stream will
 need to implement

        number_remaining()
 
 Stream properties...
    Streams should be *lightweight*.  They will be copied heavily in the auction.
    Basically act as a stack/queue, a type of iterator really.  The stream itself
    does *not* hold data, just the rules to build new variables.

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

#include "light_threads.h"
#include "debug.h"

#include <iostream>



template<class Iterator, class Op>
class FeatureStream
{
  typedef FeatureTransformation<Op>                   Transform;
  typedef boost::shared_ptr< LightThread<Transform> > ThreadPointer;
  
private:
  std::string     mName;
  Iterator        mIterator;
  Transform       mTransform;    // copy each time start a new thread
  ThreadPointer   mpThread;    

public:
  ~FeatureStream() { }

  FeatureStream (std::string name, Iterator it, Op op)
    : mName(name), mIterator(it), mTransform(op), mpThread(new LightThread<Transform>(name))  { make_features(); }

  std::string    name()                      const;
  std::string    feature_name()              const;
  void           print_to(std::ostream& os)  const;

  int            number_remaining()          const;

  bool           is_busy()                   const;
  bool           is_empty()                  const;
  bool           has_feature();
  FeatureVector  pop();

private:
  void make_features();
  bool const_has_feature()                   const;
};

template<class Iterator, class Transform>
std::ostream&
operator<<(std::ostream& os, FeatureStream<Iterator,Transform> const& s) { s.print_to(os); return os; }


// -----------------------------------------------------------------------------------------------------------------------------
//
//    make__stream     make__stream     make__stream     make__stream     make__stream     make__stream
//
// -----------------------------------------------------------------------------------------------------------------------------

template<class Collection, class Pred>
FeatureStream< CyclicIterator<Collection, Pred>, Identity>
make_finite_stream (std::string const& name, Collection const& source, Pred pred)
{
  return FeatureStream< CyclicIterator<Collection, Pred>, Identity>
    ("CyclicStream::"+name, CyclicIterator<Collection,Pred>(source, pred), Identity());
}


template<class Collection, class Pred, class Operator>
FeatureStream<DynamicIterator<Collection, Pred>, Operator>
make_dynamic_stream (std::string const& name, Collection const& source, Pred pred, Operator op)
{
  return FeatureStream< DynamicIterator<Collection, Pred>, Operator >
    ("DynamicStream::"+name, DynamicIterator<Collection,Pred>(source, pred), op);
}


inline  
FeatureStream<LagIterator, Identity>
make_lag_stream (std::string const& name, Feature const& f, int maxLag, int numberCycles, int blockSize)
{
  return FeatureStream<LagIterator, Identity>
    ("LagStream::"+name, LagIterator(f, maxLag, numberCycles, blockSize), Identity());
}


template <class Collection>
FeatureStream< DynamicIterator<Collection, SkipIfDerived>, BuildPolynomialFeatures >
make_polynomial_stream (std::string const& name, Collection const& src, int degree)
{
  return FeatureStream< DynamicIterator<Collection, SkipIfDerived>, BuildPolynomialFeatures >
    ("Polynomial::"+name, DynamicIterator<Collection,SkipIfDerived>(src, SkipIfDerived()), BuildPolynomialFeatures(degree));
}


template <class Collection>
FeatureStream< DynamicIterator<Collection, SkipIfDerived>, BuildNeighborhoodFeature>
make_neighborhood_stream (std::string const& name, Collection const& src, IntegerColumn const& col)
{
  return FeatureStream< DynamicIterator<Collection, SkipIfDerived>, BuildNeighborhoodFeature>
    ("Neighborhood::"+name, DynamicIterator<Collection,SkipIfDerived>(src, SkipIfDerived()), BuildNeighborhoodFeature(col));
}


template <class Collection>
FeatureStream< QueueIterator<Collection, SkipIfRelated>, BuildProductFeature>
make_feature_product_stream (std::string const& name, Feature const& f, Collection const& c)
{
  debugging::debug("FPRS",2) << "make_feature_product_stream from feature " << f->name() << std::endl;
  return FeatureStream< QueueIterator<Collection,SkipIfRelated>, BuildProductFeature>
    ("Feature-product::"+name, QueueIterator<Collection, SkipIfRelated>(c, SkipIfRelated(f)), BuildProductFeature(f));
}


template <class Model>
FeatureStream< ModelIterator<Model>, BuildCalibrationFeature<Model> >
make_calibration_stream (std::string const& name, Model const& model, int gap, std::string signature, int skip, bool binary)
{
  debugging::debug("FPRS",1) << "make_calibration_stream; gap between = " << gap << "  initial skip = " << skip << " cases      binary = " << binary << std::endl;
  return FeatureStream< ModelIterator<Model>, BuildCalibrationFeature<Model> >
    ("Calibration::"+name, ModelIterator<Model>(model, gap), BuildCalibrationFeature<Model>(3,signature,skip, binary));  // 3 = cubic
}



template <class Collection, class Trans>
FeatureStream< BundleIterator<Collection, SkipIfInBasis>, Trans >
make_subspace_stream (std::string const& name, Collection const& src, Trans const& trans, int bundleSize)
{
  debugging::debug("FPRS",2) << "make_subspace_stream with bundle size " << bundleSize << std::endl;
  return FeatureStream< BundleIterator<Collection,SkipIfInBasis>, Trans>
    ("Subspace::"+name, BundleIterator<Collection,SkipIfInBasis>(src, bundleSize, SkipIfInBasis()), trans);
}


template <class Collection>
FeatureStream< InteractionIterator<Collection, SkipIfRelatedPair>, Identity>
make_interaction_stream (std::string const& name, Collection const& src, bool useSquares)
{
  debugging::debug("FPRS",2) << "make_interaction_stream (static) " << std::endl;
  return FeatureStream< InteractionIterator<Collection,SkipIfRelatedPair>, Identity>
    ("Interaction::"+name, InteractionIterator<Collection,SkipIfRelatedPair>(src, useSquares, SkipIfRelatedPair()), Identity());
}


inline
FeatureStream< CrossProductIterator, Identity >
make_cross_product_stream (std::string const& name, FeatureVector const& slow, FeatureVector const& fast)
{
  debugging::debug("FPRS",2) << "make_interaction_stream (static) " << std::endl;
  return FeatureStream< CrossProductIterator, Identity>
    ("CrossProduct::"+name, CrossProductIterator(slow, fast), Identity());
}


///////////////////////////////////////////////////////////////////////

#include "feature_streams.Template.h"

#endif
