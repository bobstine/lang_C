// -*- c++ -*-
// $Id: feature_streams.h,v 1.18 2008/02/22 19:39:47 bob Exp $

#ifndef _FEATURE_STREAMS_H_
#define _FEATURE_STREAMS_H_

/*
 *  feature_stream.h
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *
 
 Streams build the features that go into the model.  Regulated
 feature streams enforce a set of checks that the features that are
 offered are reasonable for the problem (eg, not constants, not
 already in the model and so forth).

 The *only* calls that come down from the expert are calls to
 
        has_feature()

 The bidder at higher level may ask for the number remaining.
 
	number_remaining()
 
 which if true and the bid wins is followed by a call to 
 
        pop()
 
 which *must* return a feature vector (or else waste the bid).
 Feature streams are a revised version of the old recommender
 classes. The pop() operator of the stream must
        (a) pop off the top element from the stack
        (b) advance indices/whatever keeps track of the status of the stream

	
 Streams should be *lightweight*.  They will be copied heavily in the
 auction.  Basically act as a stack/queue, a type of iterator really.
 The stream itself does not hold data.

 Flavors

    Finite        chooses variables from a fixed set of columns

    Interaction   interactions from a fixed set

    Cross-product interactions between dynamic and fixed set

    Polynomial    bundle of several powers at once
    
    Subspace      several variables as a bundle

    Fitted-values filled by some external object
 
 */

#include "my_features.h"

// polynomial
#include "function_utils.h"

// principal components
#include "gsl_eigen.h"
#include <gsl/gsl_matrix.h>

// operator
#include <functional>

#include <iostream>
#include <sstream>

//  Regulated Stream    Regulated Stream    Regulated Stream    Regulated Stream    Regulated Stream 

/*
  A regulated stream injects the method 'has_feature' into a stream object.  That stream
  object must implement the three functions 'is_empty, current_feature_is_ok and
  increment_position.  The stream will then have the chance to bid in the auction and
  submit a feature to the model.

  The regulated stream provides a 'consistent interface' for all streams without needing
  an abstract base class.  (Works since never have a collection of streams.)
*/
  

template<class Stream>
class RegulatedStream: public Stream
{
public:
  RegulatedStream(Stream const& s): Stream(s) {}
  
  bool has_feature (Features::FeatureVector const& used, Features::FeatureVector const& skipped)
  {
    while(!Stream::is_empty())
    { if (Stream::current_feature_is_okay(used,skipped))
	return true;
      else
	Stream::increment_position();
    }
    return false;
  }
};



//  FiniteStream  FiniteStream  FiniteStream  FiniteStream  FiniteStream  FiniteStream

template<class Source>
class FiniteStream
{
  std::string   mName;
  Source const& mSource;
  int           mPosition;
  
public:
  
  FiniteStream(std::string const& name, Source const& src)
    :  mName(name), mSource(src), mPosition(0) {  }
  
  std::string             name()         const { return mName; }
  std::string             feature_name() const;
  Features::FeatureVector pop();
  
  void                    print_to(std::ostream& os)          const;

  int                     number_remaining()                  const { return (int)mSource.size() - mPosition; }
  
protected:
  bool  is_empty()                                                                                             const;
  bool  current_feature_is_okay(Features::FeatureVector const& used, Features::FeatureVector const& skipped)   const;
  void  increment_position();
};


template <class Source>
RegulatedStream< FiniteStream<Source> >
make_finite_stream (std::string const& name, Source const& s)
{
  return RegulatedStream< FiniteStream<Source> >(FiniteStream<Source>(name, s));
}



//  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream

template<class Model>
class FitStream
{
  int                    mCount;    // Number of times popped a fitted value (used to name fit values)
  Model          const&  mModel;
  
public:
  
  FitStream(Model const& model)    :  mCount(0), mModel(model) {  }
  
  std::string             name()           const { return mModel.name(); }
  std::string             feature_name()   const;
  Features::FeatureVector pop();

protected:  // bidder decides whether to use the fit of the model
  bool  is_empty()                                                                                const { return false; }
  bool  current_feature_is_okay(Features::FeatureVector const&, Features::FeatureVector const&)   const { return true; }
  void  increment_position()                                                                      const { };
};


template <class Model>
RegulatedStream< FitStream<Model> >
make_fit_stream (Model const& m)
{
  return RegulatedStream< FitStream<Model> >(FitStream<Model>(m));
}


//  InteractionStream  InteractionStream  InteractionStream  InteractionStream  InteractionStream  InteractionStream  

template<class Source>
class InteractionStream
{
  
private:
  bool            mUseSquares;
  std::string     mName;
  std::string     mCurrentFeatureName;
  Source const&   mSource;
  int             mPos1, mPos2;
  
public:
  
  InteractionStream(std::string name, Source const& src, bool useSquares)
    : mUseSquares(useSquares), mName(name), mCurrentFeatureName(""), mSource(src), mPos1(0), mPos2(0) { build_current_feature_name(); }
  
  std::string             name()                              const { return mName; }
  
  bool                    has_feature(Features::FeatureVector const& used, Features::FeatureVector const& skipped);
  std::string             feature_name()                      const { return mCurrentFeatureName; }
  Features::FeatureVector pop();                      
  
  int                     number_remaining()                  const;
  void                    print_to(std::ostream& os)          const { os << " " << mPos1 << " x " << mPos2 << " "; }
   
protected:
  bool  is_empty()                  const;
  bool  current_feature_is_okay(Features::FeatureVector const& used, Features::FeatureVector const& skipped)   const;
  void  increment_position();
private:
  void  build_current_feature_name();
};

template <class Source>
RegulatedStream< InteractionStream<Source> >
make_interaction_stream (std::string const& name, Source const& s, bool useSquares)
{
  return RegulatedStream< InteractionStream<Source> >(InteractionStream<Source>(name, s, useSquares));
}



//  CrossProductStream    CrossProductStream    CrossProductStream    CrossProductStream  

template<class Source1, class Source2>
class CrossProductStream 
{
  std::string     mName;
  std::string     mCurrentFeatureName;
  Source1 const&  mFixedSource;  // fixed number of features
  Source2 const&  mDynSource;    // this one iterates most often (odometer style)
  int             mFixedPos, mDynPos;
  
public:
    
  CrossProductStream(std::string name, Source1 const& fixedSrc, Source2 const& dynSrc)
    : mName(name), mCurrentFeatureName(""), mFixedSource(fixedSrc), mDynSource(dynSrc), mFixedPos(0), mDynPos(0)  { build_current_feature_name(); }
  
  std::string             name()                              const { return mName; }
  
  bool                    has_feature(Features::FeatureVector const& used, Features::FeatureVector const& skipped);
  std::string             feature_name()                      const { return mCurrentFeatureName; }
  Features::FeatureVector pop();                      
  
  int                     number_remaining()                  const { return (mFixedSource.size()-mFixedPos)*(mDynSource.size()); }
  void                    print_to(std::ostream& os)          const { os << "SCPS: " << name() << " @ " << mFixedPos << " x " << mDynPos << " "; }
  
protected:
  bool  is_empty() const;
  bool  current_feature_is_okay(Features::FeatureVector const& used, Features::FeatureVector const& skipped);  
  void  increment_position();
private:
  void build_current_feature_name();
};


template <class Source1, class Source2>
RegulatedStream< CrossProductStream<Source1, Source2> >
make_cross_product_stream (std::string const& name, Source1 const& fixedSrc, Source2 const& dynSrc)
{
  return RegulatedStream< CrossProductStream<Source1, Source2> >(CrossProductStream<Source1,Source2>(name, fixedSrc, dynSrc));
}



//  PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream
//
//               This stream forms polynomials from column features 
//               identified in a dynamically growing list. 
//               Canonical example of a stream that builds several features
//               from a subset of features in a different stream.
//

template<class Source>
class PolynomialStream 
{
  std::string     mName;
  Source const&   mSource;  
  int             mPos;
  int             mDegree;
  
public:
    
  PolynomialStream(std::string name, Source const& src, int degree)
    : mName(name), mSource(src), mPos(0), mDegree(degree) { }
  
  std::string name()      const { return mName; }
  
  bool                    has_feature(Features::FeatureVector const&, Features::FeatureVector const&);
  std::string             feature_name() const;
  Features::FeatureVector pop();                      
  
  int                     number_remaining()           const { return (mSource.size()-mPos); }
  void                    print_to(std::ostream& os)   const { os << "PLYS: " << name() << " stream @ " << mPos ; }
  
private:
  void increment_position();
  bool feature_meets_conditions(FeatureABC const* feature) const;
};

template <class Source>
inline
PolynomialStream<Source>
make_polynomial_stream (std::string const& name, Source const& src, int degree = 3)
{
  return PolynomialStream<Source>(name, src, degree);
}




//  BundleStream   BundleStream   BundleStream   BundleStream   BundleStream   BundleStream   BundleStream
//
//               This stream transforms a "bundle" of features identified 
//               in a source, in this case into a subset of eigenvectors.
//
//               The stream waits until it obtains a bundle that satisfies the input
//               predicate of the indicated size, the applies a transformation.
//               These classes should act like these:
//                    std::unary_function<Features::FeatureVector,Features::FeatureVector> Transformation;
//                    std::unary_function<FeatureABC const*, bool>                         Predicate;


template<class Source, class Pred, class Trans>
class BundleStream 
{
public:
  
private:
  std::string              mName;
  Source const&            mSource;  
  int                      mPos; 
  int                      mBundleSize;
  Features::FeatureVector  mBundle;
  Pred                     mPredicate;       // hold as object, not as reference
  Trans                    mTransformation;
  bool                     mPopped;          // set true when popped to avoid stack copy
  
public:
    
  BundleStream(std::string name, Source const& src, int bundleSize, Pred pred, Trans trans)  // not const ref to function classes
    : mName(name), mSource(src), mPos(0), mBundleSize(bundleSize), mBundle(), 
      mPredicate(pred), mTransformation(trans), mPopped(false) { }
  
  std::string             name()  const { return mName; }
  
  bool                    has_feature(Features::FeatureVector const& , Features::FeatureVector const& );
  std::string             feature_name()               const;
  Features::FeatureVector pop()                              { mPopped=true; return mTransformation(mBundle); }
  
  int                     number_remaining()           const { return (mSource.size()-mPos); }
  void                    print_to(std::ostream& os)   const { os << "BDLS: " << name() << " stream @ " << mPos ; }
};

template <class Source, class Pred, class Trans>
  inline 
  BundleStream<Source,Pred,Trans>
  make_bundle_stream (std::string const& name, Source const& src, int bundleSize, Pred pred, Trans trans)
{
  return BundleStream<Source,Pred,Trans>(name, src, bundleSize, pred, trans);
}




class FeatureAcceptancePredicate : public std::unary_function<FeatureABC const*,bool>
{
public:
  bool operator()(FeatureABC const* f) const;
};



//  SubspaceBasis   SubspaceBasis   SubspaceBasis   SubspaceBasis   SubspaceBasis   SubspaceBasis   SubspaceBasis

// This class is a wrapper that converts features into the gsl items and back for doing
// principal components and RKHS.


template<class Method>
class SubspaceBasis: public std::unary_function<Features::FeatureVector const&, Features::FeatureVector>
{
  Method mMethod;
public:
  SubspaceBasis (Method m)                : mMethod(m)          { }
  SubspaceBasis (SubspaceBasis const& pc) : mMethod(pc.mMethod) { }
                                                                                    
  Features::FeatureVector operator()(Features::FeatureVector const& fv) const;
};


template <class Source, class Method>
BundleStream<Source,FeatureAcceptancePredicate,SubspaceBasis<Method> >
make_subspace_stream(std::string name, Source const& src, int bundleSize, Method method)
{
  return make_bundle_stream(name, src, bundleSize, 
                            FeatureAcceptancePredicate(), 
                            SubspaceBasis<Method>(method));
}





///////////////////////////////////////////////////////////////////////

#include "feature_streams.Template.h"

#endif
