// -*- c++ -*-
#ifndef _FEATURE_STREAMS_H_
#define _FEATURE_STREAMS_H_

/*
 *  feature_stream.h
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *
 
 Feature streams implement an abstract protocol and deliver upon request another feature.
 The regulator template wrapper-class enforce the common protocol for checking whether the
 stream is (a) empty and (b) has a non-trivial feature (eg, one that is not a
 constant). Template objects in the feature stream object provide the data source that the
 stream uses to build the supplied features.  These might be a model, a list of variables,
 a file, and so forth.

 Feature streams (along with bidders) are held in experts that participate in the
 auction. The *only* calls that come down from the expert are calls to the method
 
        has_feature()

 This response is handled by the RegulatedStream that sits over the underlying stream.
 The bidder at higher level may ask for the number remaining (number_remaining).  If the
 stream has a feature, then a winning expert will call
 
        pop()
 
 which *must* return a feature vector (or else waste the bid).  Feature streams are a
 revised version of the old recommender classes. The pop() operator of the stream must
        (a) pop off the top element from the stack
        (b) advance indices/whatever keeps track of the status of the stream

	
 Streams should be *lightweight*.  They will be copied heavily in the
 auction.  Basically act as a stack/queue, a type of iterator really.
 The stream itself does *not* hold data.

 Flavors

    Finite           chooses variables from a fixed set of columns
    Fit              builds features depending on state of model (such as just added var)
    Interaction      interactions among features from a source of fixed size
    Feature-product  interactions between a feature and a fixed set (counts down, as vars in model)
    Cross-product    interactions between fixed set of features and a set of increasing size
    Lag              lags of a given feature
    Polynomial       bundle of several powers at once
    Subspace         several variables as a bundle

*/

#include "features.h"

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
  object must implement the three functions empty, current_feature_is_ok and
  increment_position.  The stream will then have the chance to bid in the auction and
  submit a feature to the model.

  The regulated stream provides a 'consistent interface' for all streams without needing
  an abstract base class.  (Works since never have a collection of streams; streams are
  hidden in experts, for which we *do* have an abstract base class.)
*/
  

template<class Stream>
class RegulatedStream: public Stream
{
public:
  RegulatedStream(Stream const& s): Stream(s) {}
  
  bool has_feature (std::vector<Feature> const& used, std::vector<Feature> const& skipped)
  {
    while(!Stream::empty())                               // empty signals to leave stream alone
    { if (Stream::current_feature_is_okay(used,skipped))  // chance to check feature before bidding in context of model or auction
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
  Source        mSource;
  int           mPosition;
  int           mCyclesLeft;
  
public:
  
  FiniteStream(std::string const& name, Source const& src, int cycles)
    :  mName(name), mSource(src), mPosition(0), mCyclesLeft(cycles-1) {  }
  
  std::string             name()         const { return mName; }
  std::string             feature_name() const;
  std::vector<Feature>    pop();
  
  void                    print_to(std::ostream& os)          const;

  int                     number_remaining()                  const { int m = mSource.size(); return m - mPosition + mCyclesLeft*m; }
  
protected:
  bool  empty()                                                                                       const;
  bool  current_feature_is_okay(FeatureVector const& used, FeatureVector const& skipped)   const;
  void  increment_position();
};


template <class Source>
RegulatedStream< FiniteStream<Source> >
make_finite_stream (std::string const& name, Source const& s, int numberCycles)
{
  return RegulatedStream< FiniteStream<Source> >(FiniteStream<Source>(name, s, numberCycles));
}



//  LagStream     LagStream     LagStream     LagStream     LagStream     LagStream     LagStream     LagStream

class LagStream
{
  const std::string  mName;
  const Feature      mFeature;
  const int          mMaxLag;
  const int          mBlockSize;
  int                mLag;
  int                mCyclesLeft;
  
public:
  
  LagStream(std::string const& name, Feature const& f, int maxLag, int blockSize, int cycles)
    :  mName(name), mFeature(f), mMaxLag(maxLag), mBlockSize(blockSize), mLag(0), mCyclesLeft(cycles-1) {  }
  
  std::string       name()         const ;
  std::string       feature_name() const ;
  FeatureVector     pop();
  void              print_to(std::ostream& os)          const;
  int               number_remaining()                  const;
  
protected:
  bool  empty()                                                                           const;
  bool  current_feature_is_okay(FeatureVector const& used, FeatureVector const& skipped)  const;
  void  increment_position()  ;

};


inline
RegulatedStream< LagStream >
make_lag_stream (std::string const& name, Feature const& f, int maxLag, int blockSize,  int numberCycles)
{
  return RegulatedStream< LagStream >(LagStream(name, f, maxLag, blockSize, numberCycles));
}


//  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream

template<class Model>
class FitStream
{
  int                    mCount;           // number of times popped a fitted value (used to name fit values)
  int                    mLastQ;           // used to detect change in model size
  Model          const&  mModel;
  std::string            mSignature;       // prefix for variable names so that it can recognize them
  bool                   mIncreaseDegree;
  Column                 mFit;             // holds fit values from model

public:
  
  FitStream(Model const& model, std::string s)    :  mCount(0), mLastQ(0), mModel(model), mSignature(s), mFit() {  }
  
  std::string             name()           const { return mModel.name(); }
  std::string             feature_name()   const; 
  std::vector<Feature>    pop();

protected:                                 // expert calls these methods following regulated stream protocol, allowing to grab fit
  bool  empty();
  bool  current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const&);
  void  increment_position()                                                                   const { };
};


template <class Model>
RegulatedStream< FitStream<Model> >
make_fit_stream (Model const& m, std::string signature)
{
  return RegulatedStream< FitStream<Model> >(FitStream<Model>(m,signature));
}


//  InteractionStream  InteractionStream  InteractionStream  InteractionStream  InteractionStream  InteractionStream  

template<class Source>
class InteractionStream
{
  
private:
  bool            mUseSquares;
  std::string     mName;
  std::string     mCurrentFeatureName;
  Source          mSource;                     
  int             mPos1, mPos2;
  
public:
  
  InteractionStream(std::string name, Source const& src, bool useSquares)
    : mUseSquares(useSquares), mName(name), mCurrentFeatureName(""), mSource(src), mPos1(0), mPos2(0) { build_current_feature_name(); }
  
  std::string             name()                              const { return mName; }
  
  std::string             feature_name()                      const { return mCurrentFeatureName; }
  std::vector<Feature>    pop();                      
  
  int                     number_remaining()                  const;
  void                    print_to(std::ostream& os)          const { os << " " << mPos1 << " x " << mPos2 << " "; }
   
protected:
  bool  empty ()                            const;
  bool  current_feature_is_okay    (std::vector<Feature> const& used, std::vector<Feature> const& skipped)   const;
  void  increment_position();
private:
  bool  has_feature(std::vector<Feature> const& used, std::vector<Feature> const& skipped);
  void  build_current_feature_name();
};


template <class Source>
RegulatedStream< InteractionStream<Source> >
make_interaction_stream (std::string const& name, Source const& s, bool useSquares)
{
  return RegulatedStream< InteractionStream<Source> >(InteractionStream<Source>(name, s, useSquares));
}



//  FeatureProductStream    FeatureProductStream    FeatureProductStream    FeatureProductStream    FeatureProductStream    

template<class Source>
class FeatureProductStream 
{
  std::string  mName;
  std::string  mCurrentFeatureName;
  Feature      mFeature;
  Source       mSource;              // not a reference; it holds the features... BIG BUG to put in a const&
  int          mPos;
  
public:
    
  FeatureProductStream(std::string name, Feature f, Source const& src)
    : mName(""), mCurrentFeatureName(""), mFeature(f), mSource(src), mPos(mSource.size()-1)  { mName = name + ":" + f->name() + " x Source";
                                                                                               build_current_feature_name(); }
  
  std::string             name()                              const { return mName; }  
  std::string             feature_name()                      const { return mCurrentFeatureName; }
  std::vector<Feature>    pop();
  int                     number_remaining()                  const { return mPos+1; }
  void                    print_to(std::ostream& os)          const { os << "FPST: " << name() << " @ " << mPos << " "; }
  
protected:
  bool  empty()  const;
  bool  current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const& skipped);  
  void  increment_position();
private:
  void  build_current_feature_name();
};


template <class Source>
RegulatedStream< FeatureProductStream<Source> >
make_feature_product_stream (std::string name, Feature f, Source const& Src)
{
  return RegulatedStream< FeatureProductStream<Source> >(FeatureProductStream<Source>(name, f, Src));
}



//  CrossProductStream    CrossProductStream    CrossProductStream    CrossProductStream  

template<class Source1, class Source2>
class CrossProductStream 
{
  std::string     mName;
  std::string     mCurrentFeatureName;
  Source1         mFixedSource;          // fixed number of features
  Source2 const&  mDynSource;            // this one iterates most often (odometer style)
  int             mFixedPos, mDynPos;
  
public:
    
  CrossProductStream(std::string name, Source1 const& fixedSrc, Source2 const& dynSrc)
    : mName(name), mCurrentFeatureName(""), mFixedSource(fixedSrc), mDynSource(dynSrc), mFixedPos(0), mDynPos(0)  { build_current_feature_name(); }
  
  std::string             name()                              const { return mName; }  
  std::string             feature_name()                      const { return mCurrentFeatureName; }
  std::vector<Feature>    pop();     
  
  int                     number_remaining()                  const { return (mFixedSource.size()-mFixedPos)*(mDynSource.size()); }
  void                    print_to(std::ostream& os)          const { os << "SCPS: " << name() << " @ " << mFixedPos << " x " << mDynPos << " "; }
  
protected:
  bool  empty()                             const;
  bool  current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const& skipped);  
  void  increment_position();
private:
  bool  has_feature(std::vector<Feature> const& used, std::vector<Feature> const& skipped);
  void  build_current_feature_name();
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
//               Beware... not yet regulated.

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
  std::string             feature_name() const;
  std::vector<Feature>    pop();
  
  bool has_feature(std::vector<Feature> const&, std::vector<Feature> const&);  

  int                     number_remaining()           const { return (mSource.size()-mPos); }
  void                    print_to(std::ostream& os)   const { os << "PLYS: " << name() << " stream @ " << mPos ; }
  
private:
  void increment_position();
  bool feature_meets_conditions(Feature const& feature) const;


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
//                    std::unary_function<std::vector<Feature>,std::vector<Feature>> Transformation;
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
  std::vector<Feature>     mBundle;
  Pred                     mPredicate;       // hold as object, not as reference
  Trans                    mTransformation;
  bool                     mPopped;          // set true when popped to avoid stack copy
  
public:
    
  BundleStream(std::string name, Source const& src, int bundleSize, Pred pred, Trans trans)  // not const ref to function classes
    : mName(name), mSource(src), mPos(0), mBundleSize(bundleSize), mBundle(), 
      mPredicate(pred), mTransformation(trans), mPopped(false) { }
  
  std::string             name()  const { return mName; }
  
  bool                    has_feature(std::vector<Feature> const& , std::vector<Feature> const& );
  std::string             feature_name()                   const;
  std::vector<Feature>    pop()                              { mPopped=true; return mTransformation(mBundle); }
  
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




class FeatureAcceptancePredicate : public std::unary_function<Feature const&,bool>
{
public:
  bool operator()(Feature const& f) const;
};



//  SubspaceBasis   SubspaceBasis   SubspaceBasis   SubspaceBasis   SubspaceBasis   SubspaceBasis   SubspaceBasis

// This class is a wrapper that converts features into the gsl items and back for doing
// principal components and RKHS.


template<class Method>
class SubspaceBasis: public std::unary_function<std::vector<Feature> const&, std::vector<Feature> >
{
  Method mMethod;
public:
  SubspaceBasis (Method m)                : mMethod(m)          { }
  SubspaceBasis (SubspaceBasis const& pc) : mMethod(pc.mMethod) { }
                                                                                    
  std::vector<Feature> operator()(std::vector<Feature> const& fv) const;
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
