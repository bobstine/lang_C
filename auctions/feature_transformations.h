#ifndef _FEATURE_TRANSFORMATIONS_H_
#define _FEATURE_TRANSFORMATIONS_H_

#include "auction_base_types.h"
#include "column.h"
#include "function_utils.h"
#include "smoothing_spline.h"
#include "features.h"


#include <functional>
#include "debug.h"

/*
  Feature transformations apply an operator that must product a vector of features
  as its results.  Its input is whatever the associated iterator produces.

  The supplied operator must define argument_type, as in a unary_function.

 */

template< class Operator >
class FeatureTransformation
{
 public:
  typedef typename Operator::argument_type argument_type;
  typedef FeatureVector                    result_type;
 private:
  Operator      mOp;
 public:
 FeatureTransformation(Operator op)   : mOp(op) { }
  FeatureVector operator()(argument_type arg)  { return mOp(arg); }
};



//     Identity     Identity     Identity     Identity     Identity     Identity     Identity     Identity     Identity     

class Identity: public std::function<std::vector<Feature> (Feature)>
{
 public:
  FeatureVector operator()(Feature f)        const { FeatureVector fv; fv.push_back(f); return fv; }
};

class VIdentity: public std::function<FeatureVector (FeatureVector)>
{
 public:
  FeatureVector operator()(FeatureVector fv) const { return fv; }
};


//     BeamConstructor     BeamConstructor     BeamConstructor     BeamConstructor     BeamConstructor
//
//   input vectors are the indices into the regression model
//


template<class Auc>
class BeamConstructor: public std::function<FeatureVector (std::pair<std::vector<int>, std::vector<int>>)>
{
  typedef std::vector<int> Beam;
  Auc const& mAuction;
  Beam mBeam1;
  Beam mBeam2;  
 public:
 BeamConstructor(Auc const& auction): mAuction(auction) { }
  FeatureVector operator()(std::pair<Beam,Beam> const& beams) const;
 private:  
  Feature linear_combination(Beam const& beam) const;
};


//     Polynomial     Polynomial     Polynomial     Polynomial     Polynomial     Polynomial     Polynomial     Polynomial

class BuildPolynomialFeatures: public std::function<FeatureVector (Feature)>
{
  unsigned mDegree;
 public:
 BuildPolynomialFeatures(int degree) : mDegree(degree) { }
  FeatureVector operator()(Feature const& f) const;
};


//     Neighborhood     Neighborhood     Neighborhood     Neighborhood     Neighborhood     Neighborhood     Neighborhood

class BuildNeighborhoodFeature: public std::function<FeatureVector (Feature)>
{
  IntegerColumn  mIndexColumn;   // maintained externally, not reference
public:
  BuildNeighborhoodFeature(IntegerColumn const& c) : mIndexColumn(c) {  }
  FeatureVector operator()(Feature const& f) const;
};


//     Product     Product     Product     Product     Product     Product     Product     Product     Product     Product

class BuildProductFeature: public std::function<FeatureVector(Feature)>
{
  Feature mFeature;
public:
  BuildProductFeature(Feature const& f) : mFeature(f) {}
  FeatureVector operator()(Feature f) const;
};


//     Calibration     Calibration     Calibration     Calibration     Calibration     Calibration     Calibration

template< class Model >
class BuildPolynomialCalibrationFeature: public std::function<FeatureVector(Model const*)>
{
  int         mDegree;
  std::string mSignature;  // identifying variable name
  int         mSkip;       // possible offset to allow for lag features
  bool        mTruncate;
 public:
  BuildPolynomialCalibrationFeature (int degree, std::string s, int skip, bool truncate)
    : mDegree(degree), mSignature(s), mSkip (skip), mTruncate(truncate) { }  
  FeatureVector operator()(Model const* pModel) const;
};


template< class Model >
class BuildSplineCalibrationFeature: public std::function<FeatureVector(Model const*)>
{
  int         mDF;
  std::string mSignature;  // identifying variable name
  int         mSkip;       // possible offset to allow for lag features
  bool        mTruncate;
 public:
  BuildSplineCalibrationFeature (int df, std::string s, int skip, bool truncate)
    : mDF(df), mSignature(s), mSkip (skip), mTruncate(truncate) { } 
  FeatureVector operator()(Model const* pModel) const;
};

  
#endif
