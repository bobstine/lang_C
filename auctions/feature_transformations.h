#ifndef _FEATURE_TRANSFORMATIONS_H_
#define _FEATURE_TRANSFORMATIONS_H_

#include "column.h"
#include "features.h"
#include "function_utils.h"

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
  
  FeatureVector operator()(argument_type const& arg)  { return mOp(arg); }
};



//     Identity     Identity     Identity     Identity     Identity     Identity     Identity     Identity     Identity     

class Identity: public std::function<FeatureVector (Feature)>
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
inline
std::ostream& operator<<(std::ostream& os, std::vector<int> const& beam)
{
  os << "{";
  for(int i : beam) os << " " << i;
  os << " }";
  return os;
}

template<class Auc>
class BeamConstructor: public std::function<FeatureVector (std::pair<std::vector<int>, std::vector<int>>)>
{
  typedef std::vector<int> Beam;

  Auc const& mAuction;
  Beam mBeam1;
  Beam mBeam2;
  
 public:
  
 BeamConstructor(Auc const& auction): mAuction(auction) { }
  
  FeatureVector operator()(std::pair<Beam,Beam> const& beams) const
  {
    FeatureVector result(1);
    debugging::debug("BEAM",3) << "Beam constructor (fea_tran) building linear combination of beams " << beams.first << " and " << beams.second << std::endl;
    result[0] = Feature(linear_combination(beams.first),
			linear_combination(beams.second)
			);
    return result;
  }
  
 private:
  
  Feature linear_combination(Beam const& beam) const
  {
    FeatureVector const& modelFeatures = mAuction.model_features();
    std::vector<double>  beta          = mAuction.model().beta();
    std::vector<double>  beamCoefs;
    std::vector<Feature> beamFeatures;
    beamCoefs.push_back(0.0);  // linear combination wants an intercept
    for(int i : beam)
    { beamCoefs.push_back(beta[i+1]);
      beamFeatures.push_back(modelFeatures[i]);
    }
    std::string beamName = std::string("Beam_") + beamFeatures[0]->attribute_str_value("stream") + std::string("_") + std::to_string(beam.size());
    return Feature(modelFeatures[0]->size(), beamName, beamCoefs, beamFeatures);
  }

};


//     Polynomial     Polynomial     Polynomial     Polynomial     Polynomial     Polynomial     Polynomial     Polynomial

class BuildPolynomialFeatures: public std::function<FeatureVector (Feature)>
{
 unsigned mDegree;

 public:

 BuildPolynomialFeatures(int degree) : mDegree(degree) { }
 
 FeatureVector operator()(Feature const& f) const
    { 
      debugging::debug("PLYS",4) << "BuildPolynomialFeature subspace from feature " <<  f->name() << std::endl;
      FeatureVector powers;
      if (!f->is_used_in_model())    // include X if not in model
	powers.push_back(f);
      powers.push_back(Feature(Function_Utils::Square(), f));
      if(mDegree>2) 
	powers.push_back(Feature(Function_Utils::Cube(), f));
      for (unsigned j=4; j<=mDegree; ++j)
	powers.push_back(Feature(Function_Utils::Power(j), f));
      return powers;
    }
};


//     Neighborhood     Neighborhood     Neighborhood     Neighborhood     Neighborhood     Neighborhood     Neighborhood

class BuildNeighborhoodFeature: public std::function<FeatureVector (Feature)>
{
  IntegerColumn  mIndexColumn;   // maintained externally, not reference
public:
  BuildNeighborhoodFeature(IntegerColumn const& c) : mIndexColumn(c) {  }

  FeatureVector operator()(Feature const& f) const
    {
      debugging::debug("NBDS",4) << "BuildNeighborhoodFeature from feature " <<  f->name() << std::endl;
      FeatureVector fv;
      fv.push_back(make_indexed_feature(f,mIndexColumn));
      fv[0]->set_attribute("neighborhood", mIndexColumn->name());
      return fv;
    }
};


//     Product     Product     Product     Product     Product     Product     Product     Product     Product     Product

class BuildProductFeature: public std::function<FeatureVector(Feature)>
{
  Feature mFeature;
public:
  BuildProductFeature(Feature const& f) : mFeature(f) {}

  FeatureVector operator()(Feature f) const
    {
      debugging::debug("FPRS",4) << "BuildProductFeature from " << mFeature->name() << " x " << f->name() << std::endl;
      FeatureVector fv;
      fv.push_back(Feature(mFeature,f));
      return fv;
    }
};


//     Calibration     Calibration     Calibration     Calibration     Calibration     Calibration     Calibration

template< class Model >
class BuildCalibrationFeature: public std::function<FeatureVector(Model const*)>
{
  int         mDegree;
  std::string mSignature;  // identifying variable name
  int         mSkip;       // possible offset to allow for lag features
  bool        mTruncate;
  
 public:
  BuildCalibrationFeature (int degree, std::string s, int skip, bool truncate)
    : mDegree(degree), mSignature(s), mSkip (skip), mTruncate(truncate) { } 
  
  FeatureVector operator()(Model const* pModel) const
  { // construct name for features as 'Y_hat_(number of vars)'
    std::ostringstream oss;
    oss << mSignature << pModel->q();
    Column fit = Column(oss.str().c_str(), mSkip + pModel->n_total_cases());     // grab current fit
    // fill skipped values with mean, assuming mean of y is mean of fit
    double *pFit (fit->begin());
    double mean  (pModel->y_bar());
    for(int i=0; i<mSkip; ++i)
      *pFit++ = mean;
    // fill rest with predictions from model, then center the range
    pModel->fill_with_fit(fit->begin() + mSkip, mTruncate);
    debugging::debug("CALB",1) << "Range of fit/predictions (truncate=" << mTruncate << ") returned to calibrator by model is  "
			       << *range_ops::min_element(Ranges::make_range(fit->begin() + mSkip, fit->end())) << "  --  "
			       << *range_ops::max_element(Ranges::make_range(fit->begin() + mSkip, fit->end())) << std::endl;				 
    fit->update();
    std::vector<int> powers;
    for (int j = 2; j <= mDegree; ++j)
      powers.push_back(j);
    debugging::debug("CALB",4) << "BuildCalibrationFeature constructs powers 2-" << mDegree <<" of " << fit->name() << std::endl;
    // centers the column before powering up
    FeatureVector fv (powers_of_column(fit,powers));
    return fv;
  }
};

  
#endif
