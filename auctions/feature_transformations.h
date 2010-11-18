#ifndef _FEATURE_TRANSFORMATIONS_H_
#define _FEATURE_TRANSFORMATIONS_H_

#include "column.h"
#include "features.h"


/*
  Feature transformations apply an operator that must product a vector of features
  as its results.  Its input is whatever the associated iterator produces.

  The supplied operator must define argument_type, as in a unary_function.

 */

template< class Operator >
class FeatureTransformation
{
  typedef typename Operator::argument_type ArgType;
  
  ArgType       mInput;
  FeatureVector mFV;

 public:
  void          input(ArgType x)                 { mInput = x; }
  bool          empty()                const     { return mFV.empty(); }
  FeatureVector output_features()      const     { return mFV; }

  void operator()()
  {
    std::cout << "TRANS: apply transformation\n";
    mFV = Operator()(mInput);
    std::cout << "TRANS: yields " << mFV.size() << " features;  fv[0] = " << mFV[0]->name() << std::endl;
  }
};


class WrapFeature: public std::unary_function<Feature, FeatureVector>
{
 public:
  FeatureVector operator()(Feature f) const { FeatureVector fv; fv.push_back(f); return fv; }
};


typedef FeatureTransformation<WrapFeature> Identity;


/*

class BuildNeighborhoodFeature
{
  IntegerColumn  mIndexColumn;   // maintained externally, not reference
public:
  BuildNeighborhoodFeature(IntegerColumn const& c) : mIndexColumn(c) {  }

  FeatureVector operator()(Feature const& f) const
    {
      debugging::debug("NBDS",4) << "BuildNeighborhoodFeature from feature " <<  f->name() << std::endl;
      FeatureVector fv;
      fv.push_back(make_indexed_feature(f,mIndexColumn));
      fv[0]->add_attribute("neighborhood", mIndexColumn->name());
      return fv;
    }
};


class BuildPolynomialFeature
{
  int mDegree;
public:
  BuildPolynomialFeature(int degree) : mDegree(degree) { }
  
  FeatureVector operator()(Feature const& f) const
    { 
      debugging::debug("PLYS",4) << "BuildPolynomialFeature subspace from feature " <<  f->name() << std::endl;
      FeatureVector powers;
      if (!f->is_used_in_model())    // include X if not in model
	powers.push_back(f);
      powers.push_back(Feature(Function_Utils::Square(), f));
      if(mDegree>2) 
	powers.push_back(Feature(Function_Utils::Cube(), f));
      for (int j=4; j<=mDegree; ++j)
	powers.push_back(Feature(Function_Utils::Power(j), f));
      return powers;
    }
};


class BuildProductFeature
{
  Feature mFeature;
public:
  BuildProductFeature(Feature const& f) : mFeature(f) {}

  FeatureVector operator()(Feature const& f) const
    {
      debugging::debug("FPRS",4) << "BuildProductFeature from " << mFeature->name() << " x " << f->name() << std::endl;
      FeatureVector fv;
      fv.push_back(Feature(mFeature,f));
      return fv;
    }
};


template< class Model >
class BuildCalibrationFeature
{
  int mDegree;
  int mSkip; // offset for features

 public:
  BuildCalibrationFeature (int degree,int skip) : mDegree(degree), mSkip (skip) { } 

  FeatureVector operator()(Model const& model) const
  {
    // construct name for features as 'Y_hat_x'
    std::ostringstream oss;
    oss << "Y_hat_" << model.q();
    Column mFit = Column(oss.str().c_str(), mSkip + model.n_total_cases());     // grab current fit
    double *fit (mFit->begin());
    for(int i=0; i<mSkip; ++i)
      *fit++ = 0;
    model.fill_with_fit(mFit->begin() + mSkip);
    mFit->update();
    std::vector<int> powers;
    for (int j = 2; j <= mDegree; ++j)
      powers.push_back(j);
    debugging::debug("FSTR",4) << "BuildCalibrationFeature constructs powers 2-" << mDegree <<" of " << mFit->name() << std::endl;
    return powers_of_column_feature(mFit,powers);
  }
};
*/
  
#endif
