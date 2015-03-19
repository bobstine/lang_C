#ifndef _FEATURE_TRANSFORMATIONS_TEMPLATE_H_
#define _FEATURE_TRANSFORMATIONS_TEMPLATE_H_

#include "feature_transformations.h"
#include "features.Template.h"

//     Beam     Beam     Beam     Beam     Beam     Beam     Beam     Beam     Beam     Beam     Beam     Beam     Beam

inline
std::ostream& operator<<(std::ostream& os, std::vector<int> const& beam)
{
  os << "{";
  for(int i : beam) os << " " << i;
  os << " }";
  return os;
}

template<class Auc>
FeatureVector
BeamConstructor<Auc>::operator()(std::pair<Beam,Beam> const& beams) const
{
  FeatureVector result(1);
  debugging::debug("BEAM",3) << "Beam constructor (fea_tran) building linear combination of beams " << beams.first << " and " << beams.second << std::endl;
  result[0] = Feature(linear_combination(beams.first),
		      linear_combination(beams.second)
		      );
  return result;
 }

template<class Auc>
Feature
BeamConstructor<Auc>::linear_combination(Beam const& beam) const
{
  FeatureVector const& modelFeatures = mAuction.model_features();
  std::vector<SCALAR>  beta          = mAuction.model().beta();
  std::vector<SCALAR>  beamCoefs;
  std::vector<Feature> beamFeatures;
  beamCoefs.push_back(0.0);  // linear combination wants an intercept
  for(int i : beam)
  { beamCoefs.push_back(beta[i+1]);
    beamFeatures.push_back(modelFeatures[i]);
  }
  std::string beamName = std::string("Beam_") + beamFeatures[0]->attribute_str_value("stream") + std::string("_") + std::to_string(beam.size());
  return Feature(modelFeatures[0]->size(), beamName, beamCoefs, beamFeatures);
}


template<class Model>
FeatureVector
BuildPolynomialCalibrationFeature<Model>::operator()(Model const* pModel) const
{ // construct name for features as 'Y_hat_(number of vars)'
  std::ostringstream oss;
  oss << mSignature << pModel->q();
  Column<SCALAR> fit = Column<SCALAR>(oss.str().c_str(), mSkip + pModel->n_total_cases());     // grab current fit
  // fill skipped values with mean, assuming mean of y is mean of fit
  SCALAR *pFit (fit->begin());
  SCALAR mean  (pModel->y_bar());
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
  debugging::debug("CALB",4) << "BuildPolynomialCalibrationFeature constructs powers 2-" << mDegree <<" of " << fit->name() << std::endl;
  // centers the column before powering up
  FeatureVector fv (powers_of_column(fit,powers));
  return fv;
}


template<class Model>
FeatureVector
BuildSplineCalibrationFeature<Model>::operator()(Model const* pModel) const
{ // construct name for features as 'Spl(number of vars)'
  debugging::debug("CALB",2) << "BuildSplineCalibrationFeature constructs spline with " << mDF << " df." << std::endl;
  std::ostringstream oss;
  oss << mSignature << pModel->q();
  Column<SCALAR> fit = Column<SCALAR>(oss.str().c_str(), mSkip + pModel->n_total_cases());
  Column<SCALAR> res = Column<SCALAR>(oss.str().c_str(), mSkip + pModel->n_total_cases());
  // fill skipped res with zero and skipped fit with mean
  SCALAR *pFit (fit->begin());
  SCALAR *pRes (res->begin());
  for(int i=0; i<mSkip; ++i)
    { *pRes++ = (SCALAR) 0.0;
      *pFit++ = pModel->y_bar();
    }
  // fill rest with spline fit to residuals from model
  const bool truncate = false;
  pModel->fill_with_fit      (fit->begin() + mSkip, truncate);
  pModel->fill_with_residuals(res->begin() + mSkip);
  debugging::debug("CALB",3) << "Range of residuals/pred errors for spline is "
			     << *range_ops::min_element(Ranges::make_range(res->begin() + mSkip, res->end())) << "  --  "
			     << *range_ops::max_element(Ranges::make_range(res->begin() + mSkip, res->end())) << std::endl;				 
  fit->update();
  res->update();
  SmoothingSpline ss(mDF, fit->begin()+mSkip, res->begin()+mSkip, pModel->n_estimation_cases());  // estimate on these
  FeatureVector fv(1);
  fv[0] = Feature(ss.spline_operator(), fit);   // build full fit
  return fv;
}




#endif
