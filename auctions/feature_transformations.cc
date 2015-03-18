#include "feature_transformations.Template.h"

//     Polynomial     Polynomial     Polynomial     Polynomial     Polynomial     Polynomial     Polynomial     Polynomial

FeatureVector
BuildPolynomialFeatures::operator()(Feature const& f) const
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


//     Neighborhood     Neighborhood     Neighborhood     Neighborhood     Neighborhood     Neighborhood     Neighborhood

FeatureVector
BuildNeighborhoodFeature::operator()(Feature const& f) const
{
  debugging::debug("NBDS",4) << "BuildNeighborhoodFeature from feature " <<  f->name() << std::endl;
  FeatureVector fv;
  fv.push_back(make_indexed_feature(f,mIndexColumn));
  fv[0]->set_attribute("neighborhood", mIndexColumn->name());
  return fv;
}

//     Product     Product     Product     Product     Product     Product     Product     Product     Product     Product

FeatureVector
BuildProductFeature::operator()(Feature f) const
{
  debugging::debug("FPRS",4) << "BuildProductFeature from " << mFeature->name() << " x " << f->name() << std::endl;
  FeatureVector fv;
  fv.push_back(Feature(mFeature,f));
  return fv;
}


