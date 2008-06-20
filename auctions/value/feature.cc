// $Id: feature.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

#include "feature.h"

////  

bool
Feature::has_attribute(std::string const& a)       const
{
  return (mAttributes.end() != std::find(mAttributes.begin(), mAttributes.end(), a));
}

////  Creator

FeatureABC*
Feature::make_feature_from_stream(std::istream& is, FeatureVector const& fv) const
{
  FeatureABC* f(0);
  std::string className;
  is >> className;
  if (className ==      "ColumnFeature")
    f = make_column_feature_from_stream(is, fv);
  else if (className == "UnaryFeature")
    f = make_unary_feature_from_stream(is, fv);
  else if (className == "BinaryFeature")
    f = make_binary_feature_from_stream(is, fv);
  else
    std::cout << "FETR: Read error on input stream; class name """ << className << """ not recognized.\n";
  return f;
}

FeatureABC*
Feature::make_column_feature_from_stream (std::istream& is, FeatureVector const& fv)  const
{
  std::string name;
  is >> name;
  Feature f (fv.find(name));
  if (f.mFeaturePtr)
    return new ColumnFeature(* dynamic_cast<ColumnFeature const*>(f.mFeaturePtr));
  else
    std::cout << "FETR: Could not find column feature " << name << " in source.\n";
  return 0;
}


FeatureABC*
Feature::make_unary_feature_from_stream(std::istream& is, FeatureVector const& source) const
{
  char opSymbol;
  is >> opSymbol;
  Feature f1 (make_feature_from_stream(is, source));
  switch (opSymbol)
  {
  case '^':
    int power; is >> power;
    return make_unary_feature_ptr(Function_Utils::Power(power), f1);     break;
  default:
    std::cout << "FETR: Did not recognize token """ << opSymbol << """ parsing unary feature.\n";
  }
  return 0;
}

FeatureABC*
Feature::make_binary_feature_from_stream(std::istream& is, FeatureVector const& source) const
{
  char opSymbol;
  is >> opSymbol;
  Feature f1 (make_feature_from_stream(is, source));
  Feature f2 (make_feature_from_stream(is, source));
  switch (opSymbol)
  {
  case '*':
    return make_binary_feature_ptr(std::multiplies<double>(), f1,f2);      break;
  case '+':
    return make_binary_feature_ptr(std::plus<double>()      , f1,f2);      break;
  default:
    std::cout << "FETR: Did not recognize token """ << opSymbol << """ while parsing binary feature.\n";
  }
  return 0;
}
  
  
////  Cache

std::map<FeatureABC const*, double> Feature::sCenterCache;
std::map<FeatureABC const*, double> Feature::sScaleCache;

double
Feature::center () const
{
  assert(mFeaturePtr);
  if (sCenterCache.count(mFeaturePtr))
    return sCenterCache[mFeaturePtr];
  else // cache it
  { std::cout << "FETR: Caching center of " << mFeaturePtr->name() << std::endl;
    double ave (mFeaturePtr->center());
    sCenterCache[mFeaturePtr] = ave;
    return ave;
  }
}

double
Feature::scale () const
{
  assert(mFeaturePtr);
  if (sScaleCache.count(mFeaturePtr))
    return sScaleCache[mFeaturePtr];
  else // cache it
  { std::cout << "FETR: Caching scale of " << mFeaturePtr->name() << std::endl;
    double s (mFeaturePtr->scale());
    sScaleCache[mFeaturePtr] = s;
    return s;
  }
}

//  IO

void
Feature::write_to (std::ostream& os)     const
{
  assert(mFeaturePtr);
  mFeaturePtr->write_to(os);
  os << "Feature " << mAttributes.size() << " ";
  if (!mAttributes.empty())
    std::copy(mAttributes.begin(), mAttributes.end(), std::ostream_iterator<std::string>(os," "));
  os << mTried << " " << mInModel << " " << mEntryPValue << std::endl;
}

void
Feature::read_from (std::istream& is)
{
  std::string prefix;
  is >> prefix;
  if (prefix != "Feature")
  { std::cout << "FETR: Read prefix did not match ""Feature"", got """ << prefix << """ instead.\n";
    return;
  }
  int attributeCount (0);
  is >> attributeCount;
  while (attributeCount)
  { std::string attribute;
    is >> attribute;
    mAttributes.insert(attribute);
    --attributeCount;
  }
  is >> mTried;
  is >> mInModel;
  is >> mEntryPValue;
}


void
Feature::print_to(std::ostream& os) const
{
  os << mName ;
  if (!mAttributes.empty())
    os << mAttributes;
  if (! mTried)
    os << " (not tried).";
  else
  { os << " (tried; ";
    if (!mInModel)
      os << "not ";
    os << "used; p-value " << mEntryPValue << ").";
  }
  os << "  Values " << range();
}


////  FeatureVector  FeatureVector  FeatureVector  FeatureVector  FeatureVector  FeatureVector  FeatureVector  

void
FeatureVector::print_to(std::ostream& os)    const
{
  os << "Feature Vector with " << size() << " elements: \n    ";
  std::copy(mFeatures.begin(), mFeatures.end(), std::ostream_iterator<Feature>(os,"\n    "));
}    

void
FeatureVector::convert_columns(std::vector<Column> columns)
{
  for(unsigned int i=0; i<columns.size(); ++i)
    mFeatures.push_back(Feature(columns[i]));
}

Feature
FeatureVector::find(std::string const& name) const
{
  for(unsigned int j=0; j<mFeatures.size(); ++j)
    if (mFeatures[j].name() == name)
      return mFeatures[j];
  std::cout << "FETR: Feature " << name << " not found.\n";
  return Feature();
}

void
FeatureVector::read_from (std::istream& is, FeatureVector const& source)
{
  int count(0);
  while (true)
  { 
    Feature f(is,source);
    if (f.has_valid_ptr())
    { ++count;
      mFeatures.push_back(f);
    } else break;
  }
  std::cout << "FETR: Feature vector appended " << count << " features from input stream.\n";
}
