#ifndef _FEATURE_PREDICATES_H_
#define _FEATURE_PREDICATES_H_

#include "features.h"


//  Handy utilities for testing properties of features

namespace FeaturePredicates {
  
  bool           indicators_from_same_parent  (Feature const& f1, Feature const& f2) ;

  template< class Collection >
    bool         found_name_among_features (std::string const& name, Collection const& features, std::string const& description) ;
}


// These are used in streams to skip features meeting certian requirements

class SkipNone
{
public:
  bool operator()(Feature const&)   const     { return false; }
};


class SkipIfInModel
{
public:
  bool operator()(Feature const& f) const     { return f->is_used_in_model(); }
};


class SkipIfDerived
{
public:
  bool operator()(Feature const& f) const;
};


class SkipIfRelated
{
  Feature mFeature;
public:
  SkipIfRelated(Feature const& f) : mFeature(f) {}
  bool operator()(Feature const& f) const { return FeaturePredicates::indicators_from_same_parent(mFeature, f); }
};


#include "feature_predicates.Template.h"

#endif
