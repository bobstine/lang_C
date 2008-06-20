//  $Id: feature.h,v 3.0 2004/11/19 18:58:36 foster Exp $

#ifndef _FEATURE_H_
#define _FEATURE_H_

/*
  Feature is the envelope for the feature abc within.  Notice that it
  does *not* descend from ABC.  The ABC lives in pointer land.
  Features live in reference land.

  Features take care of these tasks:

    1. Use of the feature in a model, tracking how its been used and
       so forth.

    2. Caching of functions like centering and scaling.

    3. Virtual constructor.
    
  Feature source is a collection of features that must implement the
  function 'features' which returns an object of type 'Vector' that
  supports indexed access.  The type 'Vector' is defined in the
  feature source class.

  --------------------------------------------------------------------

    13 Apr 04 ... Created to isolate these functions.

*/

#include "featureABC.h"
#include "features.h"
#include "column.h"

#include <iostream>
#include <map>
#include <set>

////

class FeatureVector;

class Feature : public EnvelopeABC
{
 public:
  typedef FeatureABC::Range      Range;
  typedef FeatureABC::Arguments  Arguments;
  typedef std::set<std::string>  Attributes;

 private:
  std::string       mName;         // Unique id for this feature
  Attributes        mAttributes;
  bool              mTried;        // Has this feature been tried in the model?
  bool              mInModel;      // Is this feature a predictor in the model?
  double            mEntryPValue;  // What p-value did the feature get?
  FeatureABC*       mFeaturePtr;
  
  static std::map<FeatureABC const*, double> sCenterCache;
  static std::map<FeatureABC const*, double> sScaleCache;

 public:
  virtual ~Feature() { if (mFeaturePtr) delete mFeaturePtr; }

  Feature ()
    : mName(""), mAttributes(), mTried(false), mInModel(false), mEntryPValue(0.0), mFeaturePtr(0) { }

  Feature (FeatureABC* f)
    : mName(f->name()),mAttributes(),mTried(false),mInModel(false),mEntryPValue(0.0),mFeaturePtr(f) { } // caution: deletes ptr

  Feature (Feature const& f)
    : EnvelopeABC(),
      mName(f.mName),mAttributes(f.mAttributes),mTried(f.mTried),mInModel(f.mInModel),mEntryPValue(f.mEntryPValue),
      mFeaturePtr(f.clone_feature_ptr()) { }

  Feature (Column const& c)
    : mName(c.name()), mAttributes(), mTried(false), mInModel(false), mEntryPValue(0.0), mFeaturePtr(0)
    { mFeaturePtr = new ColumnFeature(c); }
  
  Feature (std::istream& is, FeatureVector const& fv)
    : mName(""), mAttributes(), mTried(false), mInModel(false), mEntryPValue(0.0), mFeaturePtr(0)
    {
      mFeaturePtr = make_feature_from_stream(is,fv);
      if (mFeaturePtr){ read_from(is); mName = mFeaturePtr->name(); } else { std::cout << "FETR: Could not create.\n"; }
    }

  bool        has_valid_ptr()                           const { return mFeaturePtr != 0; }
  bool        operator== (Feature const& f)             const { return mName == f.mName; }
  bool        operator<  (Feature const& f)             const { return mName <  f.mName; }

  Attributes  attributes ()                             const { return mAttributes; }
  bool        has_attribute(std::string const& a)       const;
  void        add_attribute(std::string const& s)             { mAttributes.insert(s); }
  
  bool        was_tried_in_model ()                     const { return mTried; }
  bool        is_used_in_model ()                       const { return mInModel; }
  double      entry_p_value ()                          const { return mEntryPValue; }
  void        set_model_results(bool used, double pval)       { std::cout << "FETR: " << name() << " setting results\n";
                                                                        mTried=true; mInModel=used; mEntryPValue=pval; }

  std::string name()                                    const { assert(mFeaturePtr); return mFeaturePtr->name(); }
  std::string operator_name()                           const { assert(mFeaturePtr); return mFeaturePtr->operator_name(); }
  Arguments   arguments()                               const { assert(mFeaturePtr); return mFeaturePtr->arguments(); }
  
  Range       range ()                                  const { assert(mFeaturePtr); return mFeaturePtr->range();}
  double      center ()                                 const;
  double      scale ()                                  const;
  
  double      average()                                 const { assert(mFeaturePtr); return mFeaturePtr->average(); }
  double      std_dev()                                 const { assert(mFeaturePtr); return mFeaturePtr->std_dev(); }
  bool        is_dummy()                                const { assert(mFeaturePtr); return mFeaturePtr->is_dummy(); }

  FeatureABC* clone_feature_ptr()                       const {
    assert(mFeaturePtr);
    std::cout << "HERE " << mFeaturePtr->name() << std::endl ;
    return mFeaturePtr->clone(); }

  void        write_to (std::ostream& os)               const;
  void        print_to (std::ostream& os)               const;
  void        read_from (std::istream& is);

 private:
  FeatureABC* make_feature_from_stream        (std::istream& is, FeatureVector const& fv)  const;
  FeatureABC* make_column_feature_from_stream (std::istream& is, FeatureVector const& fv)  const;
  FeatureABC* make_unary_feature_from_stream  (std::istream& is, FeatureVector const& fv)  const;
  FeatureABC* make_binary_feature_from_stream (std::istream& is, FeatureVector const& fv)  const;
};


template<class Op>
inline
Feature
make_unary_feature(Op const& op, Feature const& f)
{
  return Feature(new UnaryFeature<Op>(op,f));
}

template<class Op>
inline
Feature
make_binary_feature(Op const& op, Feature const& f1, Feature const& f2)
{
  return Feature(new BinaryFeature<Op>(op,f1,f2));
}

inline
Feature
make_interaction_feature(Feature const& f1, Feature const& f2)
{
  return Feature(new InteractionFeature(f1,f2));
}

inline
std::ostream&
operator<< (std::ostream& os, Feature const& feature)
{
  feature.print_to(os);
  return os;
}

inline
std::ostream&
operator<< (std::ostream& os, Feature::Attributes const& attributes)
{
  os << " {";
  std::copy (attributes.begin(), attributes.end(), std::ostream_iterator<std::string>(os," "));
  os << "}";
  return os;
}


////  Feature vector collection  (aka, a feature source)  ////

class FeatureVector
{
 public:
  typedef std::vector<Feature> Vector;            // <-- typename required to be a feature source

 private:
  Vector mFeatures;
  
 public:
  FeatureVector(std::vector<Column> columns)
    : mFeatures() { convert_columns(columns); }

  FeatureVector(std::istream& is, FeatureVector const& source)
    : mFeatures() { read_from(is, source); }

  // required for feature source policy
  int         size()                        const { return mFeatures.size(); }
  Vector      features()                    const { return mFeatures; }       
  Feature     operator[](int j)             const { if (j<(int)mFeatures.size()) return mFeatures[j]; else { Feature f; return f; } }
  Feature     find(std::string const& name) const;
  void        print_to(std::ostream& os)    const;
  
 private:
  void   convert_columns     (std::vector<Column> columns);
  void   read_from           (std::istream& is, FeatureVector const& source);
};

inline
std::ostream&
operator<<(std::ostream& os, FeatureVector const& fv)
{
  fv.print_to(os); return os;
}


#endif

