#ifndef _FEATURES_H_
#define _FEATURES_H_

/* 
  Feature is a wrapper class that has a pointer to the actual feature.  This works nicely
  since messages that go to the feature get communicated to one thing, rather than many.

  These underlying features are named ranges, with optional information about the use of
  that data in a model or application. Features do *not* hold any data, so they are
  'lightweight'. For example, column features hold a reference to a column, but do not
  new/delete the space.

  Column feature

       These directly reference a column in the underlying data.
       Think of these like the names that JMP puts in the heads
       of the columns in its data file.

  Interaction feature

       The cross product of two other features, which also may
       be interactions as well.

  LinearCombination feature

       Linear combinations are used to represent, for example, the
       fit in a model or other simple linear transformations.

  Unary feature

       These use a lightweight function object to define the range.
       Since the function object is light, no big deal about copying
       the function.

  16 Aug 04 ... Splines as a new type of feature.
  22 Apr 04 ... Linear combination, model features
   5 Apr 04 ... Read/Write to support virtual constructor
   1 Apr 04 ... ABC version
  22 Mar 04 ... Cleaner version 
   9 Mar 04 ... Created to support abstraction of auction.
*/

#include "featureABC.h"
#include "column.h"
#include "range_stats.h"
#include "operator_traits.h"
#include "function_utils.h"

#include "gsl_iterator.h"

#include <gsl/gsl_vector.h>

#include <iostream>
#include <vector>


//template<class Op> class UnaryFeature;


////  Envelope class

class Feature
{
  
 private:
  FeatureABC *mFP;

 public:
  ~Feature() { if(--mFP->mRefCount <= 0) delete mFP; }

  
  // copy
  Feature(Feature const& f)    : mFP(f.mFP)     { ++f.mFP->mRefCount;  }

  //  empty has a null column
  Feature();

  //  column feature
  Feature(Column const &c);
  
  //  interaction feature
  Feature(Feature const& f1, Feature const& f2);

  //  linear combination
  Feature(int n,  std::vector<double> b, std::vector<Feature> const& fv);
   
  //  unary feature
  template<class Op>
    Feature(Op const& op, Feature const &x);

  //  binary feature
  template<class Op>
    Feature(Op const& op, Feature const &x1, Feature const& x2);

  
  Feature&    operator=(Feature const& f);
  FeatureABC* operator->()                 const       { return mFP; }  
};

inline
std::ostream&
operator<< (std::ostream& os, Feature const& feature)
{
  feature->print_to(os);
  return os;
}


inline
std::ostream&
operator<< (std::ostream& os, std::vector<Feature> const& featureVec)
{
  for(std::vector<Feature>::const_iterator i=featureVec.begin(); i!=featureVec.end(); ++i)
    (*i)->print_to(os);
  return os;
}


////  Column feature

class ColumnFeature : public FeatureABC
{
 private:
  Column mColumn;  // store by value ; columns are lightweight with ref-counted pointer

 public:
 ColumnFeature(Column c) : FeatureABC(c->size()), mColumn(c) { add_attributes_from_paired_list(c->description()); }

  std::string class_name()     const { return "ColumnFeature"; }
  std::string name()           const { return mColumn->name(); }
  std::string operator_name()  const { return ""; }
  Arguments   arguments()      const { return Arguments(); }
  
  Column      column()         const { return mColumn; }

  Iterator    begin()          const { return make_anonymous_iterator(mColumn->begin()); }
  Range       range()          const { return make_anonymous_range(mColumn->range()); }
  bool        is_dummy()       const { return mColumn->is_dummy(); }
  bool        is_constant()    const { return (1 == mColumn->num_unique()); }
  double      average()        const { return mColumn->average(); }
  double      center()         const { return mColumn->average(); }
  double      scale()          const { return mColumn->scale(); }  // defaults to range/6

  void        write_to(std::ostream& os) const;
};
  

//  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature

class InteractionFeature : public FeatureABC
{
  Feature      mFeature1;
  Feature      mFeature2;
  std::string  mName;

 public:
  virtual ~InteractionFeature() {}
  
  InteractionFeature(Feature const& f1, Feature const& f2)
    : FeatureABC(f1->size()), mFeature1(f1), mFeature2(f2) { make_name();  }   // names built in using map to define canonical order

  std::string class_name()    const { return "InteractionFeature"; }
  std::string name()          const { return mName; }
  std::string operator_name() const { return "*"; }
  Arguments   arguments()     const;
  
  Iterator    begin()         const { return make_anonymous_iterator(
                                                                     make_binary_iterator(std::multiplies<double>(),
                                                                                          mFeature1->begin(),
                                                                                          mFeature2->begin())); }
  Range       range()         const { return make_anonymous_range (
                                                                   make_binary_range(std::multiplies<double>(),
                                                                                     mFeature1->range(),
                                                                                     mFeature2->range())); }
  double      average()       const { return range_stats::average(range(), size()); }
  double      center()        const { return mFeature1->center()*mFeature2->center(); }
  double      scale()         const { return mFeature1->scale()*mFeature2->scale(); }
  bool        is_dummy()      const { return (mFeature1->is_dummy() && mFeature2->is_dummy()) ; }
  bool        is_constant()   const { return (mFeature1->is_constant() && mFeature2->is_constant()); }
  void        write_to (std::ostream& os) const;

 private:
  void        make_name();
};


//  LinearCombinationFeature  LinearCombinationFeature  LinearCombinationFeature  LinearCombinationFeature  

class LinearCombinationFeature : public FeatureABC
{
  std::vector<double>  mBeta;      // b[0] is constant, as in regr
  std::vector<Feature> mFeatures;
  std::string          mName;
  Column               mColumn;    // place to store the lin comb
  
 public:
  virtual ~LinearCombinationFeature() {}
  
 LinearCombinationFeature(int n, std::vector<double> const& b, std::vector<Feature> const& fv)
    :                                                      
    FeatureABC(n), 
      mBeta(b), mFeatures(fv), mName(), mColumn(Column("Linear Comb", n)) { if (valid_args()) { make_name(); fill_column();} }

  std::string class_name()    const { return "LinearCombinationFeature"; }
  std::string name()          const { return mName; }
  std::string long_name()     const;
  Arguments   arguments()     const { return Arguments();}
  
  Iterator    begin()         const { return make_anonymous_iterator(mColumn->begin()); }
  Range       range()         const { return make_anonymous_range(mColumn->range()); }
  double      average()       const { return mColumn->average(); }
  double      center()        const { return mColumn->average(); }
  double      scale()         const { return mColumn->scale(); }
  bool        is_dummy()      const { return mColumn->is_dummy(); }
  bool        is_constant()   const { return (1 == mColumn->num_unique()); }
  void        write_to (std::ostream& os) const;

 private:
  bool        valid_args()    const ;
  void        make_name();
  void        fill_column();
};




////  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  

template<class Op>
class UnaryFeature : public FeatureABC
{
  Op             mOp;
  Feature        mFeature;
  double         mMean;  // cache

 public:
  virtual ~UnaryFeature() {}
  
  UnaryFeature(Op const& op, Feature const& f)
    : FeatureABC(f->size()), mOp(op), mFeature(f), mMean(range_stats::average(range(), size())) { }

  std::string class_name() const { return "UnaryFeature"; }
  std::string name()       const { return operator_traits<Op>::name() + "[" + mFeature->name() + "]"; }
  Arguments   arguments()  const { return Arguments(); }
  
  Iterator    begin()      const { return make_anonymous_iterator(make_unary_iterator(mOp,mFeature->begin()));    }
  Range       range()      const { return make_anonymous_range(make_unary_range(mOp,mFeature->range()));    }
  
  bool        is_dummy()   const { return mFeature->is_dummy(); }
  bool        is_constant()const { return mFeature->is_constant(); }
  double      average()    const { return mMean; }
  double      center()     const { return mMean; }
  double      scale()      const { return range_stats::standard_deviation(range(),mMean,size()); }
  
  void        write_to (std::ostream& os) const;
};


std::vector<Feature>
powers_of_column_feature (Column const& col, std::vector<int> const& powers);
  


////  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature

template<class Op>
class BinaryFeature : public FeatureABC
{
  Op mOp;
  Feature   mFeature1;
  Feature   mFeature2;

 public:  
  BinaryFeature(Op const& op, Feature const& f1, Feature const& f2)
    : FeatureABC(f1->size()), mOp(op), mFeature1(f1), mFeature2(f2) { }

  std::string class_name()  const { return "BinaryFeature"; }
  std::string name()        const { return operator_traits<Op>::name() + "[ (" + mFeature1->name() + ")" + operator_traits<Op>::symbol() + "(" + mFeature2->name() + ") ]"; }
  Arguments   arguments()   const { return Arguments(); }
  
  Iterator    begin()       const { return make_anonymous_iterator(make_binary_iterator(mOp,mFeature1->begin(),mFeature2->begin())); }
  Range       range()       const { return make_anonymous_range(make_binary_range(mOp,mFeature1->range(),mFeature2->range())); }

  bool        is_dummy()    const { return (mFeature1->is_dummy() && mFeature2->is_dummy()); }
  bool        is_constant() const { return (mFeature1->is_constant() && mFeature2->is_constant()); }
  double      average()    const  { return range_stats::average(range(), size()); }
  double      center()      const { return mOp(mFeature1->center(),mFeature2->center()); }
  double      scale()       const { return mOp(mFeature1->scale() ,mFeature2->scale()); }

  void        write_to (std::ostream& os) const;
};


#include "features.Template.h"
  
#endif

