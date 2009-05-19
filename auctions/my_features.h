// $Id: my_features.h,v 3.9 2008/01/31 23:31:03 bob Exp $

#ifndef _MY_FEATURES_H_
#define _MY_FEATURES_H_

/* 
  Features are named ranges, with optional information about the use of that data in a
  model or application. Features do *not* hold any data, so they are 'lightweight'. For
  example, column features hold a reference to a column, but do not new/delete the space.

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

namespace Features {
  typedef FeatureABC*               Feature;
  typedef std::vector<FeatureABC *> FeatureVector;
}

////  Column feature

class ColumnFeature : public FeatureABC
{
 private:
  Column mColumn;  // store by value ; columns are lightweight with ref-counted pointer

 public:
  ColumnFeature(Column c) : FeatureABC(c.size()), mColumn(c) { }

  std::string class_name()     const { return "ColumnFeature"; }
  std::string name()           const { return mColumn.name(); }
  std::string operator_name()  const { return ""; }
  Arguments   arguments()      const { return Arguments(); }
  
  Column      column()         const { return mColumn; }

  Iterator    begin()          const { return make_anonymous_iterator(mColumn.begin()); }
  Range       range()          const { return make_anonymous_range(mColumn.range()); }
  bool        is_dummy()       const { return mColumn.is_dummy(); }
  bool        is_constant()    const { return (1 == mColumn.unique()); }
  double      average()        const { return mColumn.average(); }
  double      center()         const { return mColumn.average(); }
  double      scale()          const { return mColumn.scale(); }  // defaults to range/6

  void        write_to(std::ostream& os) const;
};
  

////  gslVector feature

class gslVectorFeature : public FeatureABC
{
  gsl_vector const* mVector;
  std::string       mName;
  int               mUnique;
  double            mAvg;
  double            mMin, mMax;
  
public:
  
  gslVectorFeature(std::string name, gsl_vector const* v)
    : FeatureABC(v->size), mVector(v), mName(name), mUnique(0), mAvg(0.0), mMin(0.0), mMax(0.0) { initialize(); }
  
  std::string class_name()     const { return "gslVectorFeature"; }
  std::string name()           const { return mName; }
  std::string operator_name()  const { return ""; }
  Arguments   arguments()      const { return Arguments(); }
  
  gsl_vector  const* vector()  const { return mVector; }
  
  Iterator    begin()          const { return make_anonymous_iterator(GSL::begin(mVector)); }
  Range       range()          const { return make_anonymous_range(make_range(GSL::begin(mVector), GSL::end(mVector))); }
  bool        is_dummy()       const { return (2 == mUnique); }
  bool        is_constant()    const { return (1 == mUnique); }
  double      average()        const { return mAvg; }
  double      center()         const { return mAvg; }
  double      scale()          const;
  
  void        write_to(std::ostream& os) const;
  
private:
  void initialize();
};



//  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature

class InteractionFeature : public FeatureABC
{
  FeatureABC  const* mFeature1;
  FeatureABC  const* mFeature2;
  std::string       mName;

 public:
  virtual ~InteractionFeature() {}
  
  InteractionFeature(FeatureABC const* f1, FeatureABC const* f2)
    : FeatureABC(f1->size()), mFeature1(f1), mFeature2(f2) { make_name();  }   // names built in map with canonical order

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
  std::vector<double>      mBeta;      // b[0] is constant, as in regr
  std::vector<FeatureABC*> mFeatures;
  std::string              mName;
  Column                   mColumn;
  
 public:
  virtual ~LinearCombinationFeature() {}
  
 LinearCombinationFeature(int n, std::vector<double> const& b, std::vector<FeatureABC*> const& fv)
    :                                                      // uses the column as a place to store the lin comb
    FeatureABC(n), 
      mBeta(b), mFeatures(fv), mName(), mColumn(Column("Linear Comb", n)) { if (valid_args()) { make_name(); fill_column();} }

  std::string class_name()    const { return "LinearCombinationFeature"; }
  std::string name()          const { return mName; }
  std::string long_name()     const;
  Arguments   arguments()     const { return Arguments();}
  
  Iterator    begin()         const { return make_anonymous_iterator(mColumn.begin()); }
  Range       range()         const { return make_anonymous_range(mColumn.range()); }
  double      average()       const { return mColumn.average(); }
  double      center()        const { return mColumn.average(); }
  double      scale()         const { return mColumn.scale(); }
  bool        is_dummy()      const { return mColumn.is_dummy(); }
  bool        is_constant()   const { return (1 == mColumn.unique()); }
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
  Op                mOp;
  FeatureABC const* mFeature;
  double         mMean;  // cache

 public:
  virtual ~UnaryFeature() {}
  
  UnaryFeature(Op const& op, FeatureABC const* f)
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

template<class Op>
FeatureABC*
make_unary_feature(Op const& op, FeatureABC const* f)
{
  return new UnaryFeature<Op>(op, f);
}


Features::FeatureVector
powers_of_column_feature (Column const& col, std::vector<int> const& powers);
  


////  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature

template<class Op>
class BinaryFeature : public FeatureABC
{
  Op mOp;
  FeatureABC  const* mFeature1;
  FeatureABC  const* mFeature2;

 public:  
  BinaryFeature(Op const& op, FeatureABC const* f1, FeatureABC const* f2)
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

#include "my_features.Template.h"
  
#endif

