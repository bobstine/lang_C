// $Id: my_features.h,v 1.12 2004/08/25 22:14:40 bob Exp $

#ifndef _MY_FEATURES_H_
#define _MY_FEATURES_H_

/* 
  Features are basically named ranges, with optional information about
  the use of that data in a model or other application. Features do
  not hold any data, so they are 'lightweight'.  That said, someone
  else must preserve the data pointed to by the feature.

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

  Spline feature

       These are basically a unary function of the data, but one that
       uses a complicated function that we do not want to repeatedly
       copy construct.  The function object is heavy, so it's carried
       as a pointer by the feature.
       
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
#include "smoothing_spline.h"

#include <iostream>


////  Column feature

class ColumnFeature : public FeatureABC
{
  Column mColumn;

 public:
  virtual ~ColumnFeature() {}

  ColumnFeature()
    : FeatureABC(0), mColumn() { }

  ColumnFeature(Column const& c)
    : FeatureABC(c.size()), mColumn(c) { }

  std::string class_name()     const { return "ColumnFeature"; }
  std::string name()           const { return mColumn.name(); }
  std::string operator_name()  const { return ""; }
  Arguments   arguments()      const { return Arguments(); }
  
  Range       range()          const { return make_anonymous_range(mColumn.range()); }
  Column      column()         const { return mColumn; }

  double      average()        const { return mColumn.average(); }
  double      std_dev()        const { return range_stats::standard_deviation(mColumn.range(), mColumn.average(), mColumn.size()); }

  double      center()         const { return average(); }
  double      scale()          const { return std_dev(); }
  bool        is_dummy()       const { return mColumn.is_dummy(); }

  void        write_to(std::ostream& os) const;
};


//  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature

class InteractionFeature : public FeatureABC
{
  FeatureABC const* mFeature1;
  FeatureABC const* mFeature2;
  std::string       mName;

 public:
  virtual ~InteractionFeature() {}
  
  InteractionFeature(FeatureABC const* f1, FeatureABC const* f2)
    : FeatureABC(f1->size()), mFeature1(), mFeature2() {if (f1<f2) {mFeature1=f1; mFeature2=f2;} else {mFeature1=f2; mFeature2=f1;} make_name();  }

  std::string class_name()    const { return "InteractionFeature"; }
  std::string name()          const { return mName; }
  std::string operator_name() const { return "*"; }
  Arguments   arguments()     const;
  
  Range       range()         const { return make_anonymous_range( make_binary_range(std::multiplies<double>(),
										     mFeature1->range(),
										     mFeature2->range())); }
  double      center()        const { return mFeature1->center()*mFeature2->center(); }
  double      scale()         const { return mFeature1->scale()*mFeature2->scale(); }
  bool        is_dummy()      const { return (mFeature1->is_dummy() && mFeature2->is_dummy()) ; }
  
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
  Column*                  mColumn;
  
 public:
  virtual ~LinearCombinationFeature() {}
  
  LinearCombinationFeature(std::vector<double> const& b,
			   std::vector<FeatureABC*> const& fv, Column* column)
    : FeatureABC(column->size()), mBeta(b), mFeatures(fv), mName(), mColumn(column)
    { if (valid_args()) { make_name(); fill_column();} }

  std::string class_name()    const { return "LinearCombinationFeature"; }
  std::string name()          const { return mName; }
  std::string long_name()     const;
  Arguments   arguments()     const { return Arguments();}
  
  Range       range()         const { return make_anonymous_range(mColumn->range()); }
  double      center()        const { return mColumn->average(); }
  double      scale()         const { return mColumn->scale(); }
  bool        is_dummy()      const { return mColumn->is_dummy(); }
  
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
  Op mOp;
  FeatureABC const* mFeature;

 public:
  virtual ~UnaryFeature() {}
  
  UnaryFeature(Op const& op, FeatureABC const* f)
    : FeatureABC(f->size()), mOp(op), mFeature(f) { }

  std::string class_name() const { return "UnaryFeature"; }
  std::string name()       const { return operator_traits<Op>::name() + "[" + mFeature->name() + "]"; }
  Arguments   arguments()  const { return Arguments(); }
  
  Range       range()      const
    { // std::cout << "FETR: Making anonymous range in Unary feature.\n";
      return make_anonymous_range(make_unary_range(mOp,mFeature->range()));
    }

  double      center()     const { return mOp(mFeature->center()); }
  double      scale()      const { return mOp(mFeature->scale()); }

  void        write_to (std::ostream& os) const;
};



////  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature

template<class Op>
class BinaryFeature : public FeatureABC
{
  Op mOp;
  FeatureABC const* mFeature1;
  FeatureABC const* mFeature2;

 public:
  virtual ~BinaryFeature() {}
  
  BinaryFeature(Op const& op, FeatureABC const* f1, FeatureABC const* f2)
    : FeatureABC(f1->size()), mOp(op), mFeature1(f1), mFeature2(f2) { }

  std::string class_name() const { return "BinaryFeature"; }
  std::string name()       const { return operator_traits<Op>::name() + "[ (" + mFeature1->name() + ")" + operator_traits<Op>::symbol()
				+ "(" + mFeature2->name() + ") ]"; }
  Arguments   arguments()  const { return Arguments(); }
  
  Range       range()      const { return make_anonymous_range(make_binary_range(mOp,mFeature1->range(),mFeature2->range())); }

  double      center()     const { return mOp(mFeature1->center(),mFeature2->center()); }
  double      scale()      const { return mOp(mFeature1->scale() ,mFeature2->scale()); }

  void        write_to (std::ostream& os) const;
};

#include "my_features.Template.h"
  
#endif

