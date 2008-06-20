// $Id: features.h,v 3.0 2004/11/19 18:58:36 foster Exp $

#ifndef _FEATURES_H_
#define _FEATURES_H_

/* 
  Features are basically named ranges. Features do not hold any data,
  so they are 'lightweight'.  That said, someone else must preserve
  the data pointed to by the feature.

  12 Apr 04 ... Unary and binary features, envelope/letter virtual creator
   5 Apr 04 ... Read/Write to support virtual constructor
   1 Apr 04 ... ABC version
  22 Mar 04 ... Cleaner version 
   9 Mar 04 ... Created to support abstraction of auction.
*/

#include "featureABC.h"
#include "column.h"
#include "range_stats.h"

#include <iostream>
#include <set>



////  Operator_traits  Operator_traits  Operator_traits  Operator_traits  Operator_traits  Operator_traits  

template<class Op>
class operator_traits
{
 public:
  typedef Op operator_type;

  static std::string name();
  static char        symbol();
};


////  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  

class ColumnFeature : public FeatureABC
{
  Column mColumn;

 public:
  virtual ~ColumnFeature() {}

  ColumnFeature()
    : FeatureABC(), mColumn() { }

  ColumnFeature(ColumnFeature const& c)
    : FeatureABC(), mColumn(c.mColumn) { }
  
  ColumnFeature(Column const& c)
    : FeatureABC(), mColumn(c) { }

  FeatureABC* clone()          const { return new ColumnFeature(mColumn); }

  std::string name()           const { return mColumn.name(); }
  std::string operator_name()  const { return ""; }
  Arguments   arguments()      const { return Arguments(); }
  
  Range       range()          const { return make_anonymous_range(mColumn.range()); }

  double      average()        const { return mColumn.average(); }
  double      std_dev()        const { return range_stats::standard_deviation(mColumn.range(), mColumn.average(), mColumn.size()); }
  double      center()         const { return average(); }
  double      scale()          const { return std_dev(); }
  bool        is_dummy()       const { return mColumn.is_dummy(); }

  void        write_to(std::ostream& os) const  { os << "ColumnFeature " << name() << "  " << std::endl; }
};


////  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature

class InteractionFeature : public FeatureABC
{
  FeatureABC const* mFeature1;
  FeatureABC const* mFeature2;

 public:
  virtual ~InteractionFeature() { assert(mFeature1); delete mFeature1; delete mFeature2; }

  InteractionFeature(FeatureABC const* f1, FeatureABC const* f2)
    : FeatureABC(), mFeature1(f1->clone()), mFeature2(f2->clone()) { order_features(); }

  InteractionFeature(EnvelopeABC const& e1, EnvelopeABC const& e2)
    : FeatureABC(), mFeature1(e1.clone_feature_ptr()), mFeature2(e2.clone_feature_ptr()) { order_features(); }

  FeatureABC* clone()         const;
  std::string name()          const;
  std::string operator_name() const { return "*"; }
  Arguments   arguments()     const;
  
  Range       range()         const { return make_anonymous_range(make_binary_range(std::multiplies<double>(),
									    mFeature1->range(),
									    mFeature2->range())); }
  double      center()        const { return mFeature1->center() * mFeature2->center(); }
  double      scale()         const { return mFeature1->scale() * mFeature2->scale(); }
  bool        is_dummy()      const { return (mFeature1->is_dummy() && mFeature2->is_dummy()) ; }
  
  void        write_to (std::ostream& os) const;
private:
  void        order_features()      {if (mFeature1 > mFeature2) { FeatureABC const* temp (mFeature1); mFeature1=mFeature2; mFeature2=temp;}}

};


////  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature

template<class Op>
class UnaryFeature : public FeatureABC
{
  Op                mOp;
  FeatureABC const* mFeature;

 public:
  virtual ~UnaryFeature() { assert(mFeature); delete mFeature;}

  UnaryFeature(Op const& op, EnvelopeABC const& e)
    : FeatureABC(), mOp(op), mFeature(e.clone_feature_ptr()) {  }

  UnaryFeature(Op const& op, FeatureABC const* f)
    : FeatureABC(), mOp(op), mFeature(f->clone()) {  }

  FeatureABC* clone()         const;
  std::string name()          const;
  std::string operator_name() const { return operator_traits<Op>::name(); }
  Arguments   arguments()     const { return Arguments(); }
  
  Range       range()         const { return make_anonymous_range(make_unary_range(mOp, mFeature->range())); }
  double      center()        const { return mOp(mFeature->center()); }
  double      scale()         const { return mOp(mFeature->scale()); }
  bool        is_dummy()      const { return mFeature->is_dummy(); }
  
  void        write_to (std::ostream& os) const;
};

template<class Op>
UnaryFeature<Op>*
make_unary_feature_ptr(Op const& op, EnvelopeABC const& e)
{
  return new UnaryFeature<Op>(op,e);
}

////  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature

template<class Op>
class BinaryFeature : public FeatureABC
{
  Op mOp;
  FeatureABC const* mFeature1;
  FeatureABC const* mFeature2;
  
 public:
  virtual ~BinaryFeature() { assert(mFeature1); delete mFeature1; assert(mFeature2); delete(mFeature2);}

  BinaryFeature(Op const& op, FeatureABC const* f1, FeatureABC const* f2)
    : FeatureABC(), mOp(op), mFeature1(f1->clone()), mFeature2(f2->clone()) { }

  BinaryFeature(Op const& op, EnvelopeABC const& e1, EnvelopeABC const& e2)
    : FeatureABC(), mOp(op), mFeature1(e1.clone_feature_ptr()), mFeature2(e2.clone_feature_ptr()) { }
  
  FeatureABC* clone()          const;
  std::string name()           const;
  std::string operator_name()  const { return operator_traits<Op>::name(); }
  Arguments   arguments()      const { return Arguments(); }
  
  Range       range()          const { return make_anonymous_range(make_binary_range(mOp,mFeature1->range(),mFeature2->range())); }

  double      center()         const { return mOp(mFeature1->center(),mFeature2->center()); }
  double      scale()          const { return mOp(mFeature1->scale(), mFeature2->scale()); }

  void        print_to (std::ostream& os) const;
  void        write_to (std::ostream& os) const;
};

template<class Op>
BinaryFeature<Op>*
make_binary_feature_ptr(Op const& op, EnvelopeABC const& e1, EnvelopeABC const& e2)
{
  return new BinaryFeature<Op>(op,e1,e2);
}


#include "features.Template.h"

#endif

