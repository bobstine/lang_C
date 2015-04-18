#ifndef _FEATURES_TEMPLATE_H
#define _FEATURES_TEMPLATE_H
#include "features.h"


////  Column     Column     Column     Column     Column     Column     Column     Column     Column     Column     

//  Feature constructors  (define after defining the different types of features)

template<class Op>
Feature::Feature(Op op, Feature x)
{
  mFP = new UnaryFeature<Op>(op,x);
}


template<class Op>
Feature::Feature(Op op, Feature  x1, Feature  x2)
{
  mFP = new BinaryFeature<Op>(op,x1,x2);
}


// UnaryFeature     UnaryFeature     UnaryFeature     UnaryFeature     UnaryFeature     UnaryFeature     UnaryFeature

template<class Op>
std::string
UnaryFeature<Op>::class_name()     const { return "UnaryFeature"; }

template<class Op>
std::string
UnaryFeature<Op>::name()           const { return operator_traits<Op>::name() + "[" + mFeature->name() + "]"; }

template<class Op>
int
UnaryFeature<Op>::degree()         const { return mFeature->degree(); }

template<class Op>
FeatureABC::DependenceMap
UnaryFeature<Op>::dependence_map() const { return DependenceMap(); }

template<class Op>
FeatureABC::Iterator
UnaryFeature<Op>::begin()          const { return make_anonymous_iterator(make_unary_iterator(mOp,mFeature->begin()));    }

template<class Op>
FeatureABC::Iterator
UnaryFeature<Op>::end()            const { return make_anonymous_iterator(make_unary_iterator(mOp,mFeature->end()));    }

template<class Op>
FeatureABC::Range
UnaryFeature<Op>::range()          const { return make_anonymous_range(make_unary_range(mOp,mFeature->range()));    }
  
template<class Op>
bool
UnaryFeature<Op>::is_dummy()       const { return mFeature->is_dummy(); }

template<class Op>
bool
UnaryFeature<Op>::is_constant()    const { return mFeature->is_constant(); }

template<class Op>
SCALAR
UnaryFeature<Op>::average()        const
{
  if(std::isnan(mMean))
    mMean = (SCALAR)Ranges::average(range(), (SCALAR)size());
  return mMean;
}

template<class Op>
SCALAR
UnaryFeature<Op>::center()         const { return average(); }

template<class Op>
SCALAR
UnaryFeature<Op>::scale()          const { return (SCALAR)Ranges::standard_deviation(range(),mMean,(SCALAR)size()); }
  

template<class Op>
FeatureABC::Arguments
UnaryFeature<Op>::arguments()  const
{
  Arguments a;
  std::string opName (operator_traits<Op>::name());
  std::string fName  (mFeature->name());
  
  if (opName == "square")
    a[fName] = 2;
  else if (opName == "cube")
    a[fName] = 3;
  else if (opName == "fourth")
    a[fName] = 4;
  else if (opName == "fifth")
    a[fName] = 5;
  else if (opName == "sixth")
    a[fName] = 6;
  else
    a[name()] = 1;
  return a;
}


template<class Op>
void
UnaryFeature<Op>::write_to (std::ostream& os) const
{
  os << class_name() << " " << operator_traits<Op>::symbol() << std::endl;
  mFeature->write_to(os);
  FeatureABC::write_to(os);
}


//  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  

template<class Op>
std::string
BinaryFeature<Op>::class_name()     const { return "BinaryFeature"; }

template<class Op>
std::string
BinaryFeature<Op>::name()       const
{
  return operator_traits<Op>::name()
    + "[ (" + mFeature1->name() + ")"
    + operator_traits<Op>::symbol()
    + "(" + mFeature2->name() + ") ]";
}

template<class Op>
int
BinaryFeature<Op>::degree()      const { return mFeature1->degree() + mFeature2->degree(); }

template<class Op>
FeatureABC::Arguments
BinaryFeature<Op>::arguments()   const { return Arguments(); }
  
template<class Op>
FeatureABC::DependenceMap
BinaryFeature<Op>::dependence_map() const { return DependenceMap(); }

template<class Op>
FeatureABC::Iterator
BinaryFeature<Op>::begin()       const { return make_anonymous_iterator(make_binary_iterator(mOp,mFeature1->begin(),mFeature2->begin())); }

template<class Op>
FeatureABC::Iterator
BinaryFeature<Op>::end()         const { return make_anonymous_iterator(make_binary_iterator(mOp,mFeature1->end(),mFeature2->end())); }

template<class Op >
FeatureABC::Range
BinaryFeature<Op>::range()       const { return make_anonymous_range(make_binary_range(mOp,mFeature1->range(),mFeature2->range())); }

template<class Op>
bool
BinaryFeature<Op>::is_dummy()    const { return (mFeature1->is_dummy() && mFeature2->is_dummy()); }

template<class Op>
bool
BinaryFeature<Op>::is_constant() const { return (mFeature1->is_constant() && mFeature2->is_constant()); }

template<class Op>
SCALAR
BinaryFeature<Op>::average()    const  { return (SCALAR) Ranges::average(range(), (SCALAR)size()); }

template<class Op>
SCALAR
BinaryFeature<Op>::center()      const { return (SCALAR) mOp(mFeature1->center(),mFeature2->center()); }

template<class Op>
SCALAR
BinaryFeature<Op>::scale()       const { return (SCALAR) mOp(mFeature1->scale() ,mFeature2->scale()); }

template<class Op>
void
BinaryFeature<Op>::write_to (std::ostream& os) const
{
  os << class_name() << " " <<  operator_traits<Op>::symbol() << std::endl;
  mFeature1->write_to(os);
  mFeature2->write_to(os);
  FeatureABC::write_to(os);
}


#endif
