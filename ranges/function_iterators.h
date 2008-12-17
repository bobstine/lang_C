#ifndef _FUNCTION_ITERATORS_H_
#define _FUNCTION_ITERATORS_H_

#include "range.h"
#include "range_traits.h"

#include "transform_iterator.hpp"  // my hacked version allows unary range to use lambda functions

#include <boost/iterator/zip_iterator.hpp>
#include <boost/tuple/tuple.hpp>



template <class F, class Range>
range<boost::transform_iterator<F, typename range_traits<Range>::const_iterator > >
make_unary_range (F f, Range r)
{
  return make_range(boost::make_transform_iterator(begin(r), f),
		    boost::make_transform_iterator(end(r)  , f)    );
};
  


template <class F, class Iter1, class Iter2>
  range< boost::transform_iterator< F, boost::zip_iterator< boost::tuple<Iter1, Iter2> > > >
  make_binary_range (F const& f, range<Iter1> r1, range<Iter2> r2)
{
  return make_range(boost::make_transform_iterator(
						   boost::make_zip_iterator(boost::make_tuple(begin(r1), begin(r2))),
						   f),
		    boost::make_transform_iterator(
						   boost::make_zip_iterator(boost::make_tuple(end(r1)  , end  (r2))),
						   f)
		    );
}




/*
template <class X1, class X2, class Y, class Iter1, class Iter2>
  range<
    boost::transform_iterator<
      std::unary_function<boost::tuple<X1,X2>,Y>,
      boost::zip_iterator< boost::tuple<Iter1, Iter2> >
  > >
  make_binary_range (std::unary_function<boost::tuple<X1,X2>,Y> f, range<Iter1> r1, range<Iter2> r2)
{
  return make_range(boost::make_transform_iterator(
						   boost::make_zip_iterator(boost::make_tuple(begin(r1), begin(r2))),
						   f),
		    boost::make_transform_iterator(
						   boost::make_zip_iterator(boost::make_tuple(end(r1)  , end  (r2))),
						   f)
		    );
}
*/




class TupleSum: public std::unary_function<boost::tuple<double,double>, double>
{
public:
  double operator()(boost::tuple<double,double> tup) const;
};

inline
double
TupleSum::operator()(boost::tuple<double,double> tup) const { return tup.get<0>() + tup.get<1>(); }


class TupleProduct: public std::unary_function<boost::tuple<double,double>, double>
{
public:
  double operator()(boost::tuple<double,double> tup) const;
};

inline
double
TupleProduct::operator()(boost::tuple<double,double> tup) const { return tup.get<0>() * tup.get<1>(); }






 // UNARY_RANGE_TRAITS  UNARY_RANGE_TRAITS  UNARY_RANGE_TRAITS  UNARY_RANGE_TRAITS
 // Shouldn't these all just be range traits rather than special sorts of range traits?
 /*
template <class F, class Range>
class unary_range_traits 
{
protected:
  typedef          range_traits<Range>            base_traits;
  typedef typename base_traits::const_iterator    const_base_iterator;
  typedef typename base_traits::iterator_category base_category;

public:
  typedef          unary_iterator<F, const_base_iterator, base_category> const_iterator;
  typedef          range<const_iterator>                                 range;
};


// BINARY_RANGE_TRAITS   BINARY_RANGE_TRAITS   BINARY_RANGE_TRAITS   BINARY_RANGE_TRAITS

namespace {
  template<class C1, class C2>
  class Base_Iterator_Category;
  
  template <class C>
  class Base_Iterator_Category<C,C>
  {
  public:
    typedef C iterator_category;
  };
}

template<class I1, class I2>
class two_iterator_traits
{
public:
  typedef typename std::iterator_traits<I1>::iterator_category iterator1_category;
  typedef typename std::iterator_traits<I2>::iterator_category iterator2_category;
  typedef typename Base_Iterator_Category<iterator1_category,iterator2_category>::iterator_category iterator_category;
};


template<class F, class I1, class I2>
class binary_iterator;

template <class F, class Range1, class Range2>
class binary_range_traits 
{
private:
  typedef          range_traits<Range1> base_traits1;
  typedef          range_traits<Range2> base_traits2;
  typedef typename base_traits1::const_iterator base_iterator1;
  typedef typename base_traits2::const_iterator base_iterator2;
  typedef typename two_iterator_traits<base_iterator1, base_iterator2>::iterator_category base_category;

public:
  typedef          binary_iterator<F, base_iterator1, base_iterator2> const_iterator;
  typedef          range<const_iterator> range;
};
 */

#endif
