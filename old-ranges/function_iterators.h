// $Id: function_iterators.h,v 1.19 2003/11/26 04:03:15 bob Exp $

#ifndef _FUNCTION_ITERATORS_H_
#define _FUNCTION_ITERATORS_H_

#include "range.h"
#include "range_traits.h"
#include <iterator>


#ifndef SPARSE_ITERATOR_TAG
#define SPARSE_ITERATOR_TAG
struct sparse_iterator_tag: public std::random_access_iterator_tag {};
#endif

// This file (along with function_iterators.Template.h) defines the following:

//  class extensible_unary_iterator;
//  class unary_iterator;
//  class unary_range_traits;
//  make_unary_iterator (F f, Iter iter);
//  make_unary_range (X item, Range r);

//  class function_iterator;
//  make_function_iterator (X item, Iter iter)
//  make_function_range (X item, Range r)
//  class extensible_binary_iterator;
//  make_binary_iterator (F f, Iter1 i1, Iter2 i2)
//  make_binary_range (F f, Iter1 i1, Iter2 i2)

// UNARY_ITERATOR  UNARY_ITERATOR  UNARY_ITERATOR  UNARY_ITERATOR  UNARY_ITERATOR

template <class Host, class I, class Category>
class extensible_unary_iterator;

template <class F, class BaseIter, class Category>
class unary_iterator;

template <class F, class Iter>
inline
unary_iterator<F,Iter,typename std::iterator_traits<Iter>::iterator_category>
make_unary_iterator (F f, Iter iter)
{
  return unary_iterator<F, Iter, typename std::iterator_traits<Iter>::iterator_category>(f, iter);
}

template <typename F, class Range>
inline
range<unary_iterator<F, typename range_traits<Range>::const_iterator, typename range_traits<Range>::iterator_category> >
make_unary_range (F item, Range r)
{
  return make_range(make_unary_iterator(item, begin(r)),
		    make_unary_iterator(item, end(r))    );
};


//  BINARY_ITERATOR  BINARY_ITERATOR  BINARY_ITERATOR  BINARY_ITERATOR  BINARY_ITERATOR  

template<class Host, class I1, class I2, class Category>
class extensible_binary_iterator;

template <class F, class BaseIter1, class BaseIter2>
class binary_iterator;

template <class F, class Iter1, class Iter2>
inline
binary_iterator<F,Iter1,Iter2>
make_binary_iterator (F f, Iter1 i1, Iter2 i2)
{
  return binary_iterator<F, Iter1, Iter2>(f, i1, i2);
};

template <typename F, class Iter1, class Iter2>
inline
range<binary_iterator<F, Iter1, Iter2> >
make_binary_range (F f, range<Iter1> r1, range<Iter2> r2)
{
  return make_range(make_binary_iterator(f, begin(r1), begin(r2)),
		    make_binary_iterator(f, end  (r1), end  (r2)) );
};


// FUNCTION_ITERATOR  FUNCTION_ITERATOR  FUNCTION_ITERATOR  FUNCTION_ITERATOR  FUNCTION_ITERATOR  FUNCTION_ITERATOR  

template <typename X, class BaseIter>
class function_iterator;

template <typename X, class Iter>
inline
function_iterator<X,Iter>
make_function_iterator (X item, Iter iter)
{
  return function_iterator<X, Iter>(item, iter);
}

template <typename X, class Range>
inline
range<function_iterator<X, typename range_traits<Range>::const_iterator > >
make_function_range (X item, Range r)
{
  return make_range(make_function_iterator(item, begin(r)),
		    make_function_iterator(item, end(r))   );
};



/*   Hungry Ranges
     
  These return a 'range looking for a function' object that may then
  be applied to various functions.

  The hungry_range class allows you to form a unary range in a
  slightly different syntax.  Rather than bind the function to the
  range using make_range, a hungry_range object is a function object
  holding the range, looking for a function to evaluate on that range.

  This form is particularly useful in forming tables as the
  cross-product of two ranges.

  The specific function in the unary range is instantiated by the
  operator() function, which then returns a unary_range.  As a result,
  we have a version of make_unary_range that takes only a range as its
  argument.

*/

template <typename X, class Range>
class hungry_unary_range;

template <typename X, class Range>
class hungry_function_range;


template <class UnaryOp, class Range>
hungry_unary_range<UnaryOp,typename range_traits<Range>::range>
make_unary_range (const Range& r)
{
  return hungry_unary_range<UnaryOp,typename range_traits<Range>::range>(make_range(r));
}

template <typename X, class Range>
hungry_function_range<X,Range>
make_function_range (const Range& r)
{
  return hungry_function_range<X,Range>(r);
}




#include "function_iterators.Template.h"

#endif
