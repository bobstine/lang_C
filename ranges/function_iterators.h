// $Id: function_iterators.h,v 1.19 2003/11/26 04:03:15 bob Exp $

/*
  Function iterators have to deal with a the big problem with stl type
  iterators, namely the linkage between traversal and referencing (ie,
  operator*. The problem occurs if you try to write a sequence of
  progressive iterators (forward -> bidirectional -> random access) as
  well as override the operator* function. The inheritance messes up
  C++ compilers ability to figure out which method to call.  Its a
  real mess.  Hence, we could not change the behavior of operator*
  *and* have the natural inheritance that you want to provide for
  traversal.

  An example of what goes wrong is in the function iterator files that
  are in the old subdirectory. It shows what happens if you try to
  glue these into one common hierarchy.

  To deal with that problem, we define extensible iterators.
  Extensible iterators define the behaviors needed for traversal but
  do not define operator*.  Operator * is defined by the 'host' class
  that is built into the traversal iterator, using the 'curiously
  recurring template pattern'.  That in turn leads to a lot of casting
  in order to get the right type for operators like operator++ and the
  like.

  Hence, we have two types of iterators that are organized like this:

         Extensible iterator                      Function iterator
	----------------------                  -------------------------
	forward_iterator_tag           -->       forward_iterator_tag
	       |
	       V
        bidirectional_iterator_tag     -->     bidirectional_iterator_tag
	       |
	       V
        random_access_iterator_tag     -->     random_access_iterator_tag
	
  The inheritance of types happens all on the left, but *not* on the
  right. A random access function iterator is not descended from a
  bidirectional function interator.  That way the compiler can figure
  out which method to use.
	
  For some reason, we decided to implement the referencing by having
  the object inherit from the function.  That seems wrong-headed in
  retrospect.  There's no sense in the iterator 'being' a function
  object (unless we had been able to define an 'apl-like' syntax of
  mapping a function over a range, as in f/x.  We did not.).

  19 Dec 2008  Revision to allow lambda functions, more const, store f rather than be f
  
*/
  
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

template <class F, class Range>
  inline
  Ranges::range<unary_iterator<F, typename range_traits<Range>::const_iterator, typename range_traits<Range>::iterator_category> >
make_unary_range (F f, Range r)
{
  return Ranges::make_range(make_unary_iterator(f, begin(r)),
			    make_unary_iterator(f, end(r))    );
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
  Ranges::range<binary_iterator<F, Iter1, Iter2> >
  make_binary_range (F f, Ranges::range<Iter1> r1, Ranges::range<Iter2> r2)
{
  return Ranges::make_range(make_binary_iterator(f, begin(r1), begin(r2)),
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
  Ranges::range<function_iterator<X, typename range_traits<Range>::const_iterator > >
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
