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



// UNARY_ITERATOR  UNARY_ITERATOR  UNARY_ITERATOR  UNARY_ITERATOR  UNARY_ITERATOR

template <class F, class Iter, class Category>
class unary_iterator;


template <class F, class Iter>
inline
unary_iterator<F,Iter, typename std::iterator_traits<Iter>::iterator_category>
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


namespace {
  template<class C1, class C2>
  class Base_Iterator_Category;
  
  template <class C>
  class Base_Iterator_Category<C,C>
  {
  public:
    typedef C iterator_category;
  };


template<class I1, class I2>
class two_iterator_traits
{
public:
  typedef typename std::iterator_traits<I1>::iterator_category iterator1_category;
  typedef typename std::iterator_traits<I2>::iterator_category iterator2_category;
  typedef typename Base_Iterator_Category<iterator1_category,iterator2_category>::iterator_category iterator_category;
};
}

template<class F, class Iter1, class Iter2, class Category>
class binary_iterator;

template <class F, class Iter1, class Iter2>
inline
binary_iterator<F, Iter1, Iter2, typename two_iterator_traits<Iter1,Iter2>::iterator_category>
make_binary_iterator (F f, Iter1 i1, Iter2 i2)
{
  return binary_iterator<F, Iter1, Iter2, typename two_iterator_traits<Iter1,Iter2>::iterator_category>(f, i1, i2);
};

template <typename F, class Iter1, class Iter2>
inline
range<binary_iterator<F, Iter1, Iter2,typename two_iterator_traits<Iter1,Iter2>::iterator_category> >
make_binary_range (F f, range<Iter1> r1, range<Iter2> r2)
{
  return make_range(make_binary_iterator(f, begin(r1), begin(r2)),
		    make_binary_iterator(f, end  (r1), end  (r2)) );
};


#include "function_iterators.Template.h"

#endif
