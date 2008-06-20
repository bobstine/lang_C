// $Id: anonymous_iterator.h,v 1.6 2007/12/19 01:02:41 bob Exp $

#ifndef _ANONYMOUS_ITERATORS_H_
#define _ANONYMOUS_ITERATORS_H_

#include "range.h"
#include "range_traits.h"

// traits
#include <iterator>
// for empty range
#include <vector>

template<class tag, class value_type>
class anonymous_iterator_abc;

template<class tag, class BaseIter>
class anonymous_iterator;

template<class tag, class value>
class anonymous_iterator_envelope;


template <class Iter>
inline
range<anonymous_iterator_envelope<typename std::iterator_traits<Iter>::iterator_category,
				  typename std::iterator_traits<Iter>::value_type> >
make_anonymous_range (const Iter& b, const Iter& e)
{
  return make_range(make_anonymous_iterator(b),make_anonymous_iterator(e));
}


template <class Range>
inline
range<anonymous_iterator_envelope<typename range_traits<Range>::iterator_category,
				  typename range_traits<Range>::value_type> >
make_anonymous_range (const Range& r)
{
  return make_range(make_anonymous_iterator(begin(r)),make_anonymous_iterator(end(r)));
}


#include "anonymous_iterator.Template.h"


#endif
