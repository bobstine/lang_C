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


template <class BaseIter>
inline
anonymous_iterator_envelope<typename std::iterator_traits<BaseIter>::iterator_category,
  typename std::iterator_traits<BaseIter>::value_type>
  make_anonymous_iterator (const BaseIter& iter);


template <class Iter>
inline
Ranges::range<anonymous_iterator_envelope<typename std::iterator_traits<Iter>::iterator_category,
				  typename std::iterator_traits<Iter>::value_type> >
  make_anonymous_range (const Iter& b, const Iter& e);



template <class Range>
inline
Ranges::range<anonymous_iterator_envelope<typename range_traits<Range>::iterator_category,
  typename range_traits<Range>::value_type> >
  make_anonymous_range (const Range& r);


#include "anonymous_iterator.Template.h"


#endif
