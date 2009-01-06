// $Id: range_ops.Template.h,v 1.9 2004/08/23 13:03:50 bob Exp $ -*- c++ -*-


// size
/*
  template <class Iter>
  inline
  size_t
  range_ops::size (range<Iter> r)
  {
  Iter i1 (begin(r));
  Iter i2 (end(r));
  return (i2-i1);
  };
*/

// count_if
template <class Range, class Pred>
inline
typename range_traits<Range>::difference_type
range_ops::count_if (Range range, Pred pred)
{
  return std::count_if(begin(range), end(range), pred);
};

// min_element
template <class Range>
inline
typename range_traits<Range>::const_iterator
range_ops::min_element(const Range& range)
{
  return std::min_element(begin(range), end(range));
};

// max_element
template <class Range>
inline
typename range_traits<Range>::const_iterator
range_ops::max_element(const Range& range)
{
  return std::max_element(begin(range), end(range));
}

// copy
template <class Range, class I>
inline
I
range_ops::copy (const Range& range, I it)
{
  return std::copy(begin(range), end(range), it);
}

// for_each
template <class Range, class F>
inline
F
range_ops::for_each (const Range& range, F f)
{
  return std::for_each(begin(range), end(range), f);
}

// fill
template<class Range>
inline
void
range_ops::fill(Range r, typename range_traits<Range>::value_type value)
{
  std::fill(begin(r), end(r), value);
}


// accumulate
namespace{
  template <class Range, class Result, class Tag>
  inline
  Result
  accumulate_unrolled(Range range, Result init, Tag)
  {
    // A call to std::accumulte runs 6 times slower than our own loop!
    // For some reason, its a lot faster to loop here than call STL.
    //    return accumulate(begin(range), end(range), init);
    for(typename range_traits<Range>::const_iterator i = begin(range); i != end(range); ++i)
      {
	init = init + *i; 
      }
    return init;
  }

  template <class Range>
  inline typename range_traits<Range>::value_type
  accumulate_unrolled(Range r, typename range_traits<Range>::value_type init, std::random_access_iterator_tag)
  {
    typename range_traits<Range>::const_iterator i = begin(r);
    typename range_traits<Range>::const_iterator e = end(r);

    if((e - i) > 6)
      {
	typename range_traits<Range>::const_iterator last = e - 5;
	while(i < last)
	  {
	    init = init + *i; ++i;
	    init = init + *i; ++i;
	    init = init + *i; ++i;
	    init = init + *i; ++i;
	    init = init + *i; ++i;
	  }
      }
    for(;i != e; ++i)   // better with < rather than !=, but not right
      { init = init + *i; }

    return init;
  }

  template <class Range>
  inline typename range_traits<Range>::value_type
  accumulate_unrolled(Range range, typename range_traits<Range>::value_type init,sparse_iterator_tag)
  {
    //      typename range_traits<Range>::next_type n(begin(range));
    typename range_traits<Range>::const_iterator::advance_to_next_non_zero next(begin(range));
    typename range_traits<Range>::const_iterator last = end(range);
    for( typename range_traits<Range>::const_iterator i = begin(range); i < last;next(i))
      init = init + *i;

    return init;
  }
}


template <class Range>
inline typename range_traits<Range>::value_type
range_ops::simple_accumulate (const Range& container_or_range, typename range_traits<Range>::value_type init)
{
  typename range_traits<Range>::range r = make_range(container_or_range);
  for(typename range_traits<Range>::const_iterator i = begin(r); i != end(r); ++i)  // shoud run fast
    init = init + *i;
  return init;

  // ****************************************************************************************
  // a variety of way of writing this for loop.  
  //  for(typename range_traits<Range>::iterator i = begin(range); i != end(range); ++i) // very slow
  //  for(typename range_traits<Range>::iterator i = begin(range); i != range.second; ++i)  // runs fast, but ugly
  //  typename range_traits<Range>::iterator ending = end(range);
  //  for(typename range_traits<Range>::iterator i = begin(range); i != ending; ++i)  // runs fast
}

// Unrolled version
template <class Range, class Result>
inline
Result
range_ops::accumulate (const Range& containerOrRange, Result init)
{
  typename range_traits<Range>::range r = make_range(containerOrRange);
  return accumulate_unrolled(r, init, typename range_traits<Range>::iterator_category());
};


// inner_product
template <class R1, class R2, class C>
inline
C
range_ops::inner_product (const R1& x, const R2& y, C initial)
{
  return accumulate(
		    make_binary_range(
				      std::multiplies<double>(),
				      x, y
				      ),
		    initial
		    );
};


// weighted_inner_product
template <class R1, class R2, class R3, class C>
inline
C
range_ops::weighted_inner_product (const R1& x, const R2& y, const R3& wts, C initial)
{
  return inner_product (x, make_binary_range(std::multiplies<double>(),y,wts), initial);
}

  
// transform
template <class Range1, class Iter, class UnaryOp>
inline void
range_ops::transform (Range1 const& x, Iter iter, UnaryOp f)
{
  std::transform(begin(x), end(x), iter, f);
}

template <class Range1, class Range2, class Iter, class BinaryOp>
inline void
range_ops::transform (Range1 const& x, Range2 const& y, Iter iter,  BinaryOp f)
{
  std::transform(begin(x), end(x), begin(y), iter, f);
}


//  RANGE OPERATORS  RANGE OPERATORS  RANGE OPERATORS  RANGE OPERATORS  RANGE OPERATORS  RANGE OPERATORS

//  global namespace since class based.

template <class Iter>
inline
typename std::iterator_traits<Iter>::value_type
accumulate ( const range<Iter>& r, typename std::iterator_traits<Iter>::value_type init)
{
  return range_ops::accumulate(r, init);
}


//  OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT OUTPUT

namespace {

  const int numToShow(7);           // shows one less than this count
  
  template <class Iter, class Tag>
  class RangePrinter;
  
  template <class Iter>
  class RangePrinter<Iter, std::forward_iterator_tag>
  {
    typedef typename std::iterator_traits<Iter>::value_type local_type;
  public:
    void
    operator()(std::ostream& os, range<Iter> const& r) const
    {
      int count (numToShow);
      Iter it (begin(r));
      Iter e  (end(r));
      while (count && (it != e))
      { os << *it << " ";
	++it;
	--count;
      }
      if ((count == 0) && (it != e)) os << " ... ";
    }
  };

  
  template <class Iter>
  class RangePrinter<Iter, std::bidirectional_iterator_tag>
  {
    typedef typename std::iterator_traits<Iter>::value_type local_type;
  public:
    void
    operator()(std::ostream& os, range<Iter> const& r) const
    {
      int count (numToShow);
      Iter it (begin(r));
      Iter e  (end(r));
      while ((count>0) && (it != e))
      { os << *it << " ";
	++it;
	--count;
      }
      if ((count == 0) && (it != e)) os << " ... ";
    }
  };

  template <class Iter>
  class RangePrinter<Iter, std::random_access_iterator_tag>
  {
    typedef typename std::iterator_traits<Iter>::value_type local_type;
  public:
    void
    operator() (std::ostream& os, range<Iter> const& r) const
    {
      int n (end(r)-begin(r));
      if (n > numToShow)
	{
	  int j = numToShow;
	  Iter it = begin(r);
	  while(--j)
	    { os << *it << " ";
	      ++it;
	    }
	  os << "... " << n-numToShow+1 << " more" ;
	}
      else
	std::copy( begin(r), end(r),  std::ostream_iterator<local_type>(os, " "));
    }
  };

}
  
template <class Iter>
inline std::ostream&
operator<<(std::ostream& os, range<Iter> const& r)
{
  RangePrinter<Iter, typename std::iterator_traits<Iter>::iterator_category>()(os,r);
  return os;
}
