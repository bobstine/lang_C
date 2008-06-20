// $Id: range_traits.h,v 1.2 2002/06/17 14:35:09 bob Exp $ -*- c++ -*-

#ifndef _RANGE_TRAITS_H
#define _RANGE_TRAITS_H

#include <utility>
#include <functional>
#include <vector>
#include <assert.h>

template <class Container>
class range_traits
{
public:
  typedef typename Container::const_iterator iterator;
  typedef typename std::pair<iterator,iterator> range;
  typedef typename std::iterator_traits<iterator>::iterator_category iterator_category;
  typedef typename std::iterator_traits<iterator>::value_type value_type;
  typedef typename std::iterator_traits<iterator>::difference_type difference_type;
  typedef typename std::iterator_traits<iterator>::pointer pointer;
  typedef typename std::iterator_traits<iterator>::reference reference;
};

template <class Range> class range_traits;

template <class Iter>
class range_traits<std::pair<Iter, Iter> >
{
public:
  typedef Iter iterator; 
  typedef typename std::pair<iterator,iterator> range;
  typedef typename std::iterator_traits<iterator>::iterator_category iterator_category;
  typedef typename std::iterator_traits<iterator>::value_type value_type;
  typedef typename std::iterator_traits<iterator>::difference_type difference_type;
  typedef typename std::iterator_traits<iterator>::pointer pointer;
  typedef typename std::iterator_traits<iterator>::reference reference;
};


// BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN


template<class Container>
inline
typename Container::const_iterator
begin(const Container& range)
{
  return range.begin();
}

template<class Iter>
inline
typename range_traits<std::pair<Iter,Iter> >::iterator
begin(const std::pair<Iter,Iter>& range)  // speed hack? Does it matter?  If so, document it!
{                                         // EVIL! should take a std::pair<> and then have compiler remove it!
  return range.first;
}

// END  END  END  END  END  END  END  END  END  END  END  END  END  END  END  END  END  END  END 

template<class Container>
inline
typename Container::const_iterator
end(const Container& range)
{
  return range.end();
}
  
template<class Iter>
inline
typename range_traits<std::pair<Iter,Iter> >::iterator
end(const std::pair<Iter,Iter>& range)   // EVIL! should take a std::pair<> and then have compiler remove it!
{
  return range.second;
}

// UNARY_RANGE_TRAITS   UNARY_RANGE_TRAITS   UNARY_RANGE_TRAITS   UNARY_RANGE_TRAITS

template<class Constant, class I>
class operator_star_unary_function;

template<class UnaryOp, class I, class Tag,class Operator_star>
class unary_iterator;

template <class UnaryOp, class Range>
class unary_range_traits 
{
  typedef range_traits<Range> base_traits;
  typedef typename base_traits::iterator base_iterator;
  typedef typename base_traits::iterator_category base_category;
  typedef operator_star_unary_function<UnaryOp, base_iterator> operator_star;
  typedef unary_iterator<UnaryOp, base_iterator, base_category, operator_star> iterator;
  typedef typename std::pair<iterator,iterator> range;
};


template<class I>
class operator_star_function_iterator;

template <class Range>
class unary_other_range_traits 
{
  typedef range_traits<Range> base_traits;
  typedef typename base_traits::iterator base_iterator;
  typedef typename base_traits::iterator_category base_category;

  typedef typename base_traits::value_type::argument_type argument_type;
  typedef operator_star_function_iterator<base_iterator> operator_star;
  typedef unary_iterator<argument_type, base_iterator, base_category, operator_star> iterator;
  typedef typename std::pair<iterator,iterator> range;
};



// BINARY_RANGE_TRAITS   BINARY_RANGE_TRAITS   BINARY_RANGE_TRAITS   BINARY_RANGE_TRAITS

template<class BinaryOp, class I1,class I2> class forward_binary_iterator;
template<class BinaryOp, class I1,class I2> class bidirectional_binary_iterator;
template<class BinaryOp, class I1,class I2> class random_access_binary_iterator;

template <class BinaryOp, class Range1, class Range2, class Tag =typename range_traits<Range1>::iterator_category>
  class binary_range_traits;                         

template <class BinaryOp, class Range1, class Range2>
class binary_range_traits<BinaryOp,Range1,Range2,std::forward_iterator_tag>
{
public:
  typedef typename forward_binary_iterator<BinaryOp,typename range_traits<Range1>::iterator,
			   		            typename range_traits<Range2>::iterator>::iterator iterator;
  typedef std::pair<iterator,iterator> range;
};
  
template <class BinaryOp, class Range1, class Range2>
class binary_range_traits<BinaryOp,Range1,Range2,std::bidirectional_iterator_tag>
{
public:
  typedef bidirectional_binary_iterator<BinaryOp,typename range_traits<Range1>::iterator,
						 typename range_traits<Range2>::iterator> iterator;
  typedef std::pair<iterator,iterator> range;
};
  
template <class BinaryOp, class Range1, class Range2>
class binary_range_traits<BinaryOp,Range1,Range2,std::random_access_iterator_tag>
{
public:
  typedef random_access_binary_iterator<BinaryOp, typename range_traits<Range1>::iterator,
  			                          typename range_traits<Range2>::iterator> iterator;
  typedef std::pair<iterator,iterator> range;
};



// FUNCTION_RANGE_TRAITS   FUNCTION_RANGE_TRAITS   FUNCTION_RANGE_TRAITS   FUNCTION_RANGE_TRAITS  

template<class Iterator> class forward_function_iterator;
template<class Iterator> class bidirectional_function_iterator;
template<class Iterator> class random_access_function_iterator;

template <class Range, class Tag = typename range_traits<Range>::iterator_category>
class function_range_traits;           

template <class Range>
class function_range_traits<Range,std::forward_iterator_tag>
{
public:
  typedef forward_function_iterator<typename range_traits<Range>::iterator> iterator;
  typedef std::pair<iterator,iterator> range;
  typedef typename std::iterator_traits<iterator>::value_type::argument_type  argument_type;  // f_i domain
  typedef typename std::iterator_traits<iterator>::value_type::result_type      result_type;  // f_i range
};
  
template <class Range>
class function_range_traits<Range,std::bidirectional_iterator_tag>
{
public:
  typedef bidirectional_function_iterator<typename range_traits<Range>::iterator> iterator;
  typedef std::pair<iterator,iterator> range;
  typedef typename std::iterator_traits<iterator>::value_type::argument_type  argument_type;  // f_i domain
  typedef typename std::iterator_traits<iterator>::value_type::result_type      result_type;  // f_i range

};
  
template <class Range>
class function_range_traits<Range,std::random_access_iterator_tag>
{
public:
  typedef random_access_function_iterator<typename range_traits<Range>::iterator> iterator;
  typedef typename std::pair<iterator,iterator> range;
  typedef typename std::iterator_traits<typename range_traits<Range>::iterator>::value_type::argument_type  argument_type;  // f_i domain
  typedef typename std::iterator_traits<typename range_traits<Range>::iterator>::value_type::result_type      result_type;  // f_i range
};

#endif




