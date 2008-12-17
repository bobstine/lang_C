// $Id: function_iterators.Template.h,v 1.15 2005/06/15 14:54:17 bob Exp $ -*- c++ -*-

#ifndef _FUNCTION_ITERATORS_TEMPLATE_H_
#define _FUNCTION_ITERATORS_TEMPLATE_H_

#include <iterator>
#include "function_result_type.h"


////////////////////////////////////////////////////////////////////////////////
//
// Our definition of a new tag

#ifndef SPARSE_ITERATOR_TAG
#define SPARSE_ITERATOR_TAG
struct sparse_iterator_tag: public std::random_access_iterator_tag {};
#endif

//
////////////////////////////////////////////////////////////////////////////////

namespace std
{
  template <class Func, class Iter, class Cat>
  class iterator_traits<unary_iterator<Func,Iter,Cat> >
  {
    typedef typename iterator_traits<Iter>::iterator_category   iterator_category;
    typedef typename function_value_type<F>::type               value_type;
    typedef typename iterator_traits<Iter>::difference_type     difference_type;
    typedef typename &value_type                                reference;
  }
}


// UNARY_RANGE_TRAITS  UNARY_RANGE_TRAITS  UNARY_RANGE_TRAITS  UNARY_RANGE_TRAITS

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
  typedef          binary_iterator<F, base_iterator1, base_iterator2, typename two_iterator_traits<base_iterator1, base_iterator2>::iterator_category> const_iterator;
  typedef          range<const_iterator> range;
};



// UNARY_ITERATOR     UNARY_ITERATOR     UNARY_ITERATOR     UNARY_ITERATOR     UNARY_ITERATOR     UNARY_ITERATOR     

template<class F, class I>
class unary_iterator<F, I, std::forward_iterator_tag>
{
protected:
  F mF;
  I mIter;
  
public:  
  typedef unary_iterator<F,I,std::forward_iterator_tag> type;

  unary_iterator(F const& f, I const& it)
    : mF(f), mIter(it)  {  }

  type& operator++()                    { ++mIter; return (*this); }
  type operator++(int)                  { type result(*this); ++mIter; return result; }

  bool operator!=(const type& it) const { return mIter != it.mIter; }
  bool operator==(const type& it) const { return mIter == it.mIter; }
  
  typename function_result_type<F>::type operator*()  const { return mF(*mIter); }
  
  typename std::iterator_traits<I>::difference_type
    operator-(const type& it) const {
      return mIter - it.mIter; }
  
};


template<class F, class I>
class unary_iterator<F,I,std::bidirectional_iterator_tag> : public unary_iterator<F,I,std::forward_iterator_tag>
{
 public:
  typedef unary_iterator<F,I,std::bidirectional_iterator_tag> type;
  typedef unary_iterator<F,I,std::forward_iterator_tag>       parent_type;
  
  unary_iterator(F const& f, I const& it) : parent_type(f,it) { }

  type& operator--()                    { --parent_type::mIter; return (*this); }

  type operator--(int)                  { type result(*this); --parent_type::mIter; return result;}
};



template<class F, class I>
class unary_iterator<F, I, std::random_access_iterator_tag> : public unary_iterator<F,I,std::bidirectional_iterator_tag>
{
 public:

  typedef unary_iterator<F, I,std::random_access_iterator_tag> type;
  typedef unary_iterator<F,I,std::bidirectional_iterator_tag>  parent_type;
  
  unary_iterator(F const& f, I const& it) : parent_type(f, it) { }

  type operator+(int n)  const  {type result(*this); result += n; return result; }
  type operator-(int n)  const  {type result(*this); result -= n; return result; }
  
  type& operator+=(int n) { parent_type::mIter += n; return (*this); }
  type& operator-=(int n) { parent_type::mIter -= n; return (*this); }

  
  bool operator<(type const& it) const { return parent_type::mIter < it.mIter; }
  bool operator>(type const& it) const { return parent_type::mIter > it.mIter; }
  
  bool operator<=(type const& it) const { return parent_type::mIter <= it.mIter; }
  bool operator>=(type const& it) const { return parent_type::mIter >= it.mIter; }
  
  typename std::iterator_traits<I>::difference_type operator-(const type& it) const
  { return parent_type::mIter - it.mIter; }
  
};



template<class F, class I>
class unary_iterator<F, I, sparse_iterator_tag> : public unary_iterator<F,I,std::random_access_iterator_tag>
                                                , public std::iterator<sparse_iterator_tag, typename function_result_type<F>::type>
{
 public:

  typedef unary_iterator<F, I,sparse_iterator_tag>            type;
  typedef unary_iterator<F,I,std::random_access_iterator_tag> parent_type;
  
  unary_iterator(F const& f, const I& it) : parent_type(f,it) { }

  class advance_to_next_non_zero
  {
    typename I::advance_to_next_non_zero m_n;
  public:
    advance_to_next_non_zero(const type& start_iter):
      m_n(start_iter.mIter)
    {
    };
    void operator()(type& iter)
    {
      m_n(iter.mIter);
    }
  };
};



//   BINARY_ITERATOR     BINARY_ITERATOR     BINARY_ITERATOR     BINARY_ITERATOR     BINARY_ITERATOR     BINARY_ITERATOR     



template<class F, class I1, class I2>
class binary_iterator<F,I1,I2,std::forward_iterator_tag>
{
protected:
  F  mF;
  I1 mIter1;
  I2 mIter2;
public:
  typedef binary_iterator<F,I1,I2,class std::forward_iterator_tag> type;
  
  binary_iterator(F const& f, const I1& it1, const I2& it2)
    : mF(f), mIter1(it1), mIter2(it2)  {  }

  typename F::result_type  operator*() const { return mF(*mIter1,*mIter2); }

  type& operator++()        { ++mIter1; ++mIter2; return (*this); }

  type  operator++(int)     { type result(*this); ++mIter1; ++mIter2; return result; }

  bool operator!=(const type& it) const { return mIter2 != it.mIter2; } 
  bool operator==(const type& it) const { return mIter2 == it.mIter2; } 

};


template<class F, class I1, class I2>
class binary_iterator<F,I1,I2,std::bidirectional_iterator_tag> : public binary_iterator<F,I1,I2,std::forward_iterator_tag>
{
 public:
  typedef binary_iterator<F,I1,I2,std::bidirectional_iterator_tag> type;
  typedef binary_iterator<F,I1,I2,std::forward_iterator_tag> parent_type;
  
  binary_iterator(F const& f, const I1& it1, const I2& it2)    : parent_type(f, it1, it2) { }

  type& operator--()    { --parent_type::mIter1; --parent_type::mIter2; return (*this); }
  type  operator--(int) {  type result(*this); --parent_type::mIter1; --parent_type::mIter2; return result; }

};


template<class F, class I1, class I2>
class binary_iterator<F,I1,I2,std::random_access_iterator_tag>  : public binary_iterator<F,I1,I2,std::bidirectional_iterator_tag>
{
 public:
  typedef binary_iterator<F,I1,I2,std::random_access_iterator_tag> type;
  typedef binary_iterator<F,I1,I2,std::bidirectional_iterator_tag> parent_type;
  
  binary_iterator(F const&f, const I1& it1, const I2& it2) : parent_type(f, it1,it2) { }

  type operator+(int n)  const  { type result(*this); result += n; return result; }
  type operator-(int n)  const  { type result(*this); result -= n; return result; }
  
  type& operator+=(int n) { parent_type::mIter1 += n; parent_type::mIter2 += n; return (*this);}
  type& operator-=(int n) { parent_type::mIter1 -= n; parent_type::mIter2 -= n; return (*this);}

  typename std::iterator_traits<I1>::difference_type operator-(type const& it) const {return parent_type::mIter2 - it.mIter2; }

  bool operator< (const type& it) const { return parent_type::mIter2 <  it.mIter2; } // second
  bool operator> (const type& it) const { return parent_type::mIter2 >  it.mIter2; } // second
  bool operator<=(const type& it) const { return parent_type::mIter2 <= it.mIter2; } // second
  bool operator>=(const type& it) const { return parent_type::mIter2 >= it.mIter2; } // second
};

#endif
