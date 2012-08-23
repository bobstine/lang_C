// $Id: function_iterators.Template.h,v 1.15 2005/06/15 14:54:17 bob Exp $ -*- c++ -*-

#ifndef _FUNCTION_ITERATORS_TEMPLATE_H_
#define _FUNCTION_ITERATORS_TEMPLATE_H_
 
#include <iterator>
#include "function_result_type.h"  // type calculator

/*
  Extensible iterators basically embody the iterator properties of our
  range iterators, with the exception of the operator* function that
  is mixed in later to support both f o x_1,...x_n and x o f_1,...,f_n
  operations.  Instead of operator*, extensible iterators supply a
  function 'value' that dereferences the underlying pointer.

  The Host class is the class that uses the extensible class.  Since
  extensible iterators have to support all of the iterator functions,
  including those that return a copy of itself (eg ++), they have to
  use a static cast to return the right type of object.

  Extensible iterators were also useful manage the iterator 'tagging'.
  We first define an abstract template with 3 arguments, then define a
  specific implementation with 2 arguments, using the third as an
  'index' for the different tags.

  Note: b
  Boost implements these as "transform_iterator" and for binary iterators
  as "zip_iterator".

*/

////////////////////////////////////////////////////////////////////////////////
//
// Our definition of a new tag

#ifndef SPARSE_ITERATOR_TAG
#define SPARSE_ITERATOR_TAG
struct sparse_iterator_tag: public std::random_access_iterator_tag {};
#endif

//
////////////////////////////////////////////////////////////////////////////////


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
  typedef          Ranges::range<const_iterator> range;
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
  typedef          Ranges::range<const_iterator> range;
};



// FUNCTION_RANGE_TRAITS   FUNCTION_RANGE_TRAITS   FUNCTION_RANGE_TRAITS   FUNCTION_RANGE_TRAITS

template <class Range>
class function_range_traits 
{
private:
  typedef range_traits<Range> base_traits;
  typedef typename base_traits::const_iterator const_base_iterator;
  typedef typename base_traits::iterator_category base_category;

  typedef typename base_traits::value_type::argument_type argument_type;
  typedef typename base_traits::value_type::result_type result_type;

public:
  typedef function_iterator<argument_type,const_base_iterator> const_iterator;
  typedef          Ranges::range<const_iterator> range;
};



// EXTENSIBLE_UNARY_ITERATOR  EXTENSIBLE_UNARY_ITERATOR  EXTENSIBLE_UNARY_ITERATOR  EXTENSIBLE_UNARY_ITERATOR  
  
template<class Host, class I, class Category>
class extensible_unary_iterator;


template<class Host, class I>
class extensible_unary_iterator<Host,I,std::forward_iterator_tag>
{
protected:   // random access need to define difference and so must access this slot
  I mIter;
  
public:  
  typedef extensible_unary_iterator<Host,I,class std::forward_iterator_tag> type_of_this;
  
  extensible_unary_iterator(const I& it)
    : mIter(it)  {  }

  Host& operator++() { ++mIter; return static_cast<Host&>(*this); }

  Host operator++(int) {
    Host result(static_cast<Host&>(*this));
    ++mIter;
    return result;
  }

  bool operator!=(const type_of_this& it) const { return mIter != it.mIter; }
  bool operator==(const type_of_this& it) const { return mIter == it.mIter; }

protected:
  
  typename std::iterator_traits<I>::value_type 
    value() const
    { return *mIter; }

  typename std::iterator_traits<I>::value_type 
    value()
    { return *mIter; }
};


template<class Host, class I>
class extensible_unary_iterator<Host,I,std::bidirectional_iterator_tag>
 : public extensible_unary_iterator<Host,I,std::forward_iterator_tag>
{
 public:
  typedef extensible_unary_iterator<Host,I,std::bidirectional_iterator_tag> type_of_this;
  typedef extensible_unary_iterator<Host,I,std::forward_iterator_tag> parent_type;
  
  extensible_unary_iterator(const I& it)
    : extensible_unary_iterator<Host,I,std::forward_iterator_tag>(it) { }

  Host& operator--() { --parent_type::mIter; return static_cast<Host&>(*this); }

  Host operator--(int) {
    Host result(static_cast<Host&>(*this));
    --parent_type::mIter;
    return result;
  }
};


template<class Host, class I>
class extensible_unary_iterator<Host, I, std::random_access_iterator_tag>
  : public extensible_unary_iterator<Host,I,std::bidirectional_iterator_tag>
{
 public:

  typedef extensible_unary_iterator<Host, I,std::random_access_iterator_tag> type_of_this;
  typedef extensible_unary_iterator<Host,I,std::bidirectional_iterator_tag> parent_type;
  
  extensible_unary_iterator(const I& it)
    : parent_type(it) { }

  Host
  operator+(int n)  const  {
    Host result(static_cast<const Host&>(*this));
    result += n;
    return result;
  }
  
  Host
  operator-(int n)  const  {
    Host result(static_cast<const Host&>(*this));
    result -= n;
    return result;
  }
  
  Host&
  operator+=(int n) {
    parent_type::mIter += n;
    return static_cast<Host&>(*this);
  }
 
  Host&
  operator-=(int n) {
    parent_type::mIter -= n;
    return static_cast<Host&>(*this);
  }

  
  bool
    operator<(const type_of_this& it) const {
      return parent_type::mIter < it.mIter; }

  bool
    operator>(const type_of_this& it) const {
      return parent_type::mIter > it.mIter; }
  
  bool
    operator<=(const type_of_this& it) const {
      return parent_type::mIter <= it.mIter; }
  
  bool
    operator>=(const type_of_this& it) const {
      return parent_type::mIter >= it.mIter; }

  
  typename std::iterator_traits<I>::difference_type
    operator-(const type_of_this& it) const {
      return parent_type::mIter - it.mIter; }

};



template<class Host, class I>
class extensible_unary_iterator<Host, I, sparse_iterator_tag>
  : public extensible_unary_iterator<Host,I,std::random_access_iterator_tag>
{
 public:

  typedef extensible_unary_iterator<Host, I,sparse_iterator_tag> type_of_this;
  extensible_unary_iterator(const I& it)
    : extensible_unary_iterator<Host,I,std::random_access_iterator_tag>(it) { }

  class advance_to_next_non_zero
  {
    typename I::advance_to_next_non_zero m_n;
  public:
    advance_to_next_non_zero(const type_of_this& start_iter):
      m_n(start_iter.mIter)
    {
    };
    void operator()(type_of_this& iter)
    {
      m_n(iter.mIter);
    }
  };
};

// UNARY_ITERATOR  UNARY_ITERATOR  UNARY_ITERATOR  UNARY_ITERATOR  UNARY_ITERATOR

/*
  Unary iterators bind an object to an extensible iterator.  Unary
  iterators are one example of a Host class.  The binary iterators
  that follow bind an object to two iterators.

  The class F of the object that is bound to the extensible iterator
  must implement the 'operator policy' (ie, implement the operator()
  function and define a result_type trait).  For example, the
  std::unary_function class would be typical.  The class F can be
  stateful; it is copy constructed upon initialization.  Notice that
  the unary iterator does not have state, but only gains a notion of
  state via the inherited properties of F.

  If F implements operator< or operator>, then we have a problem.  One
  must use the 'purify' class to build a clean function that passes
  only the operator() call.

  Finally, unary iterators also inherit from std::iterator, combining
  the tag trait from the underlying extensible iterator with the
  result_type of F.
*/

template <class F, class BaseIter>
class unary_iterator<F,BaseIter,typename std::iterator_traits<BaseIter>::iterator_category>
  :  
  public extensible_unary_iterator<unary_iterator<F,BaseIter,typename std::iterator_traits<BaseIter>::iterator_category>,
				   BaseIter, typename std::iterator_traits<BaseIter>::iterator_category> ,
  public std::iterator<typename std::iterator_traits<BaseIter>::iterator_category, typename function_result_type<F>::type>
{
  typedef typename std::iterator_traits<BaseIter>::iterator_category     Category;
  typedef          unary_iterator<F, BaseIter, Category>                 Host; 
  typedef          extensible_unary_iterator<Host, BaseIter, Category>   extensible_iterator;

  F mF;

public:
  
  unary_iterator(const F& f, const BaseIter& it)
    : extensible_iterator(it), mF(f) { }                  // copy construct F after extensible iterator
    
  typedef typename function_result_type<F>::type    value_type;

  // value_type   operator*()        { return mF(extensible_iterator::value()); }
  value_type   operator*() const  { return mF(extensible_iterator::value()); }
  
  using extensible_iterator::operator==;                       // Identify whose == to call
  using extensible_iterator::operator!=;               
  
  // Should these be here also? If so, then need to specialize the template for random access iterator.
  // extensible_iterator::operator<;
  // extensible_iterator::operator>;                
  // extensible_iterator::operator<=;
  // extensible_iterator::operator>=;                

};



template <class F, class BaseIter>
class unary_iterator<F,BaseIter,sparse_iterator_tag>
  : 
    public extensible_unary_iterator<unary_iterator<F,BaseIter,sparse_iterator_tag>,
				     BaseIter, sparse_iterator_tag>,
    public std::iterator<sparse_iterator_tag, typename function_result_type<F>::type>
{
  typedef unary_iterator<F,BaseIter,sparse_iterator_tag> Host;
  typedef extensible_unary_iterator<Host,BaseIter,sparse_iterator_tag> extensible_iterator;

  F mF;
  
public:
  unary_iterator(const F& f, const BaseIter& it)
    : extensible_iterator(it), mF(f) { }              // copy construct F and extensible iterator properties

  typedef typename function_result_type<F>::type    value_type;

  value_type   operator*()        { return operator()(extensible_iterator::value()); }
  value_type   operator*() const  { return operator()(extensible_iterator::value()); }

  using F::operator();
  
  using extensible_iterator::operator<;                   // Identify who to call
  using extensible_iterator::operator>;                
  using extensible_iterator::operator==;               
  using extensible_iterator::operator!=;               
};




// FUNCTION_ITERATOR  FUNCTION_ITERATOR  FUNCTION_ITERATOR  FUNCTION_ITERATOR  FUNCTION_ITERATOR  FUNCTION_ITERATOR  

/*
  A function iterator is essentially a unary iterator, only differing
  in the way that it treats the operator* function.  A function
  iterator 'reverses' the order of evaluation, treating the value from
  the underlying iterator as the function to be applied to its object
  F.
*/


template <typename X, class BaseIter>
class function_iterator : public extensible_unary_iterator<function_iterator<X,BaseIter>,BaseIter,
							   typename std::iterator_traits<BaseIter>::iterator_category> ,
			  public std::iterator<typename std::iterator_traits<BaseIter>::iterator_category,
					       typename std::iterator_traits<BaseIter>::value_type::result_type>
{
  X mX;
  
public:
  typedef function_iterator<X,BaseIter> Host;
  typedef typename std::iterator_traits<BaseIter>::value_type::result_type result_type;
  typedef typename std::iterator_traits<BaseIter>::iterator_category iterator_category;
  typedef extensible_unary_iterator<Host,BaseIter,iterator_category> extensible_iterator;
  
  function_iterator(const X& x, const BaseIter& it)
    : extensible_iterator(it), mX(x) { }              // initialize extensible iterator properties

  result_type
  operator*()
  { return extensible_iterator::value()(mX); }                             // apply iterator's value to slot value

  result_type
  operator*() const
  { return extensible_iterator::value()(mX); }
};


//  EXTENSIBLE_BINARY_ITERATOR  EXTENSIBLE_BINARY_ITERATOR  EXTENSIBLE_BINARY_ITERATOR

/*
  Extensible binary iterators are analogous to unary versions, but
  support two iterators rather than just one.

  Note: all lengths and end conditions determined by I2 to carry the 2 * range syntax.
*/



template<class Host, class I1, class I2>
class extensible_binary_iterator<Host,I1,I2,std::forward_iterator_tag>
{
protected:
  I1 mIter1;
  I2 mIter2;
public:
  typedef extensible_binary_iterator<Host,I1,I2,class std::forward_iterator_tag> type_of_this;
  
  extensible_binary_iterator(const I1& it1, const I2& it2)
    : mIter1(it1), mIter2(it2)  {  }

  Host& operator++() { ++mIter1; ++mIter2; return static_cast<Host&>(*this); }

  Host operator++(int) {
    Host result(static_cast<Host&>(*this));
    ++mIter1; ++mIter2;
    return result;
  }

  bool operator!=(const type_of_this& it) const { return mIter2 != it.mIter2; } 
  bool operator==(const type_of_this& it) const { return mIter2 == it.mIter2; } 

 protected:
  
  typename std::pair<typename std::iterator_traits<I1>::value_type, typename std::iterator_traits<I2>::value_type>
    value() const
    { return std::make_pair(*mIter1,*mIter2); }
  
  typename std::iterator_traits<I1>::value_type
    first() const
    { return *mIter1; }

  typename std::iterator_traits<I2>::value_type
    second() const
    { return *mIter2; }

  
  typename std::pair<typename std::iterator_traits<I1>::value_type, typename std::iterator_traits<I2>::value_type>
    value() 
    { return std::make_pair(*mIter1,*mIter2); }
  
  typename std::iterator_traits<I1>::value_type
    first() 
    { return *mIter1; }

  typename std::iterator_traits<I2>::value_type
    second()
    { return *mIter2; }

};

template<class Host, class I1, class I2>
class extensible_binary_iterator<Host,I1,I2,std::bidirectional_iterator_tag>
 : public extensible_binary_iterator<Host,I1,I2,std::forward_iterator_tag>
{
 public:
  typedef extensible_binary_iterator<Host,I1,I2,std::forward_iterator_tag> type_of_parent;
  
  extensible_binary_iterator(const I1& it1, const I2& it2)
    : extensible_binary_iterator<Host,I1,I2,std::forward_iterator_tag>(it1, it2) { }

  Host& operator--() {
    --type_of_parent::mIter1;
    --type_of_parent::mIter2;
    return static_cast<Host&>(*this); }

  Host operator--(int) {
    Host result(static_cast<Host&>(*this));
    --type_of_parent::mIter1;
    --type_of_parent::mIter2;
    return result;
  }

};

template<class Host, class I1, class I2>
class extensible_binary_iterator<Host,I1,I2,std::random_access_iterator_tag>
  : public extensible_binary_iterator<Host,I1,I2,std::bidirectional_iterator_tag>
{
 public:
  typedef extensible_binary_iterator<Host,I1,I2,std::random_access_iterator_tag> type_of_this;
  typedef extensible_binary_iterator<Host,I1,I2,std::bidirectional_iterator_tag> type_of_parent;
  
  extensible_binary_iterator(const I1& it1, const I2& it2)
    : type_of_parent(it1,it2) { }

  Host
  operator+(int n)  const  {
    Host result(static_cast<const Host&>(*this));
    result += n;
    return result;
  }
  
  Host
  operator-(int n)  const  {
    Host result(static_cast<const Host&>(*this));
    result -= n;
    return result;
  }
  
  Host&
  operator+=(int n) {
    type_of_parent::mIter1 += n;
    type_of_parent::mIter2 += n;
    return static_cast<Host&>(*this);}
 
  Host&
  operator-=(int n) {
    type_of_parent::mIter1 -= n;
    type_of_parent::mIter2 -= n;
    return static_cast<Host&>(*this);}

  typename std::iterator_traits<I1>::difference_type
    operator-(const type_of_this& it) const {
      return type_of_parent::mIter2 - it.mIter2; }  // use second for 2 * X ops

  bool
    operator<(const type_of_this& it) const {
      return type_of_parent::mIter2 < it.mIter2; } // second

  bool
    operator>(const type_of_this& it) const {
      return type_of_parent::mIter2 > it.mIter2; } // second

  bool
    operator<=(const type_of_this& it) const {
      return type_of_parent::mIter2 <= it.mIter2; } // second

  bool
    operator>=(const type_of_this& it) const {
      return type_of_parent::mIter2 >= it.mIter2; } // second

};


template<class Host, class I1, class I2>
class extensible_binary_iterator<Host,I1,I2,sparse_iterator_tag>
  : public extensible_binary_iterator<Host,I1,I2,std::random_access_iterator_tag>
{
 public:
  typedef extensible_binary_iterator<Host,I1,I2,sparse_iterator_tag> type_of_this;

  extensible_binary_iterator(const I1& it1, const I2& it2)
    : extensible_binary_iterator<Host,I1,I2,std::random_access_iterator_tag>(it1,it2) { }

  class advance_to_next_non_zero
  {
    typename I1::advance_to_next_non_zero n;
  public:
    advance_to_next_non_zero(const type_of_this& start_iter):
      n(start_iter.mIter1)
    {
    };
    void operator()(type_of_this& iter)
    {
      I1 before_jump = iter.mIter1;
      n(iter.mIter1);
      iter.mIter2 += iter.mIter1 - before_jump;
    }
  };

};



//  BINARY_ITERATOR  BINARY_ITERATOR  BINARY_ITERATOR  BINARY_ITERATOR  BINARY_ITERATOR  BINARY_ITERATOR  

template <class F, class BaseIter1, class BaseIter2>
class binary_iterator :
  public extensible_binary_iterator<binary_iterator<F,BaseIter1,BaseIter2>,
				    BaseIter1, BaseIter2,
				    typename two_iterator_traits<BaseIter1,BaseIter2>::iterator_category>,
  public std::iterator<typename two_iterator_traits<BaseIter1,BaseIter2>::iterator_category, typename function_result_type<F>::type>
{
  F mF;
  
public
:
  typedef          binary_iterator<F,BaseIter1,BaseIter2> Host;
  typedef typename two_iterator_traits<BaseIter1,BaseIter2>::iterator_category iterator_category;
  typedef          extensible_binary_iterator<Host,BaseIter1,BaseIter2,iterator_category> extensible_iterator;
  
  binary_iterator(const F& f, const BaseIter1& it1, const BaseIter2& it2)
    : extensible_iterator(it1, it2), mF(f) { }
  
  typedef typename function_result_type<F>::type    value_type;

  //  value_type  operator*()        { return mF(extensible_iterator::first(), extensible_iterator::second()); }
  value_type  operator*() const  { return mF(extensible_iterator::first(), extensible_iterator::second()); }

  using extensible_iterator::operator==;
  using extensible_iterator::operator!=;

};



//  HUNGRY_RANGE  HUNGRY_RANGE  HUNGRY_RANGE  HUNGRY_RANGE  HUNGRY_RANGE  HUNGRY_RANGE  HUNGRY_RANGE

template<class F, class Range>
class hungry_unary_range : public std::unary_function<F,typename unary_range_traits<F,Range>::range>
{
  typename range_traits<Range>::range mRange;
  
 public:
  typedef typename unary_range_traits<F,Range>::range  result_range;
  
  hungry_unary_range(const Range& r) 
    : mRange(make_range(r))  { };
  
  result_range
    operator()(const F& f) const
    {
      return make_unary_range(f, mRange);
    }
};


template<typename X, class Range>
class hungry_function_range: public std::unary_function<X,typename function_range_traits<Range>::range>
{
  typename range_traits<Range>::range mRange;
  
 public:
  typedef typename function_range_traits<Range>::range  result_range;
  
  hungry_function_range(const Range& r) 
    : mRange(make_range(r))  { };
  
  result_range
    operator()(const X& x) const
    {
      return make_function_range(x, mRange);
    }
};


#endif
