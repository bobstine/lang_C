/* $Id: range.h,v 1.2 2002/06/17 14:35:09 bob Exp $ -*- c++ -*-
 
   Ranges
 
   23 Mar 02 ... Compile under GCC 3 to settle template issues.
   19 Feb 02 ... Get composer working in style that I wanted.
   10 Feb 02 ... Remove argument iterator type in favor of unary, binary iterators.
   11 Jan 02 ... Created to support factored statistics code.

*/

#ifndef _RANGE_H_
#define _RANGE_H_

// #ifdef __APPLE__
// #include "patch_iterator.h"
// #endif

#include "compose.h"
#include "range_traits.h"

#include <utility>
#include <functional>
#include <vector>
#include <assert.h>

//////////////////////////////   Composition of ranges  ///////////////////////////////////

template<class F>
class Composer {
  F mf;
public:
  Composer(const F& f) : mf(f) { }
  template<class G>
  boost::compose_f_gx_t<F,G>  operator()(const G& g) const
  { return boost::compose_f_gx_t<F,G>(mf,g); }
};

template<class F>
Composer<F> make_composer(const F& f) 
{ return Composer<F>(f); }


template<class F, class G>
class operator_traits {
  typedef typename F::result_type result_type;
};

template<class F, class G>
class operator_traits <Composer<F>,G> {
  typedef typename boost::compose_f_gx_t<F,G> result_type;
};



////////////////////////////  Convenience creator functions  /////////////////////////

template <class Container>
typename range_traits<Container>::range
make_range(const Container& c)
{
  return make_pair(begin(c), end(c));
}


template <class Iter>
std::pair<Iter,Iter>
make_range(std::pair<Iter,Iter> range)
{
  return range;
}

template <class Iter>
std::pair<const Iter, const Iter>
make_range(std::pair<const Iter, const Iter> range)
{
  return range;
}


//////////////////////////////   Unary Iterator  ///////////////////////////////////

// the following two classes are "policy classes" (page 8 Andrei Alexandrescu)

template<class Constant, class I>
class operator_star_unary_function
{
 protected:
  I mIter;
  Constant mOp;
 public:
  typedef typename std::iterator_traits<I> I_traits;
  typedef operator_star_unary_function<Constant,I> type_of_this;
  typedef typename operator_traits<Constant, typename I_traits::value_type>::result_type result_type;

  operator_star_unary_function(const Constant& op,const I& iter)
    :
    mIter(iter),
    mOp(op)
    {
    }

  // the following is a "copy constructor" usable by operator++(int) and operator+(int)
  operator_star_unary_function(const type_of_this& other,const I& iter)
    :
    mIter(iter),
    mOp(other.mOp)
    {
    }
  
  result_type
  operator*() const
  {
    return mOp(*mIter);
  }
};


template<class I>
class operator_star_function_iterator
{
 public:
  typedef          operator_star_function_iterator<I> type_of_this;
  typedef typename std::iterator_traits<I> I_traits;
  typedef typename I_traits::value_type F;
  typedef typename F::argument_type Constant;
  typedef typename F::result_type result_type;

protected:
  I mIter;
  Constant mX;
public:


  operator_star_function_iterator(const Constant& X,const I& iter)
    :
    mIter(iter),
    mX(X)
    {
    }

  // the following is a "copy constructor" usable by operator++(int) and operator+(int)
  operator_star_function_iterator(const type_of_this& other,const I& iter)
    :
    mIter(iter),
    mX(other.mX)
    {
    }
  
  result_type
  operator*() const
  {
    return (*mIter)(mX);
  }
};

//template<class UnaryOp, class I, class Tag,class Operator_star=operator_star_unary_function<UnaryOp,I> >
template<class UnaryOp, class I, class Tag,class Operator_star>
class unary_iterator;

template<class UnaryOp, class I, class Operator_star>
class unary_iterator<UnaryOp,I,std::forward_iterator_tag,Operator_star>
  : public Operator_star,
    public std::iterator<typename std::iterator_traits<I>::iterator_category,
                         typename Operator_star::result_type>
{
 public:
  typedef std::forward_iterator_tag tag;
  typedef unary_iterator<UnaryOp,I,tag,Operator_star> iterator;
  typedef std::pair<iterator,iterator> range;

  unary_iterator(const UnaryOp& op, const I& it)
    : Operator_star(op,it)
  {
  }

  iterator& operator++() { ++mIter; return *this; }

  bool operator!=(const iterator& it) const { return mIter != it.mIter; }
  bool operator==(const iterator& it) const { return mIter == it.mIter; }

private:
  unary_iterator() {}
  //  void operator=(const iterator&); // need to allow for std STL functions like min_element?

  //  unary_iterator(const iterator&);
};

template<class UnaryOp, class I,class Operator_star>
class unary_iterator<UnaryOp,I,std::bidirectional_iterator_tag,Operator_star>
 : public unary_iterator<UnaryOp,I,std::forward_iterator_tag,Operator_star>
{
 public:
  typedef std::bidirectional_iterator_tag tag;
  typedef unary_iterator<UnaryOp,I,tag,Operator_star> iterator;
  typedef std::pair<iterator,iterator> range;

  unary_iterator(const UnaryOp& op, I it)
    : unary_iterator<UnaryOp,I,std::forward_iterator_tag,Operator_star>(op,it) { }

  iterator&
    operator--() { --mIter; return *this; }

private:
  unary_iterator() { }
};


template<class UnaryOp, class I,class Operator_star>
class unary_iterator<UnaryOp, I,std::random_access_iterator_tag,Operator_star>
  : public unary_iterator<UnaryOp,I,std::bidirectional_iterator_tag,Operator_star>
{
 public:
  typedef std::random_access_iterator_tag tag;
  typedef unary_iterator<UnaryOp,I,tag,Operator_star> iterator;
  typedef std::pair<iterator,iterator> range;

  unary_iterator(const UnaryOp& op, I it)
    : unary_iterator<UnaryOp,I,std::bidirectional_iterator_tag,Operator_star>(op,it) { }  // OOPS!!!!!

  iterator
    operator+(const int n)  const
  {
    return iterator(mOp,mIter+n);
  }

  iterator
    operator-(const int n) const
  {
    return iterator(mOp,mIter-n);
  }

  unary_iterator&
    operator+=(const int n) { mIter += n; return *this; }
 
  unary_iterator&
    operator-=(const int n) { mIter -= n; return *this; }

  typename std::iterator_traits<I>::difference_type
    operator-(const unary_iterator& it) { return mIter - it.mIter; }

  bool
    operator<(const unary_iterator& it) const { return mIter < it.mIter; }
  bool
    operator>(const unary_iterator& it) const { return mIter > it.mIter; }

private:
  unary_iterator () {}
};

//  IDEAL:
//     range::make(...)
//     make_range(f,X_range);        // usual unary_iterator
//     make_range(f,X1_range,X2_range); // binary_iterator
//     make_range(f_range,x);        // makes function_iterator (or other_iterator)
//     make_range(f_range,x_range);  // makes table
//
// BARFS ON: (undefined, ideally compile time error)
//    make_range(f,x);

template <class UnaryOp, class Range>
typename unary_range_traits<UnaryOp,Range>::range
make_unary_range (UnaryOp fff, const Range& range)
{
  typedef unary_range_traits<UnaryOp,Range> traits;
  typedef typename traits::iterator iterator;
  return typename traits::range(iterator(fff, begin(range)), iterator(fff, end(range)) );
}

template <class Range>
typename unary_other_range_traits<Range>::range
make_other_unary_range (const Range& range, typename range_traits<Range>::value_type::argument_type x)
{
  typedef unary_other_range_traits<Range> traits;
  typedef typename traits::iterator iterator;
  return traits::range(iterator(x, begin(range)), iterator(x, end(range)));
}


namespace{
  template<class UnaryOp, class Range>
  class Apply_function: public std::unary_function<UnaryOp,typename unary_range_traits<UnaryOp,Range>::range>
  {
    typename range_traits<Range>::range mRange;
  public:
    typedef typename unary_range_traits<UnaryOp,Range>::range  Result_range;

    Apply_function(Range range) 
      : mRange(make_range(range))  { };
  
    Result_range
    operator()(const UnaryOp& f) const
    {
      return make_unary_range(f,mRange);
    };
  };
}

template <class UnaryOp, class Range>
Apply_function<UnaryOp,Range>
make_unary_range (const Range& range)
{
  return Apply_function<UnaryOp,Range>(range);
}


//////////////////////////////   Binary Iterator  ///////////////////////////////////

template<class BinaryOp, class I1, class I2>
class forward_binary_iterator : public std::iterator<typename std::iterator_traits<I1>::iterator_category,
                                                typename BinaryOp::result_type>
{
 protected:
  I1 mIter1;
  I2 mIter2;
  BinaryOp mOp;

 public:
  typedef typename BinaryOp::result_type value_type;
  typedef std::pair<forward_binary_iterator<BinaryOp,I1,I2>,forward_binary_iterator<BinaryOp,I1,I2> > range;

  forward_binary_iterator(BinaryOp op, I1 it1, I2 it2)
    : mIter1(it1), mIter2(it2), mOp(op) { }

  //  void operator++() { ++mIter1; ++mIter2;  }  // removing the return doesn't effect speed (as it shouldn't)
  forward_binary_iterator& operator++() { ++mIter1; ++mIter2; return *this; }
  value_type operator*() const { return mOp(*mIter1, *mIter2); }

  bool operator!=(const forward_binary_iterator& it) const
    { return mIter1 != it.mIter1 || mIter2 != it.mIter2; }
  bool operator==(const forward_binary_iterator& it) const
    { return mIter1 == it.mIter1 && mIter2 == it.mIter2; }

private:
  forward_binary_iterator() {}
};


template<class BinaryOp, class I1, class I2>
class bidirectional_binary_iterator : public forward_binary_iterator<BinaryOp,I1,I2>
{
 public:
  typedef std::pair<bidirectional_binary_iterator<BinaryOp,I1,I2>,
               bidirectional_binary_iterator<BinaryOp,I1,I2> > range;

  bidirectional_binary_iterator(BinaryOp op, I1 it1, I2 it2)
    : forward_binary_iterator<BinaryOp,I1,I2>(op,it1,it2) { }

  bidirectional_binary_iterator&
    operator--() { --mIter1; --mIter2; return *this; }
private:
  bidirectional_binary_iterator() { }
};


template<class BinaryOp, class I1, class I2>
class random_access_binary_iterator : public bidirectional_binary_iterator<BinaryOp,I1,I2>
{
 public:
  typedef std::pair<random_access_binary_iterator<BinaryOp,I1,I2>,
               random_access_binary_iterator<BinaryOp,I1,I2> > range;

  random_access_binary_iterator(BinaryOp op, I1 it1, I2 it2)
    : bidirectional_binary_iterator<BinaryOp,I1,I2>(op,it1,it2) { }

  random_access_binary_iterator
  operator+(const int n) 
  {
    return random_access_binary_iterator<BinaryOp,I1,I2>(mOp,mIter1+n,mIter2+n);
  }

  random_access_binary_iterator
  operator-(const int n) 
  {
    return random_access_binary_iterator<BinaryOp,I1,I2>(mOp,mIter1-n,mIter2-n);
  }

  random_access_binary_iterator&
  operator+=(const int n) { mIter1 += n; mIter2 += n; return *this; }
  random_access_binary_iterator&
    operator-=(const int n) { mIter1 -= n; mIter2 -= n; return *this; }

  typename std::iterator_traits<I1>::difference_type
    operator-(const random_access_binary_iterator& it) { return mIter1 - it.mIter1; }

  bool
    operator<(const random_access_binary_iterator& it) const { return mIter1 < it.mIter1; }
  bool
    operator>(const random_access_binary_iterator& it) const { return mIter1 > it.mIter1; }

private:
  random_access_binary_iterator () {}
  
};

template <class BinaryOp, class Range1, class Range2>
typename binary_range_traits<BinaryOp,  Range1,  Range2>::range
make_binary_range (BinaryOp f, const Range1& range1, const Range2& range2)
{
  typedef typename binary_range_traits<BinaryOp,  Range1,  Range2>::iterator iterator;
  
  return make_pair(iterator(f,begin(range1),begin(range2)),
		   iterator(f,end  (range1),end  (range2)));
}


//////////////////////////  Function Iterator  ///////////////////////////////////


template<class I>
class forward_function_iterator : public std::iterator<typename std::iterator_traits<I>::iterator_category,
                                           typename std::iterator_traits<I>::value_type::result_type>
{
  typedef typename std::iterator_traits<I>::value_type F;
 protected:
  I mIter;
  const typename F::argument_type mX;  // could use reference for 'big' types

 public:
  typedef typename F::result_type value_type;
  typedef std::pair<forward_function_iterator<I>,forward_function_iterator<I> > range;

  forward_function_iterator(I it, const typename F::argument_type x)
    : mIter(it), mX(x) { } 

  forward_function_iterator& operator++() { ++mIter; return *this; }
  value_type operator*() const
  {
    return (*mIter)(mX);
  };

  bool operator!=(const forward_function_iterator& it) const { return mIter != it.mIter; }
  bool operator==(const forward_function_iterator& it) const { return mIter == it.mIter; }

private:
  forward_function_iterator() {}
};


template<class I>
class bidirectional_function_iterator : public forward_function_iterator<I>
{
 public:
  typedef std::pair<bidirectional_function_iterator<I>, bidirectional_function_iterator<I> > range;

  bidirectional_function_iterator(I it, const typename F::argument_type x)
    : forward_function_iterator<I>(it,x) { }

  bidirectional_function_iterator&
    operator--() { --mIter; return *this; }

private:
  bidirectional_function_iterator() { }

};


template<class I>
class random_access_function_iterator : public bidirectional_function_iterator<I>
{
 public:
  typedef std::pair<random_access_function_iterator<I>, random_access_function_iterator<I> > range;

  random_access_function_iterator(I it, const typename F::argument_type x)
    : bidirectional_function_iterator<I>(it,x) { }

  random_access_function_iterator
  operator+(const int n) 
  {
    return random_access_function_iterator<I>(mIter+n, mX);
  }

  random_access_function_iterator
  operator-(const int n) 
  {
    return random_access_function_iterator<I>(mIter-n,mX);
  }

  random_access_function_iterator&
  operator+=(const int n) { mIter += n; return *this; }
  random_access_function_iterator&
    operator-=(const int n) { mIter -= n; return *this; }

  typename std::iterator_traits<I>::difference_type
    operator-(const random_access_function_iterator& it) { return mIter - it.mIter; }

  bool
    operator<(const random_access_function_iterator& it) const { return mIter < it.mIter; }
  bool
    operator>(const random_access_function_iterator& it) const { return mIter > it.mIter; }

private:
  random_access_function_iterator () {}
  
};

template <class Range>
typename function_range_traits<Range>::range
make_function_range (const Range& range, typename range_traits<Range>::value_type::argument_type x)
{
  typedef typename function_range_traits<Range>::iterator iterator;
  
  return make_pair(iterator(begin(range),x), iterator(end  (range),x));
};


namespace{
  template<class Range>
  class Apply_range: public std::unary_function<typename function_range_traits<Range>::argument_type,
                                                typename function_range_traits<Range>::range>
  {
    Range mRange;
  public:
    typedef typename function_range_traits<Range>::iterator iterator;
    typedef std::pair<iterator,iterator> Result_range;
  
    Apply_range(Range range) 
      :
      mRange(range)
    {
    };
  
    Result_range
    operator()(typename range_traits<Range>::value_type::argument_type x) const
    {
      return make_function_range(mRange,x);
    };
  };
}

template <class Range>
Apply_range<Range>
make_function_range (const Range& range)
{
  return Apply_range<Range>(range);
}



#endif
