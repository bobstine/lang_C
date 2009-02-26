/* $Id: make_range.h,v 1.1 2003/11/26 14:11:50 bob Exp $ -*- c++ -*-
 
   Ranges

   23 Apr 03 ... Add code to handle combinations of scalars and ranges.
   20 Mar 03 ... Move out the iterators and functional tools like composer, evaluator.
   17 Mar 03 ... New version of make_range that makes a range class rather than pair.
   25 Jun 02 ... New factored version of extensible iterators and operator*
   23 Mar 02 ... Compile under GCC 3 to settle template issues.
   19 Feb 02 ... Get composer working in style that I wanted.
   10 Feb 02 ... Remove argument iterator type in favor of unary, binary iterators.
   11 Jan 02 ... Created to support factored statistics code.

*/

#ifndef _MAKE_RANGE_H_
#define _MAKE_RANGE_H_

#include "range.h"
#include "range_traits.h"
#include "composer.h"
#include "evaluator.h"
#include "function_iterators.h"






// FUNCTION_RANGE_TRAITS   FUNCTION_RANGE_TRAITS   FUNCTION_RANGE_TRAITS   FUNCTION_RANGE_TRAITS

template<typename X, class Range>
class function_iterator;

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
  typedef          range<const_iterator> range;
};


// SPARSE_RANGE_TRAITS   SPARSE_RANGE_TRAITS   SPARSE_RANGE_TRAITS   SPARSE_RANGE_TRAITS

template <class Sparse_range>
class sparse_range_traits
{
public:
  typedef typename Sparse_range::first_type::next_type next_type;
};


/*
template <class F, class Range1, class Range2>
wrapper_class_traits<class F, class Range1, class Range2>
{
  typedef recursive_multiplier_traits<F,F::first_argument_type,second_argument_type,Range1,Range2> traits;
  typedef traits::operator_type operator_type;
}



  template <class F, class Arg1, class Arg2, class Range1, class Range2>
class recursive_multiplier_traits 
{
private:
    typedef recursive_multiplier_traits<F,F::first_argument_type,
					second_argument_type,
					Range1::value_type,
					Range2::value_type> traits;

  typedef traits::result_type recursive_operator_type;
public:
  typedef binary_range<recursive_operator_type,Range1,Range2> operator_type;
};


template <class F, class Arg1, class Arg2>
class recursive_multiplier_traits<F,Arg1,Arg2,Arg1,Arg2>
{
  typedef F operator_type;
};

template <class F, class Arg1, class Arg2, class Range1>
class recursive_multiplier_traits<F,Arg1,Arg2,Range1,Arg2>
{
  std::string class_name(){return "bind second";};
};


template <class F, class Arg1, class Arg2, class Range2>
class recursive_multiplier_traits<F,Arg1,Arg2,Arg1,Range2>
{
  std::string class_name(){return "bind first";};
  
};
*/







// FUNCTORS: MAKE_RANGE

namespace range_namespace
{


  // I think the following class is stupid.  I can't think of why it should possibly exist
  // but for now I'm leaving it since I think everything should be a function object.
  // When we figure out how to actually use this code, this might be removed.
  template<class Container>
  class Make_range: public std::unary_function<Container, range<typename range_traits<Container>::const_iterator> >
  {
  public:

    // adding typename to the following seems to cause g++ to crash.
    // Whereas not having it simply generates a warning

    // typename result_type
    range<typename range_traits<Container>::const_iterator>
    operator()(const Container& c)
    {
      return result_type(begin(c),end(c));
    }
  };
};






// FUNCTORS: MAKE_UNARY_RANGE

namespace range_namespace
{
  template<class F, class Range>
  class Make_unary_range: public std::binary_function<F,
						      Range,
						      typename unary_range_traits<F,Range>::range>
  {
    typedef unary_range_traits<F,Range> trait;

  public:
    // adding typename to the following seems to cause g++ to crash.
    // Whereas not having it simply generates a warning

    // typename result_type
    typename trait::range
    operator()(const F&f, const Range& r) 
    {
      return result_type(trait::const_iterator(f, begin(r)), trait::const_iterator(f, end(r)));
    }
  };
}
//  MAKE_UNARY_RANGE  MAKE_UNARY_RANGE  MAKE_UNARY_RANGE  MAKE_UNARY_RANGE  MAKE_UNARY_RANGE  


template <class F, class Range>
typename range_namespace::Make_unary_range<F,Range>::result_type
make_unary_range (const F& f, const Range& r)
{
  //  typedef unary_range_traits<F,Range> traits;
  //  typedef typename traits::const_iterator const_iterator;
  //  return typename traits::range(const_iterator(f, begin(r)), const_iterator(f, end(r)) );
  return range_namespace::Make_unary_range<F,Range>()(f,r);
}

//********************************************************************************************
//
//               start experimental code
//
//********************************************************************************************


//  MAKE_STATELESS_UNARY_RANGE      MAKE_STATELESS_UNARY_RANGE      MAKE_STATELESS_UNARY_RANGE  

namespace range_namespace
{
  // The following is an attempt to have sin(vector<vector<double> >) pass the
  // sin down to the lowest level.  It would be called something like:
  //
  //                   vector<range<vector<double> >::range> foo
  //                   make_stateless_unary_range(sin(), make_range(foo));
  //  

  class True;
  class False;

  template<class T1, class T2>
  class match
  {
  public:
    //    typedef typename std::pair<T1,T2>::should_not_be_a_match check;
    typedef False result;
  };

  template<class T1>
  class match<T1,T1>
  {
  public:
    //    typedef typename T1::matched_by_definitin check;
    typedef True result;
  };

  template<class T> class Not;

  template<>
  class Not<True>
  {
    typedef False result;
  };

  template<>
  class Not<False>
  {
    typedef True result;
  };

  // The following will primarilly deliver operator() applied to a range
  template<class F, class Range> class Make_stateless_unary_range;

  // The following should be a member template--but that doesn't seem to be working
  // It will be defined after the main class.  If there is a match between
  // F::argument_type and Range, it will deliver F for its op.  Otherwise
  // it computes a recursive object.

  template<class F, class Range, class T> class recursive_case;

  template<class F, class Range_or_arg>
  class stateless_unary_range_traits
  {
  protected:
    typedef typename match<typename F::argument_type,Range_or_arg>::result args_match;
    typedef typename Not<args_match>::result        recursive_case_p;

  public:
    typedef recursive_case<F, Range_or_arg, recursive_case_p> recursive;
    typedef typename recursive::const_iterator   const_iterator;
    typedef typename recursive::recursive_op     recursive_op;
    typedef typename recursive::this_op          this_op;
    typedef range<const_iterator>                range;
  };

  template<class F, class Range>
  class Make_stateless_unary_range: public std::unary_function<Range,
							       typename stateless_unary_range_traits<F,Range>::range>
  {
    typedef stateless_unary_range_traits<F,Range> trait;
  public:
    typename trait::range
    operator()(const Range& r) 
    {
      return result_type(typename trait::const_iterator(typename trait::recursive_op(), begin(r)),
			 typename trait::const_iterator(typename trait::recursive_op(), end(r)  ));
    }
  };



  template<class F, class Range>
  class recursive_case<F,Range,True>
  {
    typedef          range_traits<Range>            base_traits;
    typedef typename base_traits::const_iterator    const_base_iterator;
    typedef typename base_traits::iterator_category base_category;
    typedef typename base_traits::value_type        value_type;
    typedef typename stateless_unary_range_traits<F,value_type>::this_op recursive_op;
    typedef Make_stateless_unary_range<F,Range>     this_op;

    typedef unary_iterator<recursive_op, const_base_iterator, base_category> const_iterator;
  };

  // First the simple case
  template<class F, class Range>
  class recursive_case<F,Range,False>
  {
    //    typedef          range_traits<Range>            base_traits;
    //    typedef typename base_traits::const_iterator    const_base_iterator;
    //    typedef typename base_traits::iterator_category base_category;

    //    typedef          unary_iterator<F, const_base_iterator, base_category> const_iterator;
    typedef          False const_iterator;
    typedef          F     this_op;
    typedef          False recursive_op;
  };

};

template <class F, class Range>
typename range_namespace::Make_stateless_unary_range<F,Range>::result_type
make_stateless_unary_range (const Range& r)
{
  return range_namespace::Make_stateless_unary_range<F,Range>()(r);
}

//********************************************************************************************
//
//               end experimental code
//
//********************************************************************************************


// MAKE_BINARY_RANGE  MAKE_BINARY_RANGE  MAKE_BINARY_RANGE  MAKE_BINARY_RANGE  MAKE_BINARY_RANGE

template <class F, class R1, class R2>
typename binary_range_traits<F,R1,R2>::range
make_binary_range (const F& f, const R1& r1, const R2& r2)
{
  typedef typename range_traits<R1>::range Range1;
  typedef typename range_traits<R2>::range Range2;
  Range1 range1 (make_range(r1));
  Range2 range2 (make_range(r2));
  typedef binary_range_traits<F,Range1,Range2> traits;
  typedef typename traits::const_iterator const_iterator;
  return typename traits::range( const_iterator(f, begin(range1), begin(range2)),
				 const_iterator(f,   end(range1),   end(range2)) );
}




//  MAKE_FUNCTION_RANGE  MAKE_FUNCTION_RANGE  MAKE_FUNCTION_RANGE  MAKE_FUNCTION_RANGE


template <typename X, class Range>
typename function_range_traits<Range>::range
make_function_range (const X& x, const Range& r)
{
  typedef function_range_traits<Range> traits;
  typedef typename traits::const_iterator const_iterator;
  return typename traits::range(const_iterator(x, begin(r)), const_iterator(x, end(r)) );
}


//  HUNGRY_RANGES  HUNGRY_RANGES  HUNGRY_RANGES  HUNGRY_RANGES  HUNGRY_RANGES  HUNGRY_RANGES  

/*
  The hungry_range class allows you to form a unary range in a
  slightly different syntax.  Rather than bind the function to the
  range using make_range, a hungry_range object is a function object
  holding the range, looking for a function to evaluate on that range.

  This form is particularly useful in forming tables as the
  cross-product of two ranges.

  The specific function in the unary range is instantiated by the
  operator() function, when then returns a unary_range.  As a result,
  we have a version of make_unary_range that takes only a range as its
  argument.

  This returns a 'range looking for a function' object that may then
  be applied to various functions.
*/

namespace{
  template<class F, class Range>
  class hungry_unary_range: public std::unary_function<F,typename unary_range_traits<F,Range>::range>
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
    };
  };
}

template <class UnaryOp, class Range>
hungry_unary_range<UnaryOp,typename range_traits<Range>::range>
make_unary_range (const Range& r)
{
  return hungry_unary_range<UnaryOp,typename range_traits<Range>::range>(make_range(r));
}


namespace{
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
    };
  };
}

template <typename X, class Range>
hungry_function_range<X,Range>
make_function_range (const Range& r)
{
  return hungry_function_range<X,Range>(r);
}

#endif