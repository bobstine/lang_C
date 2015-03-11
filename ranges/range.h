/*
  Ranges support the protocol (policy) of begin and end returning a
  pair of iterators. These then allow for lazy function evaluation and
  enforced matching of the endpoints of stl algorithms.

  Begin and end support three things

          (a) containers
	  (b) pairs of iterators
	  (c) ranges

   2 Dec 03 ... Remove assignable ranges (just use non-const iterators), protect mBegin, mEnd.
  17 Mar 03 ... Assignable ranges (old form of writeable ranges), range class.
  18 Dec 02 ... Working on const_iterators.
*/

#ifndef _RANGE_H_
#define _RANGE_H_

#include <utility>    // pairs
#include <iterator>   // begin-end functions
#include "iterators.h"  

namespace Ranges 
{

template <class Iter>         class range;
template <typename Container>       range<typename Container::const_iterator> make_range(const Container& c);
template <typename Container>       range<typename Container::iterator>       make_range(Container* c);
template <class Iter>               range<Iter>                               make_range(std::pair<Iter, Iter> r);
template <class Iter>               range<Iter>                               make_range(Iter b, Iter e);
template <class Iter>               range<Iter>                               make_range(range<Iter> r);
template <class Container>          typename Container::const_iterator        begin(const Container& container);
template <class Container>          typename Container::iterator              begin(Container* container);
template <class Iter>               Iter                                      begin(range<Iter> r);
template <class Container>          typename Container::const_iterator        end(const Container& container);
template <class Container>          typename Container::iterator              end(Container* container);
template <class Iter>               Iter                                      end(range<Iter> r);


// RANGE RANGE RANGE RANGE RANGE RANGE RANGE RANGE RANGE RANGE RANGE RANGE RANGE RANGE RANGE

template <class Iter>
class range
{
protected:
  Iter mBegin;
  Iter mEnd;

public:
  //  range ()
  //  : mBegin(0), mEnd(0) { }
  
  range (Iter begin, Iter end)
    : mBegin(begin), mEnd(end) { }
  
  range ( range<Iter> const& r )
    : mBegin(r.mBegin), mEnd(r.mEnd) { }
  
  template<class I2>                           // these constructors allow type conversion between base iterators
    range (I2 begin, I2 end)                     // ??? not sure why there's an advantage to this as a constructor after seeing that
    : mBegin(begin), mEnd(end) { }               //     a range just glues together two arbirary iterators

  template<class I2>
    range (range<I2> const& r)
    : mBegin(begin(r)), mEnd(end(r)) { }
  
  template<class I2> friend I2 begin(range<I2>);
  template<class I2> friend I2 end(range<I2>);
};



// MAKE_RANGE  MAKE_RANGE  MAKE_RANGE  MAKE_RANGE  MAKE_RANGE  MAKE_RANGE  MAKE_RANGE  MAKE_RANGE  

 template <typename Container>
   range<typename Container::const_iterator>
   make_range(Container const& c)
 {
   return range<typename Container::const_iterator>(c.begin(),c.end());
 }
  

 template <typename Container>                       // support for new begin/end functions in C++11
   range<typename Container::const_iterator>
   make_range(Container const& c)
 {
   return range<typename Container::const_iterator>(std::begin(c),std::end(c));
 }
  

 template <typename Container>
   range<typename Container::iterator>
   make_range(Container *c)
 {
   return range<typename Container::iterator>(c->begin(),c->end());
 }
 
 template <class Iter>
   range<Iter>
   make_range(std::pair<Iter, Iter> r)
 {
   return range<Iter>(r.first, r.second);
 }
 
  
 template <class Iter>
   range<Iter>
   make_range(Iter b, Iter e)
 {
   return range<Iter>(b, e);
 }
  

 template <class Iter>
   range<Iter>
   make_range(range<Iter> r)
 {
   return r;
 }
 
 // JOIN   JOIN   JOIN   JOIN   JOIN   JOIN   JOIN   JOIN   JOIN   JOIN   JOIN   JOIN

  
 template <typename I1, typename I2>
   range< JoinIterator<I1, I2> >
   join_ranges(range<I1> r1, range<I2> r2)
 {
   typedef JoinIterator<I1, I2> JoinIter;
   return range<JoinIter> (JoinIter(begin(r1), begin(r2)), JoinIter(end(r1), end(r2)));
 }
 
 


 // BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN BEGIN
 
 
 
 template<class Container>
   inline
   typename Container::const_iterator
   begin(const Container& container)
 {
   return container.begin();
 }
 
 template<class Container>
   inline
   typename Container::iterator
   begin(Container* container)
 {
   return container->begin();
 }

 template<class Iter>
   inline
   Iter
   begin(range<Iter> r)
 {
   return r.mBegin;
 }
 

// END  END  END  END  END  END  END  END  END  END  END  END  END  END  END  END  END  END  END 

template<class Container>
inline
typename Container::const_iterator
end(const Container& container)
{
  return container.end();
}
  
template<class Container>
inline
typename Container::iterator
end(Container* container)
{
  return container->end();
}

template<class Iter>
inline
Iter
end(range<Iter> r)
{
  return r.mEnd;
}

} // namespace ranges

using namespace Ranges;

#endif
