// $Id: range_traits.h,v 1.48 2003/12/02 21:00:25 bob Exp $ -*- c++ -*-

#ifndef _RANGE_TRAITS_H_
#define _RANGE_TRAITS_H_

#include "range.h"

#include <iterator>

  
  template <class Container>
    class range_traits : public std::iterator_traits<typename Container::const_iterator>
  {
  public:
    typedef typename Container::const_iterator const_iterator;
    typedef          range<const_iterator>              range;
  };
  
  
  template <class Container>
    class range_traits<Container *> : public std::iterator_traits<typename Container::iterator>
  {
  public:  
    typedef typename Container::const_iterator const_iterator;
    typedef typename Container::iterator       iterator;
    typedef range<iterator>                    range;
  };
  
  
  
  template <class Iter>
    class range_traits<std::pair<Iter, Iter> > : public std::iterator_traits<Iter>
  {
  public:
    typedef Iter                  const_iterator; 
    typedef range<const_iterator> range;
  };
  
  
  template <class Iter>
    class range_traits<range< Iter > > : public std::iterator_traits<Iter>
  {
  public:
    typedef Iter                  const_iterator;
    typedef range<const_iterator> range;
  };



#endif
