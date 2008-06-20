// $Id: sparse_iterator.h,v 1.4 2003/06/10 13:46:18 foster Exp $

////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//  Data structure layout of of how a vector<double>(10) would
//  be represented to be used by a sparse_iterator: 
//
//          index:     0   1   2    3       4   5    6   7   8      9
//           data:    [0,  0,  0,   3.141,  0,  0,   0,  0,  2.78,  0]
//
//  representation:    [<0,0>,  <3,3.141>,  <8, 2.78>,  <10,0>]
//
//  NOTE: Notice that we have a "one past end" represented as <10,0>.
//  NOTE: Notice a beginning cell of the array is included.
//
//  An iterator taking on values 0..9 would look like:
//
//  base index iterator=[vector, base_iter,offset]   *(iterator)
//     0           [representation&, 0,  0 ]            0
//     1           [representation&, 1,  2 ]            0
//     2           [representation&, 1,  1 ]            0
//     3           [representation&, 1,  0 ]            3.141
//     4           [representation&, 2,  4 ]            0
//     5           [representation&, 2,  3 ]            0 
//     6           [representation&, 2,  2 ]            0
//     7           [representation&, 2,  1 ]            0
//     8           [representation&, 2,  0 ]            2.78
//     9           [representation&, 3,  1 ]            0
//
//     10          [representation&, 3,  0 ]         illegal
//
//
//
//   OOPS!!!! is <0,0> actually used  ????
//
//
//
////////////////////////////////////////////////////////////////////////////////////////////////////////


#ifndef _SPARSE_ITERATOR_H_
#define _SPARSE_ITERATOR_H_

#include <vector>
#include <utility>
#include <iostream>


#ifndef SPARSE_ITERATOR_TAG
#define SPARSE_ITERATOR_TAG
struct sparse_iterator_tag: public std::random_access_iterator_tag {};
#endif

// This file a along with sparse_iterator.Template.h define the following items:

template <class Key, class X> class const_sparse_iterator;
template <class Key, class X>       std::ostream&                 operator<<(std::ostream& out,const_sparse_iterator<Key,X> i);
template <class Key, class X>       const_sparse_iterator<Key,X>  begin(const typename std::vector<std::pair<Key,X> >& map);
template <class Key, class X>       const_sparse_iterator<Key,X>  end  (const typename std::vector<std::pair<Key,X> >& map);
template<class X>             class sparse_back_insert_iterator;




template <class Key, class X>
std::ostream&
operator<<(std::ostream& out,const_sparse_iterator<Key,X> i)
{
  i.print_on(out);
  return out;
};

template <class Key, class X>
const_sparse_iterator<Key,X>
begin(const typename std::vector<std::pair<Key,X> >& map)
{
  //  typename std::vector<std::pair<Key,X> >::const_iterator b = map.begin(); 
  return const_sparse_iterator<Key,X>(map); // needs typename above for g++ 3.2.
};

template <class Key, class X>
const_sparse_iterator<Key,X>
end(const typename std::vector<std::pair<Key,X> >& map)
{
  int last_index = (*(map.rbegin())).first;
  return const_sparse_iterator<Key,X>(map,last_index);
};


        //////////////////////////////////////////////////////////////////////////////////////////
       //                                                                                      //
      // The meat of this code is hidden in:                                                  //
     //                                                                                      //
    //                                                                                      //
#include "sparse_iterator.Template.h"
  //                                                                                      //
 //                                                                                      //
//////////////////////////////////////////////////////////////////////////////////////////



         //////////////////////////////////////////////////////////////////////////////////////////
        //                                                                                      //
       //                                                                                      //
      //      The following don't have a natural home.  They could be put                     //
     //      in range and range_traits.  Or they could be put here.  I think they            //
    //      belong here, but this part of the code won't run unless range                   //
   //      is defined.                                                                     //
  //                                                                                      //
 //                                                                                      //
//////////////////////////////////////////////////////////////////////////////////////////


template <class Range>
class range_traits;

template <class Iter>
class range;

template <class X>
class range_traits<std::vector<std::pair<int,X> > >
{
public:
  typedef const_sparse_iterator<int,X> const_iterator;
  typedef const_iterator iterator;          // we were sloppy
  typedef range<const_iterator> range;
  typedef sparse_iterator_tag iterator_category;
  typedef X value_type;
  typedef int difference_type;
  typedef typename const_iterator::advance_to_next_non_zero next_type;

  /*  I dont' think the following two can be sensable defined
  typedef typename std::iterator_traits<iterator>::pointer pointer;
    typedef typename std::iterator_traits<iterator>::reference reference;
  */
};

// MAKE_RANGE  MAKE_RANGE  MAKE_RANGE  MAKE_RANGE  MAKE_RANGE  MAKE_RANGE  MAKE_RANGE  MAKE_RANGE  

template <typename value_type>
range<const_sparse_iterator<int,value_type> >
make_sparse_range(const std::vector<std::pair<int,value_type> >& c)
{
  return range<const_sparse_iterator<int,value_type> >(begin(c),end(c));
};

template<class X>
class sparse_back_insert_iterator;

#endif
