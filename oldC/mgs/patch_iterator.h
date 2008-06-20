// $Id: patch_iterator.h,v 1.4 2002/03/16 03:30:07 bob Exp $

#ifndef  _PATCH_ITERATOR_H_
#define  _PATCH_ITERATOR_H_

#include <iterator>

  namespace std
  {
    template <class _Category, class _Tp, class _Distance = ptrdiff_t,
      class _Pointer = _Tp*, class _Reference = _Tp&>
      struct iterator {
        typedef _Category  iterator_category;
        typedef _Tp        value_type;
        typedef _Distance  difference_type;
        typedef _Pointer   pointer;
        typedef _Reference reference;
      };

    /* template <class _Iterator>
       struct iterator_traits {
       typedef typename _Iterator::iterator_category iterator_category;
       typedef typename _Iterator::value_type        value_type;
       typedef typename _Iterator::difference_type   difference_type;
       typedef typename _Iterator::pointer           pointer;
       typedef typename _Iterator::reference         reference;
       };

     
       template <class _Tp>
       struct iterator_traits<_Tp*> {
       typedef random_access_iterator_tag iterator_category;
       typedef _Tp                         value_type;
       typedef ptrdiff_t                   difference_type;
       typedef _Tp*                        pointer;
       typedef _Tp&                        reference;
       };


       template <class _Tp>
       struct iterator_traits<const _Tp*> {
       typedef random_access_iterator_tag iterator_category;
       typedef _Tp                         value_type;
       typedef ptrdiff_t                   difference_type;
       typedef const _Tp*                  pointer;
       typedef const _Tp&                  reference;
       };
    */

  }

#endif
