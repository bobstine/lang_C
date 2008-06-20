// $Id: cyclic.h,v 1.1 2000/02/04 19:36:03 bob Exp $-*- c++ -*-
#ifndef _cyclic_
#define _cyclic_

#include <vector.h>


/*

  2 Feb 00
  Only supports ++ and * operators, so you can only use these methods
  to walk a double vector in a forward direction.

*/

class Cyclic_iterator
{
public:
  Cyclic_iterator(vector<double>::iterator begin,
		  vector<double>::const_iterator end):
    mBegin (begin),
    mEnd (end),
    mCurrent(begin)
    {
    }
  
    Cyclic_iterator& operator++();
    double& operator*();
  
private:
  vector<double>::iterator mBegin;
  vector<double>::const_iterator mEnd;
  vector<double>::iterator mCurrent;
};

#endif
