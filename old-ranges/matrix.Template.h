// $Id: matrix.Template.h,v 1.2 2005/06/14 21:14:24 bob Exp $

#include <iostream>

template<class Range>
int
Matrix::push_back (Range x)
{
  if (mQ == mP && resize(10))
    std::cout << "MATR: Resizing to append column" << std::endl;
  typename range_traits<Range>::const_iterator xIter (begin(x));
  for (int i=0; i<mN; ++i, ++xIter)
    mPtr[i][mQ] = *xIter;
  ++mQ;
  return mQ;
}

template<class Range, class Iter>
void
Matrix::Xb(Range b, Iter dest) const
{
  for (int i=0; i<mN; ++i)
    { *dest = std::inner_product(begin(b), end(b), mPtr[i], 0.0);
    ++dest;
    }
}

template<class Range, class Iter>
void
Matrix::Xty(Range y, Iter dest) const
{
  { Iter col (dest);   // zero dest range
  for(int j=0; j<mQ; ++j, ++col)
    *col = 0.0;
  }
  typename range_traits<Range>::const_iterator yIter (begin(y));
  for (int i=0; i<mN; ++i, ++yIter)
    { double *row (mPtr[i]);
    Iter col (dest);
    for (int j=0; j<mQ; ++j, ++col)
      *col = *col + *yIter * row[j];
    }
}		 

template<class Iter>
void
Matrix::XtX(Iter dest) const  // write dest (00,01,02...0p), (11,12,...,1p) upper rows
{
  {
    Iter xx (dest);   // zero dest range
    for(int j=0; j<(mQ*(mQ+1))/2; ++j, ++xx)
      *xx = 0.0;
  }
  for (int k=0; k<mQ; ++k)
    { 
      for (int i=0; i<mN; ++i)
        { double *row (mPtr[i]);
	Iter xx (dest);
	for (int j=k; j<mQ; ++j, ++xx)
	  *xx = *xx + row[k] * row[j];
	}
      dest = dest + mQ-k;
    }
}		 


template<class Range, class Iter>
void
Matrix::XtWX(Range wts, Iter dest) const  // write dest (00,01,02...0p), (11,12,...,1p) upper rows
{
  {
    Iter xx (dest);   // zero dest range
    for(int j=0; j<(mQ*(mQ+1))/2; ++j, ++xx)
      *xx = 0.0;
  }
  for (int k=0; k<mQ; ++k)
    { typename range_traits<Range>::const_iterator w (begin(wts));
    for (int i=0; i<mN; ++i,++w)
      { double *row (mPtr[i]);
      Iter xx (dest);
      for (int j=k; j<mQ; ++j, ++xx)
	*xx = *xx + *w * row[k] * row[j];
      }
    dest = dest + mQ-k;
    }
}
