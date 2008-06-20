// $Id: matrix.h,v 1.7 2004/08/25 14:47:09 bob Exp $
#ifndef _MATRIX_H_
#define _MATRIX_H_

/*
  The Matrix class is a vector of pointers to rows.  Each row is held
  as a good old fashioned C array of doubles.  The code understands
  how to interact with ranges.

  The idea is that the matrix knows the number of rows to use.  It
  will always use all n of them. It only uses the number of columns
  determined by the arguments.
  
   7 Feb 03 ... Created to help out logistic regression.

*/

#include "anonymous_iterator.h"
#include "range.h"
#include "range_traits.h"

#include <vector>
#include <numeric>
#include <utility>

class Matrix {
  int mN, mQ, mP;   // using q out of p
  double **mPtr;

 public:
  ~Matrix();

  Matrix (Matrix const& m)
    : mN(m.mN), mQ(m.mQ), mP(m.mP) { allocate(); copy_from(m); }
  Matrix (int n, int p)
    : mN(n), mQ(0), mP(p) { allocate(); }
  Matrix (int n, int p, double x)
    : mN(n), mQ(p), mP(p) { allocate(); fill_with_x(x); }

  int                n_rows () const { return mN;                     }
  std::pair<int,int> n_cols () const { return std::make_pair(mQ, mP); }

  inline double element(int i, int j) const { return mPtr[i][j]; }  // (i=row, j=col)
  
  int           pop_back ()                       { --mQ; return mQ; }
  void          print_to (std::ostream &os) const;

  template<class Range>              int  push_back (Range x);
  
  template<class Range, class Iter> void  Xb        (Range b, Iter dest)   const;

  template<class Range, class Iter> void  Xty       (Range y, Iter dest)   const;
  
  template<class Iter>              void  XtX       (Iter dest)            const;  // write dest (00,01,...0p), (11,12,...,1p) upper rows

  template<class Range, class Iter> void  XtWX      (Range wts, Iter dest) const;  // write dest (00,01,02...0p), (11,12,...,1p) upper rows
  
 private:
  void allocate       ();  
  bool resize         (int nColsToAdd);
  void fill_with_x    (double x);
  void copy_from      (Matrix const& m);
  void delete_pointers();

  Matrix&             operator=(Matrix const&) { return *this; }
};

inline
std::ostream&
operator<< (std::ostream &os, Matrix const& mat)
{ mat.print_to(os); return os; }

#include "matrix.Template.h"

#endif
