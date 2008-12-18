// $Id: matrix.test.cc,v 1.6 2004/04/08 17:44:36 bob Exp $


#include "matrix.h"
#include "print_utils.h"

#include <iostream>

int
main()
{
  const size_t nRows (10);
  const size_t nCols (2);
  
  // init
  Matrix m(nRows, nCols, 1.1);
  std::cout << "m[2,1] = " << m.element(2,1) << std::endl;
  std::cout << "Whole matrix " << m << std::endl;

  //  copy of a matrix
  Matrix copy(m);
  std::cout << "Copy of m " << copy << std::endl;

  // weighted sum of each row
  std::vector<double> b(nCols);
  std::vector<double> xb(nRows);
  b[0] = 1.0;
  b[1] = 1.0;
  m.Xb(b,xb.begin());
  std::cout << "XB " << xb << std::endl;

  // accumuate sum of products over rows
  std::vector<double> xty(nCols);
  std::vector<double> y  (nRows);
  std::fill(y.begin(), y.end(), 1.0);
  xty[1] = 100.; // should overwrite with zero
  m.Xty(y, xty.begin());
  std::cout << "xty " << xty << std::endl;

  // append column forcing resize
  std::vector<double> col (nRows);
  std::fill(col.begin(), col.end(), 7.0);
  m.push_back(col);
  std::cout << m << std::endl;

  //  now get the vector form of upper half of XtX
  int q (m.n_cols().first);
  std::vector<double> xtx((q*(q+1))/2);
  m.XtX(xtx.begin());
  std::cout << "xtx " << xtx << std::endl;
  std::vector<double> wts(nRows);
  std::fill(wts.begin(), wts.end(), 2.0);
  m.XtWX(wts,xtx.begin());
  std::cout << "xtwx " << xtx << std::endl;

  return 0;
}
