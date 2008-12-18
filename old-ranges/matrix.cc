// $Id: matrix.cc,v 1.5 2004/08/25 14:47:09 bob Exp $

#include "matrix.h"
#include <iostream>
#include <algorithm>
#include <iterator>

/// Print

void
Matrix::print_to (std::ostream & os) const
{
  os << "      MATRIX [" << mN << "," << mQ << "/" << mP << "]" << std::endl;
  os << "      "; std::copy(mPtr[0]   , mPtr[0]+mP   , std::ostream_iterator<double>(os, ","));  os << std::endl;
  os << "      "; std::copy(mPtr[1]   , mPtr[1]+mP   , std::ostream_iterator<double>(os, ","));  os << std::endl;
  os << "        ... " << mN-3 << " more rows ...\n";
  os << "      "; std::copy(mPtr[mN-1], mPtr[mN-1]+mP, std::ostream_iterator<double>(os, ","));
  os << std::endl;
}

///  Allocate


Matrix::~Matrix()
{
  delete_pointers();
}

void
Matrix::delete_pointers()
{
  if (mPtr)
  {
    for (int i=0; i<mN; ++i)
      delete mPtr[i];
    delete mPtr;
  }
}

void
Matrix::allocate()
{
  mPtr = new double*[mN];
  for (int i=0; i<mN; ++i)
    mPtr[i] = new double[mP];
}

bool
Matrix::resize (int nColsToAdd)
{
  std::cout << "MATR: Resize from " << mP << " to " << mP + nColsToAdd << " ... ";
  double **pMat = new double*[mN];
  for (int i=0; i<mN; ++i)
    { pMat[i] = new double[mP + nColsToAdd];
      if (pMat[i])
	std::copy(mPtr[i], mPtr[i]+mP, pMat[i]);
      else break;
    }
  if (pMat[mN-1])
  { delete_pointers();
    mP += nColsToAdd;
    mPtr = pMat;
    std::cout << "done." << std::endl;
    return true;
  }
  else
  { std::cout << "fails." << std::endl;
    return false;
  }
}


///  Initialize

void
Matrix::fill_with_x (double x)
{
  for (int i=0; i<mN; ++i)
    for (int j=0; j<mP; ++j)
      mPtr[i][j] = x;
}

///  Copy from another matrix

void
Matrix::copy_from (Matrix const& m)
{
  for (int i=0; i<mN; ++i)
    for (int j=0; j<mP; ++j)
      mPtr[i][j] = m.mPtr[i][j];
}
