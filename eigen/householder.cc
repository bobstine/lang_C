#include "householder.h"



//     initialize     initialize     initialize     initialize

void
Householder::allocate()
{
  mVRPtr = new StorageType[mRows*mCols];
  // zero space
  for (int i = 0; i < mRows*mCols; ++i)
    mVPtr[i] = 0.0;
}

//     access     access     access     access     access     

Householder::Matrix
Householder::R() const
{
  Matrix VR = Matrix::Map(mVRPtr,mCols,mCols);
  return VR.corner(Eigen::TopLeft, mXNumCols, mXNumCols);
}

//     calculation       calculation       calculation       calculation

double
Householder::house(int j)       //  rotate V[,j] into {0,0,1,0,0,0...0} vector
{
  std::cout << "HHDR: Computing rotation using vector V[" << j << "]" << std::endl;
  Eigen::Map<Matrix> V (mVRPtr,mRows,mCols);
  double      partialNorm2 = V.col(j).end(mRows-j-1).squaredNorm();
  StorageType v0           = V(j,j);
  double      norm2        = partialNorm2 + v0*v0;
  if (norm2 == 0.0)
    return 0.0;
  V(j,j) = (v0 < 0.0) ? (v0 - norm2) : ((-partialNorm2)/(v0+sqrt(norm2)));
  double Vjj2 (V(j,j)*V(j,j));
  mBeta[j] = 2 * Vjj2/(partialNorm2 + Vjj2);
  
  std::cout << "TEST:  j=" << j << "   v0=" << v0 << "  partial norm " << partialNorm2  << "   norm " << norm2 <<  std::endl;
  return v0*v0 + partialNorm2;                       // returns squared norm of input column
}


Householder::Vector
Householder::apply_transform(int column)
{
  Eigen::Map<Matrix> V (mVPtr,mRows,mCols);
  Vector r (column);
  for (int k = 0; k < column; ++k)
  { r[k] = (2 * V.col(k).dot(V.col(column))/mVtV[k]);
    V.col(column) = V.col(column) - r[k] * V.col(k);
  }
  return r;
}

void
Householder::add_vector(Vector const& x)
{
  Eigen::Map<Matrix> V (mVPtr,mRows,mCols);
  Eigen::Map<Matrix> R (mRPtr,mCols,mCols);
  // sweep any prior columns out of x; put elements in R
  V.col(mXNumCols) = x;
  if (mXNumCols > 0) std::cout << "Entry\n" <<  V.corner(Eigen::TopLeft,mRows,mXNumCols+1) << std::endl;
  if (mXNumCols > 0)
  { Vector r (apply_transform (mXNumCols));
    R.col(mXNumCols).start(mXNumCols) = r;
  }
  if (mXNumCols > 0) std::cout << "Exit\n" <<  V.corner(Eigen::TopLeft,mRows,mXNumCols+1) << std::endl;
  // convert x to rotation vector
  R(mXNumCols,mXNumCols) = house(mXNumCols);
  if (R(mXNumCols,mXNumCols) == 0)
    std::cout << "HHDR:  Error, singularity detected at variable " << mXNumCols << std::endl;
  else
    ++mXNumCols;
}

  
//     printing       printing       printing       printing       printing

void
Householder::print_to (std::ostream& os) const
{
  Eigen::Map<Matrix> V (mVPtr,mRows,mCols);
  Eigen::Map<Matrix> R (mRPtr,mCols,mCols);
  os << "Matrix V" << std::endl << V.corner(Eigen::TopLeft,   mRows  , mXNumCols) << std::endl;
  os << "Matrix R" << std::endl << R.corner(Eigen::TopLeft, mXNumCols, mXNumCols) << std::endl;
}
