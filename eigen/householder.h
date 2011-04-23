#ifndef _HOUSEHOLDER_H_
#define _HOUSEHOLDER_H_
/*
  Created to implement a static, inplace version of householder transformation.

 16 Mar 11 ... Created in test form


*/


#include <Eigen/Core>


class Householder
{
 public:
  typedef Eigen::MatrixXd Matrix;
  typedef Eigen::VectorXd Vector;
  
 private:
  typedef double          StorageType;
  typedef StorageType*    Ptr;
  
  const int mRows;      // number of rows -- never changes
  int mCols;            // number of columns -- can increase
  int mXNumCols;        // current number of columns in the factorization
  Vector mBeta;         // used to form householder outer product
  Ptr mVRPtr;           // space used to store householder vectors v_j in style of Golub

 public:
  Householder()
    : mRows(0), mCols(0), mXNumCols(0) { }

  Householder(int rows, int cols)
    : mRows(rows), mCols(cols), mXNumCols(0), mVtV(cols) { allocate(); }


  Matrix R() const;

  void add_vector(Vector const& x);

  void print_to (std::ostream& os) const;
  
 private:
  void    allocate();
  double  house(int column);             // convert V[column] to v used in reflection, in-place; return norm2 of v
  Vector  apply_transform(int column);   // apply current transform V[0] .. V[column-1] to V[col]
};


inline
std::ostream&
operator<<(std::ostream& os, Householder const& h)
{
  os << "- - - - - - - - Householder Decomposition - - - - - - - - - - - - - - - -" << std::endl;
  h.print_to(os);
  os << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
  return os;
}

#endif
