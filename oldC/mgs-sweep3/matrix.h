/* $Id: matrix.h,v 1.7 2002/02/27 22:44:37 bob Exp $
  
    4 Feb 02 ... Another run at C++ matrix code for regression.
   14 Dec 01 ... Version for C++ based on vector class.
   13 Aug 01 ... Add file io functions.
   30 Oct 98 ... Initialize function added, more functional style added
   26 Oct 98 ... Pure pointer, functional style
   15 Oct 98 ... Created.
*/

#ifndef _MATRIX_H_
#define _MATRIX_H_  

#include <vector>
#include <iostream>
#include <fstream>
#include <functional>

#include "random.h"
#include "functions.h"
#include "range.h"

////////////////////////////////////////////////////////////////////////////////
//  held as vector of "rows"
////////////////////////////////////////////////////////////////////////////////


class DiagonalIterator : public iterator<forward_iterator_tag, double> {
  vector< vector<double> >::const_iterator mRow;
  int mCol;

 public:
  DiagonalIterator(vector< vector<double> >::const_iterator begin)
    : mRow(begin),mCol(0) { }
  void
    operator++() { ++mCol; ++mRow; }
  double
    operator*() const { return (*mRow)[mCol]; }
  bool operator==(const DiagonalIterator& it) const { return mRow == it.mRow; }
  bool operator!=(const DiagonalIterator& it) const { return mRow != it.mRow; }
};


class Matrix {
  vector< vector<double> > mVecs;
  
 public:
  Matrix()
    : mVecs(0) { }
  Matrix(const char* fileName);
  Matrix(istream& input)
    : mVecs(0) { read_from(input); }
  Matrix (int nRows, int nCols)
    : mVecs() { fill_with_zero(nRows, nCols); }
  Matrix (int nRows, int nCols, RandomGenerator& rand) // fill with random
    : mVecs() { fill_with_iid(nRows, nCols, rand); }
  Matrix(const vector< vector<double> > vecs)
    : mVecs(vecs) { };
  Matrix(const Matrix& m)
    : mVecs(m.mVecs) { }

  bool empty() const { return ((nRows()==0) && (nCols()==0)); }
  
  size_t nRows() const;
  size_t nCols() const;
  pair<size_t,size_t> dimension () const    { return make_pair(nRows(), nCols()); }

  typedef vector< vector<double> >::const_iterator row_iterator;
  typedef pair<row_iterator, row_iterator> range;
  
  range
    row_range () const    { return make_pair(mVecs.begin(), mVecs.end()); }
  DiagonalIterator
    begin_diagonal () const { return DiagonalIterator(mVecs.begin()); }
  row_iterator
    begin() const    { return mVecs.begin(); }
  row_iterator
    end() const    { return mVecs.end(); }
  vector<double>
    operator[](const int i) const    { return mVecs[i]; }

  unary_iterator<Function::Index, row_iterator>::range
    column (int col) const { return make_unary_range(Function::Index(col),mVecs.begin(), mVecs.end()); }

  void
    append (vector<double> row);
  
  friend Matrix
    transpose (const Matrix& m);
  
  friend Matrix
    operator* (const Matrix& m, const Matrix& a); // <m,a> := M'a
  friend vector<double>
    operator* (const Matrix& m, const vector<double>& a);  // <M,a> := M'a   lin comb of rows

  friend Matrix
    operator| (const Matrix& m, const vector<double>& a); // m | a, concatenation

  void write_to (ostream &output) const;
  void read_from (istream &input);

 private:
  
  void fill_with_iid (int nRows, int nCols, RandomGenerator& rand);
  void fill_with_zero (int nRows, int nCols);
  
};

istream&
operator>>(istream& input, Matrix& m);

ostream&
operator<<(ostream& output, const Matrix& m);
    

template <class I>
vector<double>
matrix_product(const Matrix::range X, const pair<I,I> y, unsigned skip=0)
     // X'y, with option to skip columns of X (used in cross_product)
{
  vector<double> result (X.first->size(), 0.0);
  I yPtr = y.first;
  for(vector< vector<double> >::const_iterator it = X.first; it<X.second; ++it)
    {
      double yVal = *yPtr; ++ yPtr;
      int j = skip;
      for(vector<double>::const_iterator col=it->begin()+skip; col<it->end(); ++col, ++j)
	result[j] += yVal * (*col);
    }
  return result;
}

template <class I>
Matrix
matrix_cross_product (const Matrix::range X,  const pair<I,I> w)  // X'WX
{
  Matrix mat;
  const unsigned dim = X.first->size();
  for (unsigned i=0; i<dim; ++i) {
    vector<double> row =
      (matrix_product(X,
		      make_binary_range(multiplies<double>(),
					w,
					make_unary_range(Function::Index(i),
							 X)),
		      i));  // skip redundant calculations
    for (unsigned j=0; j<i; ++j) // fill in leading elements via symmetry
      row[j] = mat[j][i];
    mat.append(row);
  }
  return mat;
}


// stored as of columns (ie, lower triangular)
class TriangularMatrix {
  vector< vector<double> > mVecs;
  
 public:
  TriangularMatrix()
    : mVecs() {  }
  TriangularMatrix(const char* fileName);
  TriangularMatrix(istream& input)
    : mVecs() { read_from(input); }
  TriangularMatrix (const vector< vector<double> > vecs)
    : mVecs(vecs) { };

  size_t nRows() const;
  size_t nCols() const;
  pair<size_t,size_t> dimension () const    { return make_pair(nRows(), nCols()); }

  vector< vector<double> >::const_iterator
    begin() const { return mVecs.begin(); }
  DiagonalIterator
    begin_diagonal() const { return DiagonalIterator(mVecs.begin()); }
  
  vector<double>
    operator[](const int i) const    { return mVecs[i]; }

  void
    append (vector<double> col);
  
  void write_to (ostream &output) const;
  void read_from (istream &input);
};

ostream&
operator<<(ostream& output, const TriangularMatrix& m);
    


Matrix
IdentityMatrix (int dim);

double
TraceOfMatrix (const Matrix& m);

Matrix
MatrixProduct (Matrix a, Matrix b, char *creator);

Matrix 	LUFactor (Matrix mat, long *permutation, double *parity, char *creator);
void 	LUSolve (Matrix factor, long *permutation, double b[]);

Matrix	InverseMatrix (Matrix m, char *creator);

double  QuadraticForm (Matrix m, double *x);          // x'Mx

Matrix  TriangularQuadraticForm (Matrix u, Matrix m); // u m u', u up triagular


//  Stepwise calculations

int ForwardStepwise (Matrix XpX, long nSteps, long *cols);
/*
	Which of the columns to add will have the most impact.
	Assume that Y is the first col of the X'X input array.
*/

int ReverseStepwise (Matrix XpX, long nSteps, long *cols);
/*
	Assuming all have been swept, restoring which of the 
	previously swept columns will have the least impact if 
	removed from the fitted model.
*/

int ForwardSweepMatrix(Matrix m, long col);
int ReverseSweepMatrix(Matrix m, long col);

#endif

