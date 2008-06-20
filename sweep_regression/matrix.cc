/* $Id: matrix.cc,v 1.1 2005/06/13 20:47:51 bob Exp $

  14 Dec 01 ... Created.

*/

#include "matrix.h"
#include "functions.h"
#include "utility.h"

#include <assert.h>
#include <fstream>
#include <algorithm>

///////////////////////////////////////////////////////////////////////////////////////

Matrix::Matrix(const char *fileName)
{
  ifstream input(fileName);
  read_from(input);
}


size_t
Matrix::nRows() const
{
  return mVecs.size();
}

size_t
Matrix::nCols() const
{
  if (mVecs.empty())
    return 0;
  else
    return mVecs[0].size();
}


////  Input/output ////

void
Matrix::read_from (istream& input)
{
  while (not(input.eof())) {
    vector<double> vec;
    input >> vec;
    if (vec.size()>0){
      mVecs.push_back(vec);
    }
    else
      cout << "Matrix: read blank row" << endl;
  }
  cout << "Read matrix of dimension ("
       << nRows() << "," << nCols() << ")" << endl;
}

void
Matrix::write_to (ostream& output) const
{
  output << "matrix [" << nRows() << "," << nCols() << "] \n";
  for_each (mVecs.begin(), mVecs.end(), Printer< vector<double> >(output));
}

istream&
operator>>(istream& input, Matrix& m)
{
  m.read_from(input); return input;
}

ostream&
operator<<(ostream& output, const Matrix& m)
{
  m.write_to(output);
  return output;
}

////  Random fill  ////


void
Matrix::fill_with_iid(int nRows, int nCols, RandomGenerator& rand)
{
  vector<double> x(nCols);
  for (int i=0; i<nRows; ++i)
    {
      for (int j=0; j<nCols; ++j)
	x[j] = rand.normal();
      mVecs.push_back(x);
    }
}

////  Fill with zero  ////


void
Matrix::fill_with_zero(int nRows, int nCols)
{
  for (int i=0; i<nRows; ++i)
    {
      vector<double> x(nCols,0.0);
      mVecs.push_back(x);
    }
}


////  Append row  ////

void
Matrix::append (vector<double> row)
{
  if (!empty())
    assert(row.size() == mVecs[0].size());
  mVecs.push_back (row);
}

////  Operators  ////

Matrix
transpose (const Matrix& m)
{
  vector< vector<double> > mt;
  for (unsigned j = 0; j < m.nCols(); ++ j)
    {
      vector<double> col(m.nRows());
      transform(m.begin(), m.end(), col.begin(), Function::Index(j));
      mt.push_back(col);
    }
  Matrix mat(mt);
  return mat;
}
		
namespace {
  class AddOn {
  public:
    vector<double>
    operator()(const vector<double>& a, const double toAdd)
    {
      vector<double> result = a;
      result.push_back(toAdd);
      return result;
    }
    vector<double>
    operator()(const double toAdd)
    {
      vector<double> result(1,toAdd);
      return result;
    }
  };
}
  
Matrix
operator| (const Matrix& m, const vector<double>& b)
{
  vector< vector<double> > result;
  if (m.empty())
    transform (b.begin(), b.end(), back_inserter(result), AddOn());
  else {
    assert (m.nRows() == b.size());
    transform (m.begin(), m.end(), b.begin(), back_inserter(result), AddOn());
  }
  return result;
}

vector<double>
operator* (const Matrix& m, const vector<double>& a)
{
  assert (a.size() == m.nRows());
  return matrix_product(m.row_range(), make_range(a));
}
    

///////////////////////////////////////////////////////////////////////////////////
//          Triangular Matrix
///////////////////////////////////////////////////////////////////////////////////

TriangularMatrix::TriangularMatrix(const char *fileName)
{
  ifstream input(fileName);
  read_from(input);
}


size_t
TriangularMatrix::nRows() const
{
  return mVecs.size();
}

size_t
TriangularMatrix::nCols() const
{
  return mVecs.size();
}


////  Input/output ////

void
TriangularMatrix::read_from (istream& input) // col at a time
{
  unsigned i = 1;
  while (not(input.eof())) {
    vector<double> vec;
    input >> vec;
    if (vec.size() > 0){
      assert(vec.size() == i);
      mVecs.push_back(vec);
      ++i;
    }
    else
      cout << "TriangularMatrix: read blank column" << endl;
  }
  cout << "Read lower triangular matrix of dimension ("
       << nRows() << "," << nCols() << ")" << endl;
}

void
TriangularMatrix::write_to (ostream& output) const
{
  output << "lower triangular [" << nRows() << "," << nCols() << "] (cols)\n";
  for_each (mVecs.begin(), mVecs.end(), Printer< vector<double> >(output));
}

istream&
operator>>(istream& input, TriangularMatrix& m)
{
  m.read_from(input); return input;
}

ostream&
operator<<(ostream& output, const TriangularMatrix& m)
{
  m.write_to(output);
  return output;
}

void
TriangularMatrix::append (vector<double> vec)
{
  assert(vec.size() == (1+mVecs.size()));
  mVecs.push_back(vec);
}
