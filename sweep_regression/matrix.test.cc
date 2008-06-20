// $Id: matrix.test.cc,v 1.1 2005/06/13 20:47:51 bob Exp $

#include <iostream>
#include <string>
#include <functional>
#include <time.h>
#include <vector>

#include "matrix.h"
#include "utility.h"
#include "range.h"
#include "random.h"

int main (void)
{
  Matrix mat("test/matrix");
  std::cout << mat;
  
  { // tack on a vector
    vector<double> zero(mat.nRows(),0.0);
    std::cout << (mat | zero);
  }

  { // get a column via a range
    std::cout << "Second column: \n" ;
    std::cout << mat.column(2);
  }

  { // make new matrix via concatenating
    vector<double> one(mat.nRows(), 1.0);
    vector<double> two(mat.nRows(), 2.0);
    Matrix a,b,c;
    b = a | one;
    c = b | two;
    std::cout << c;
  }
  
  { // matrix mult by vector
    vector<double> wts (mat.nRows(), 1.0);
    vector<double> sum;
    sum = mat * wts;
    std::cout << sum << endl;
  }

  { // form cross-product matrix
    vector<double> wts (mat.nRows(), 1.0);
    std::cout << "Cross-product matrix: \n" << matrix_cross_product(mat.row_range(), make_range(wts));
  }
				 

  { // accumulate elements
    RandomGenerator rand(12);
    Matrix bigMat(25000,25,rand);
    vector<double> wts (bigMat.nRows(), 1.0);

    // raw summation of rows
    // standard c-style indexed loop is glacial (ie, bigMat[i][j] kills you)
    clock_t startRaw = clock();
    vector<double> rawTotal (bigMat.nCols(), 0.0);
    unsigned i = 0;
    for (vector< vector<double> >::const_iterator row = bigMat.begin();
	 row < bigMat.end(); ++ row, ++i)
      {
	double wt = wts[i] - 0.5;
	unsigned j = 0;
	for (vector<double>::const_iterator col = row->begin(); col < row->end(); ++col, ++j)
	  rawTotal[j] += wt * *col;
      }
    std::cout << "Raw totals " << rawTotal;
    std::cout << "    --- tick count " << clock() - startRaw << endl;
    
    // summation via ranges
    clock_t startRange = clock();
    vector<double> sum;
    sum = matrix_product(bigMat.row_range(),
			 make_unary_range(bind2nd(minus<double>(),0.5),make_range(wts)));
    std::cout << "Centered totals " << sum;
    std::cout << "    --- tick count " << clock() - startRange << endl;

    // weighted summation via range
    clock_t startWeighted = clock();
    sum = matrix_product(bigMat.row_range(),
			  make_binary_range (plus<double>(),
					     make_unary_range(bind2nd(minus<double>(),0.5),
							      make_range(wts)),
					     make_range(wts)));
    std::cout << "Weighted and centered " << sum;
    std::cout << "    --- tick count " << clock() - startWeighted << endl;
  }

  
  return 0;
}
    
