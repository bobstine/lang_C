/* $Id$
 *  gsl_data.test.cc
 *  seq_regr
 *
 *  Created by Robert Stine on 12/14/07.
 *  Copyright 2007. All rights reserved.
 *
 */


#include "gsl_data.h"

// for printing vector
#include "gsl_utils.h"

#include "column.h"

#include "random.h"
#include "print_utils.h"

// for constant iterator
#include "cyclic_iterator.h"
#include "anonymous_iterator.h"

#include <iostream>
#include <vector>

#define LEN 12



int
main (void)
{
  std::pair<double,double> eval;
  // boolean selection vector (like an APL compress operator); set some to be false
  bool b[LEN];
  for (int i=0; i<LEN; ++i)
    b[i]=true;
  b[0] = b[1] = b[5] = b[LEN-1] = false;
  int n (LEN - 3);
  
  // make response related to first predictor; echo data file to output
  std::cout << "TEST: Making a uniform generator \n";
  MarsagliaGenerator uni1(17);
  MarsagliaGenerator uni2(3837);
  double x1[LEN], x2[LEN], x3[LEN];
  double *y (new double[LEN]);
  double weights[LEN];
  for (int i=0; i<LEN; ++i)
  { weights[i] = (i+1) * 0.10;
    if ( LEN*uni1() < i )  y[i] = 1.0;
    else                   y[i] = 0.0;
  }
  std::cout << std::endl;
  
  // use a constant iterator for things that do not vary
  constant_iterator<bool> noSelection(true);
  constant_iterator<int>  noWeights(1);
  
  // make a vector of columns
  std::vector<Column> columns;
  Column yCol ("y", y, y+LEN);
  columns.push_back(yCol);
  
  // create anonymous iterators
  anonymous_iterator_envelope <std::random_access_iterator_tag,double> 
    hiddenY (make_anonymous_iterator(y));
  
  range <anonymous_iterator_envelope <std::random_access_iterator_tag,double> > 
    yRange  (make_anonymous_range(y, y+10));
  
  // y is a std::vector<double> works fine        theData(y.begin(),...)
  // y is an anonymous iterator is also fine      theData(hiddenY, ...)
  // y is begin of an anonymous range is fine     theData(begin(yRange),...)
  // y is memory from a column                    theData(yCol.memory(),...
  gslData  empty;
  gslData  theData(columns[0].memory(), noSelection, noWeights, LEN, 100); 
    
  // extract a column
  std::cout << "TEST: y from the data set is " << theData.y() << std::endl;
  
  // Use a temporary matrix
  int dim = 5;
  gsl_matrix *mat (theData.temp_mat(dim,dim));
  gsl_matrix_set_zero(mat);
  for (int i=0; i<dim; ++i)
    for (int j=0; j<dim; ++j)
      gsl_matrix_set (mat,i,j,i*j);
  for (int j=0; j<dim; ++j)
    std::cout << "TEST: column " << j << ": " << &gsl_matrix_column(mat,j).vector << std::endl;
  
  return 0;
}

