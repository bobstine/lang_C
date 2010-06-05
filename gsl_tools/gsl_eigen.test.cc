/*
 *  gsl_eigen.test.cc
 *  seq_regr
 *
 *  Created by Robert Stine on 1/29/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "gsl_eigen.h"
#include "gsl_utils.h"

#include <iostream>
#include <fstream>
#include <utility>
#include <time.h>


double
uniform_rand()
{
  return ((double)rand())/RAND_MAX;
}


/*
 
 Plan

 -- What to do about standardizing in the RKHS context?
 
 -- RKHS: Add a layer/ability (perhaps this belongs in the stream) to handle the spectrum 
    of the matrix.  If its too close to diagonal of 1's, then need to wided kernel.
 
 -- Need to keep the creation of the decomposition homogeneous so can template these things.
    Standardize makes sense as option for PC and RKHS, in sense that you want to standardize
    data prior to computing the distances.
                                                       
 */

int
main()
{
  srand (4553);

  const int n (30);
  const int k ( 5);
  
  // build data matrix
  gsl_matrix *data (gsl_matrix_alloc(n,k));
  for (int i=0; i<n; ++i)
    for (int j=0; j<k; ++j)
      gsl_matrix_set(data,i,j,uniform_rand());
  
  // write to file so can check output results
  char filename[] = "/Users/bob/Desktop/pc.dat";
  gsl_matrix_write_to_file (filename, data);  

  // create a principal component operator, standardizing
  gslPrincipalComponents pc(0,true);
  
  // apply to data matrix
  gsl_matrix * decomp ( pc(data) );
  int nPC (decomp->size2);
  std::cout << "TEST: Found " << nPC << " eigenvectors which are \n";
  for (int j=0; j<nPC; ++j)
    std::cout << &gsl_matrix_const_column(decomp,j).vector << std::endl;

  // compare SD of resulting pc's to eigenvalues
  std::cout << "TEST: Squared SDs of the output prin comps are: ";
  for (int j=0; j<nPC; ++j)
  { double sd = gsl_vector_standard_deviation(&gsl_matrix_const_column(decomp,j).vector);
    std::cout << sd*sd << ", ";
  }
  std::cout << std::endl;

  gsl_matrix_free(data);
  
  // now for the RKHS test
  std::cout << "\n\n\n RKHS \n\n\n";
  const int N (400);
  data = gsl_matrix_alloc(N,k);
  for (int i=0; i<N; ++i)
    for (int j=0; j<k; ++j)
      gsl_matrix_set(data,i,j,20.0 * (uniform_rand() - 0.5));
  std::ofstream output ("/Users/bob/Desktop/data.txt");
  for (int i=0; i<N; ++i)
  { for (int j=0; j<k; ++j)
      output << gsl_matrix_get(data,i,j) << " ";
    output << std::endl;
  }
  

  const int  numComponents (2);
  const bool standardize (true);
  
  gsl_vector *sd (gsl_vector_alloc(k));
  for (int j=0; j<k; ++j)
    gsl_vector_set(sd,j, gsl_vector_standard_deviation(&gsl_matrix_column(data,j).vector));
  gslRKHS<RadialKernel> rkhs (numComponents, standardize);
  gsl_matrix * basis ( rkhs(data) );
  
  std::cout << "TEST: Found " << basis->size2 << " eigenvectors; PCs are \n";
  for (int j=0; j<(int)basis->size2; ++j)
    std::cout << &(gsl_matrix_const_column(basis,j).vector) << std::endl;
  
  return 0;
}
  
