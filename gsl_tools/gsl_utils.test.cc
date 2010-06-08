/*  $Id: gsl_utils.test.cc,v 1.6 2008/02/12 03:48:17 bob Exp $
 *  gsl_utils.test.cc
 *  seq_regr
 *
 *  Created by Robert Stine on 1/29/08.
 *  Copyright. All rights reserved.
 
Only see the benefits of the raw distance calculation when turn on some optimization (O2).
Otherwise this raw version as well as the range version crawl.  If locked on other dot prod
then avoid writes to memory as Dean does.

Elaspsed time for raw 7
Elaspsed time for ranges 11
Elaspsed time for Dean is 16
Elaspsed time for subtract and one ddot is 20

*/

#include "gsl_utils.h"

#include "range.h"

// test speed
#include <gsl/gsl_blas.h>
#include <time.h>
#include "range_ops.h"

#include <iostream>
#include <vector>



class Wrapper
{
  gsl_vector const* mv;
public:
  Wrapper(gsl_vector const* v) : mv(v) { }
  
  Ranges::range<gsl_vector_const_iterator> range() const { return make_range(mv); }
};

time_t global_start_time = time(0);
double global_minutes = 40 * 24 * 60;

bool
time_ended_p()
{
  return (difftime(time(0),global_start_time) > 60 * global_minutes);
}



double 
range_ddot (double const* x, double const* y, int n)
{
  return range_ops::accumulate(
                               make_binary_range(
                                                 std::multiplies<double>(),
                                                 Ranges::make_range(x,x+n), 
                                                 Ranges::make_range(y,y+n)
                                                 ),
                               0.0);
}



double 
l2_distance_raw(const gsl_vector* d1, const gsl_vector* d2)
{  // (X-Y)^2 = |X|^2 + |Y|^2 - 2 X'*Y
  int n           (d1->size);
  double const* x (gsl_vector_const_ptr(d1,0));
  double const* y (gsl_vector_const_ptr(d2,0));
  register double dist (0.0);
  register double diff;
  for (int i=0; i<n; ++i) 
  {
    diff = *x-*y;
    dist += diff * diff;
    ++x;
    ++y;
  }
  return dist;
}


double 
l2_distance_range(const gsl_vector* d1, const gsl_vector* d2)
{  // (X-Y)^2 = |X|^2 + |Y|^2 - 2 X'*Y
  int n           (d1->size);
  double const* x (gsl_vector_const_ptr(d1,0));
  double const* y (gsl_vector_const_ptr(d2,0));
  return range_ddot(x,x,n)+range_ddot(y,y,n)-2.0*range_ddot(x,y,n);
}


double 
l2_distance(const gsl_vector* d1, const gsl_vector* d2)
{  // (X-Y)^2 = |X|^2 + |Y|^2 - 2 X'*Y
  double xx = 0.0;
  double yy = 0.0;
  double xy = 0.0;
  gsl_blas_ddot(d1,d1,&xx);
  gsl_blas_ddot(d2,d2,&yy);
  gsl_blas_ddot(d1,d2,&xy);
  return xx + yy - 2 * xy;
}

double 
l2_dist(const gsl_vector* d1, const gsl_vector* d2, gsl_vector* temp)
{  // (X-Y)^2 = |X|^2 + |Y|^2 - 2 X'*Y
  gsl_vector_memcpy(temp, d1);
  gsl_vector_sub(temp,d2);
  double diff;
  gsl_blas_ddot(d1,d1,&diff);
  return diff;
}

  
int 
main()
{    
  int nRows (200);
  int nCols (9);
  
  gsl_vector * v (gsl_vector_alloc(nRows));
  for (int i=0; i<nRows; ++i)
    gsl_vector_set(v,i,0.5+i);
  
  gsl_matrix * m (gsl_matrix_alloc(nRows,nCols));
  
  std::vector<Wrapper> collection;
  for(int j = 0; j<nCols; ++j)
    collection.push_back(Wrapper(v));
  gsl_matrix_fill_from_ranges(collection, m);   // items in collection must have range() method
    
  std::cout << "TEST: mean = " << gsl_vector_mean(v) << std::endl;
  std::cout << "TEST:  sd  = " << gsl_vector_standard_deviation(v) << std::endl;
  
  std::cout << "TEST: means of the columns are ... " ;
  for(int j=0; j<nCols; ++j)
    std::cout << "   " << gsl_vector_mean(&gsl_matrix_const_column(m,j).vector);
  std::cout << std::endl;
  
  //  Test speed of difference
  std::cout << "TEST: Beginning test of speed of distance calculations ... \n" ;
  const int testSize (10000);
  gsl_vector *xx   (gsl_vector_alloc(testSize));
  gsl_vector *yy   (gsl_vector_alloc(testSize));
  gsl_vector *temp (gsl_vector_alloc(testSize));
  for (int i = 0; i<testSize; ++i) 
  { gsl_vector_set(xx,i,i);
    gsl_vector_set(yy,i,2*i);
  }
  
  const int nReps (1000000000/testSize); //(1000000000/testSize);
  double dot (0.0);

  time_t t0 (time(0));
  for(int i=0; i<nReps; ++i)
  {   
    dot += l2_distance_raw(xx,yy);
  }
  time_t t1 (time(0));
  std::cout << "Elaspsed time for raw " << difftime(t1,t0) << std::endl;
  
  
  t0 = time(0);
  for(int i=0; i<nReps; ++i)
  {   
    dot += l2_distance_range(xx,yy);
  }
  t1 =time(0);
  std::cout << "Elaspsed time for ranges " << difftime(t1,t0) << std::endl;
  
  
  
  t0 = time(0);
  for(int i=0; i<nReps; ++i)
  {   
    dot += l2_distance(xx,yy);
  }
  t1 = time(0);
  std::cout << "Elaspsed time for Dean is " << difftime(t1,t0) << std::endl;
  
  t0 = time(0);
  for(int i=0; i<nReps; ++i)
  {   
    dot += l2_dist(xx,yy,temp);
  }
  t1 = (time(0));
  std::cout << "Elaspsed time for subtract and one ddot is " << difftime(t1,t0) << std::endl;
  
  
  
  
  // test sampling; put counts into first column of src matrix
  /* initialize random generator */
  srand ( time(NULL) );
  for (int i=0; i<nRows; ++i)
    gsl_matrix_set(m,i,0,i);
  gsl_matrix *sample (gsl_matrix_alloc(13,nCols));
  gsl_matrix_sample_rows(sample, m);
  std::cout << "TEST: Sampled matrix is " << sample << std::endl;
  
  return 0;
}
