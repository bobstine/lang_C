/*
  Solve the system Ax=b and check the accuracy.
  Note the style in manipulations: &view.matrix/vector
*/

#include <stdio.h>
#include <time.h>

#include <gsl/gsl_errno.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>

#define REPS 100
#define DIM 50

int
main (void)
{
  // Save the error handler to be able to restart
  gsl_error_handler_t *errHandler;
  
  // Init random generator
  gsl_rng_env_setup();
  const gsl_rng_type *T;
  T = gsl_rng_default;
  gsl_rng *r;
  r = gsl_rng_alloc (T);

  // Fill matrix and vector
  double a [DIM*DIM];
  for (int i = 0; i < DIM; ++i)
  {
    for (int j=0; j < DIM; ++j)
    {
      double mij = gsl_rng_uniform (r);
      a[DIM*i+j] = mij;
    }
  }
  
  printf ("Speed test with DIM=%d and REPS=%d \n", DIM, REPS);
  clock_t start = clock();

  // make a 'true' solution vector
  gsl_vector *x = gsl_vector_alloc (DIM);
  for (int i=0; i<DIM; ++i)
    gsl_vector_set(x,i,(double)i);
  
  gsl_vector *b = gsl_vector_alloc (DIM);
  gsl_vector *Ax = gsl_vector_alloc (DIM);
  gsl_matrix *Acopy = gsl_matrix_alloc(DIM,DIM);
  gsl_permutation *p = gsl_permutation_alloc (DIM);

  double accumErr = 0.0;
  int statusSolve, statusDecomp;
  for(int rep = 0; rep < REPS; ++rep)
  { // alter the matrix A to force compiler to recompute  a new object
    a[7] = gsl_rng_uniform(r);
    // load data into GSL matrix
    gsl_matrix_view A = gsl_matrix_view_array(a, DIM, DIM);
    // induce singularity: make first two columns proportional
    for (int k=0; k<DIM; ++k)
      gsl_matrix_set(&A.matrix,k,0, 10.0 * gsl_matrix_get(&A.matrix,k,1));
    // Save a copy prior to factoring
    gsl_matrix_memcpy (Acopy, &A.matrix);
    // Make sure result b is a linear combination of A, b = 1 A x + 0 b
    gsl_blas_dgemv(CblasNoTrans, 1.0, Acopy, x, 0.0, b);
    // Solve the system
    int s;
    errHandler = gsl_set_error_handler_off();     // Dont abort on a GSL error
    statusDecomp = gsl_linalg_LU_decomp (&A.matrix, p, &s);
    statusSolve = gsl_linalg_LU_solve (&A.matrix, p, b, x);
    gsl_set_error_handler(errHandler);            // Resume error checking
    if (statusDecomp || statusSolve)
    {
      fprintf (stderr, "Failed, gsl_errnos=%d,%d\n", statusDecomp, statusSolve);
      exit(-1);
    }
    // Check the answer by computing A times solution x
    gsl_blas_dgemv(CblasNoTrans, 1.0, Acopy, x, 0.0, Ax);
    // printf ("B = \n");  gsl_vector_fprintf(stdout,&B.vector, "%g");
    // printf ("Ax= \n");  gsl_vector_fprintf(stdout,Ax, "%g");
    // Subtract with difference in Ax,Ax=Ax-b
    gsl_vector_sub(Ax, b);
    // Accumulate error norm
    accumErr += gsl_blas_dnrm2(Ax);
  }
  double time = ((double) clock() - start)/CLK_TCK;
  printf ("Time = %lf sec/matrix, with avg accumErr norm %lf \n", time/REPS, accumErr/REPS);
  printf ("permutation = \n"); gsl_permutation_fprintf(stdout, p, " %d");
  printf ("\nx = \n");  gsl_vector_fprintf(stdout, x, "%g");
    
  // Free memory
  gsl_rng_free (r);
  gsl_permutation_free (p);
  return 0;
}

