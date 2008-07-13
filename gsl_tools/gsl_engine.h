// -*- c++ -*-
// $Id: gsl_engine.h,v 1.13 2008/01/07 04:06:38 bob Exp $
#ifndef _GSL_ENGINE_H_
#define _GSL_ENGINE_H_

#include "gsl_data.h"

#include <gsl/gsl_errno.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>

#include <iostream>

/*
 A GSL vector engine implements a policy for manipulating vectors in
 n-space that supports either OLS or WLS regression.  The engine
 isolates calculation with weighted dot products from other code used
 elsewhere in regression.
 
 An engine manipulates n dimensional vectors as declared on
 creation. External calling routine must be sure that the vector or
 matrix is of this length, such as by making a view of a subset of a
 larger array.  

 A gsl engine also has square root of the weights.
 
  6 Jan 08 ... Push spline calculation down here.
 25 Nov 07 ... Created to support partitioning of gsl routines, weighted least squares.
*/



class olsEngine
{
protected:
  const int mN;
  
public:
  olsEngine ()                     : mN (0)         { }
  olsEngine (int n)                : mN (n)         { }
  olsEngine (gslData const* data)  : mN (data->n()) { }
  olsEngine (olsEngine const& src) : mN (src.mN)    { }
    
  int  n () const { return mN; }
  
  // summary statistics
  double average         (gsl_vector const* v)              const;
  double sum_of_squares  (gsl_vector const* v, double mean) const;
  double sum_of_squares  (gsl_vector const* v)              const;

  void   prepare_vector_for_analysis (gsl_vector *dest, gsl_vector const* src) const { gsl_vector_memcpy(dest, src); }
  void   insert_analysis_matrix      (gsl_matrix *dest, gsl_matrix const* src) const { gsl_matrix_memcpy(dest, src); }
  
  // dot product
  void   blas_ddot (gsl_vector const* x, gsl_vector const* y, double *dp)                      const;
  // accumulate sum e_i^2 z z'
  void   blas_dsyr (gsl_matrix const* z, gsl_vector const* e, gsl_matrix *zdz) const;
  
  // smoothing spline returns SS around mean of smooth
  double   smooth (int df, gsl_vector const*x, gsl_vector const* y, gsl_vector *smth) const;

  void   print_header_to (std::ostream& os) const { os << "OLS Engine "; }
  // void   print_to (std::ostream& os)        const { };
    
private:
  void configure (gslData *data); 
  
  olsEngine& operator=(const olsEngine& regr);
};


class wlsEngine
{
protected:
  const int mN;
  gsl_vector *mWeights;  // space is managed by gslData object
  gsl_vector *mSqrtW;    // allocate locally; frees these on exit

public:
    ~wlsEngine() { std::cout << "WLSE: Freeing engine.\n"; gsl_vector_free(mSqrtW); }
  
  wlsEngine()                    : mN(0), mSqrtW(0) { }
  wlsEngine(gslData *data)       : mN(data->n()), mSqrtW(0) { configure(data); }
  
  // accessors
  int               n ()        const { return mN; }
  gsl_vector      * weights ()        { return mWeights; } // not const so can set externally
  gsl_vector const* sqrt_wts()  const { return mSqrtW; }   // only computed to length in use; not extended
  void              weights_have_changed ();
  
  double average         (gsl_vector const* v)              const;
  double sum_of_squares  (gsl_vector const* v, double mean) const;
  double sum_of_squares  (gsl_vector const* v)              const;

  void   prepare_vector_for_analysis (gsl_vector *dest, gsl_vector const* src)                 const;
  void   insert_analysis_matrix      (gsl_matrix *dest, gsl_matrix const* src)                 const;
  void   unweight                    (gsl_vector *vec)                                         const;
   
  void   blas_ddot (gsl_vector const* x, gsl_vector const* y, double *dp)                      const;
  void   blas_dsyr (gsl_matrix const* z, gsl_vector const* e, gsl_matrix *zdz)                 const;

  // smoothing spline returns SS around mean of smooth
  double   smooth (int df, gsl_vector const*x, gsl_vector const* y, gsl_vector *smth) const;

  void   print_header_to (std::ostream& os) const { os << "WLS Engine "; }

private:    
  void configure(gslData *data); 
  
  // use subset of weights for partial householder transform stages
  void blas_ddot (gsl_vector const* x, gsl_vector const* y, gsl_vector const* w, double *dp)  const;  
  
  wlsEngine& operator=(const wlsEngine& engine);
};

/*
namespace gsl_engine { // avoid conflict with gsl_regression operator<< 

  std::ostream&
  operator<<(std::ostream& os, const olsEngine& engine) { engine.print_header_to(os); return os; }

  std::ostream&
  operator<<(std::ostream& os, const wlsEngine& engine) { engine.print_header_to(os); return os; }

}
*/

#endif
