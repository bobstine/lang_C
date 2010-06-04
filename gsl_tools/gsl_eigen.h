/* $Id: gsl_eigen.h,v 1.12 2008/02/22 19:39:47 bob Exp $
 *  gsl_eigen.h
 *  seq_regr
 *
 *  Created by Robert Stine on 1/28/08.
 *  Copyright 2008. All rights reserved.

 
 Uses GSL code to build a principal value decomposition.
 Must satisfy the following protocol to be suited for a
 use as a transformation in a variable selection bundle 
 stream.

 Protocol:  
           Class creator has 2 arguments (int, bool)
           operator() method applies to a gsl_matrix

           operator() returns evalues, selected number of principal components
 
 */
#ifndef _GSL_EIGEN_H_
#define _GSL_EIGEN_H_

#include <gsl/gsl_matrix.h>

#include <utility>
#include <functional>
#include <vector>

namespace gsl_eigen 
{
  void
  standardize_matrix_columns (gsl_matrix *data);
  
  void 
    principal_components (gsl_matrix* data, gsl_vector* eVals, gsl_matrix* eVecs);
  
  gsl_vector*                // assumes standardized if pSD is not nil
    construct_principal_component (gsl_matrix const* data, int j, gsl_matrix const* eVecs, gsl_vector const* pMeans, gsl_vector const* pSD);
}


class gslPrincipalComponents: public std::unary_function<gsl_matrix const*, std::pair<gsl_vector*, std::vector<gsl_vector*> > >
{
  int         mNumComponents;
  bool        mStandardize;
  int         mSkip;            // skip this number of leading context rows
  
public:
  gslPrincipalComponents (int k, bool standardize, int skip) : mNumComponents(k), mStandardize(standardize), mSkip(skip) {}
  
  int number_of_components() const { return mNumComponents; }
   
  result_type operator()(gsl_matrix const* data) const;   //  < evals, evectors >
};



//  RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS    RKHS   

class RadialKernel
{
  static std::string classname;
public:
  std::string const& name() const { return classname; }
  double operator()(gsl_vector const *a, gsl_vector const* b) const;
};
  
class WeightedRadialKernel
{
  static std::string classname;
  gsl_vector const* mWts;
public:
  WeightedRadialKernel (gsl_vector const* wts) : mWts(wts) { };
  WeightedRadialKernel (WeightedRadialKernel const& k) : mWts(k.mWts) { };
  
  std::string const& name() const { return classname; }
  
  double operator()(gsl_vector const *a, gsl_vector const* b) const;
};

class QuadraticKernel
{
  static std::string classname;
public:
  std::string const& name() const { return classname; }
  double operator()(gsl_vector const*a, gsl_vector const* b) const;
};

class L2Kernel
{
  static std::string classname;
public:
  std::string const& name() const { return classname; }
  double operator()(gsl_vector const* a, gsl_vector const* b) const;
};




template<class Kernel>
class gslRKHS: public std::unary_function<gsl_matrix const*, std::pair<gsl_vector*, std::vector<gsl_vector*> > >
{
  int    const  mNumComponents;
  bool   const  mStandardize;
  
public:
    ~gslRKHS() {  }
  
  gslRKHS (int k, bool standardize) : mNumComponents(k), mStandardize(standardize) { }
  gslRKHS (gslRKHS const& rkhs) : mNumComponents(rkhs.mNumComponents), mStandardize(rkhs.mStandardize) { }
  
  int number_of_components() const { return mNumComponents; }
  
  result_type operator()(gsl_matrix* data) const;   //  WARNING: over-writes input data 
};


#include "gsl_eigen.template.h"


#endif
