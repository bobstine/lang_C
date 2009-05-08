/*    $Id: gsl_data.h,v 1.7 2008/01/16 03:28:08 bob Exp $
 
 Data object used to store lots of distinct columns in coherent way that 
 can be shared among the objects in a policy group.
 
 The vectors in gsl data objects are set up to support cross validation.  
 The first n rows are active in the sense that these are the rows intended
 for the estimation sample, with the remaining len-n intented for validation. 
 
 Created by Robert Stine on 12/4/07. Copyright 2007. All rights reserved.
*/
#ifndef _GSL_DATA_H_
#define _GSL_DATA_H_

#include <gsl/gsl_matrix.h>
#include <vector>
#include <string>

static const int gslDataTempSize (25);

class gslData {

private:
  int         mN;              // number of "active" rows as identified on start
  gsl_vector *mY;              // may be centered or otherwise altered by analysis
  gsl_matrix *mX;              // only selected rows retained, X held as (mN x mQ)
  gsl_vector *mXb;             // linear combination of the X columns
  gsl_vector *mE;              // multiple uses, depending on model (e.g.: y - xb)
  gsl_vector *mWeights;
  int        *mPermute;        // permute the input for 0/1 compression
  
  std::vector <std::string> mNames;  // names of predictor columns
  
  double *         mScratch;         // hold temporary calculations
  gsl_matrix_view  mTempMatView;
  std::vector<gsl_vector *> mTempVec;
  
public:
    ~gslData () { free(); }
  
  gslData() : mN(0), mY(0), mX(0) { }

  template<class Iter, class BIter>                        // no weights
    gslData(Iter Y, BIter B, int len, int maxQ) : mN(0), mNames(1+maxQ)
    {
      allocate(len, maxQ, false);
      int k (len);
      for(int i=0; i<len; ++i, ++Y, ++B) {
        if (*B) {
          gsl_vector_set(mY, mN, *Y); mPermute[i]=mN; ++mN;}
        else {
          --k; gsl_vector_set(mY, k, *Y); mPermute[i]=k; }
      }
    }
  
  template<class Iter, class BIter, class WIter>            // weights
    gslData(Iter Y, BIter B, WIter W, int len, int maxQ) : mN(0), mNames(1+maxQ)
    {
      allocate(len,maxQ, true);
      // k marks end and counts down to insert validation cases
      int k (len);  
      for(int i=0; i<len; ++i, ++Y, ++B, ++W)
      { if (*B)
	{ gsl_vector_set(mY, mN, *Y); 
          gsl_vector_set(mWeights, mN, *W);
          mPermute[i]=mN; ++mN;
	} 
        else
	{ --k;
	  gsl_vector_set(mY, k, *Y);
	  mPermute[i]=k;
	}
      }
    }

  // preserve const status
  inline int       n()     const { return mN; }
  inline int   max_q()     const { return mX->size2; }
  int         length()     const { return (mY) ? mY->size : 0; }
  const int* permutation() const { return mPermute; }
  
  gsl_vector const* Xb()  const { return mXb; }
  gsl_matrix const* x()   const { return mX; }
  gsl_vector const* e()   const { return mE; }
  gsl_vector const* y()   const { return mY; }
  gsl_vector const* w()   const { return mWeights; }

  std::vector< std::string > const& x_names() const { return mNames; }

  
  // use these to grab pointers needed for updating
  gsl_vector* live_Xb()      { return mXb;}
  gsl_matrix* live_x()       { return mX; }
  gsl_vector* live_e()       { return mE; }
  gsl_vector* live_y()       { return mY; }
  gsl_vector* live_w()       { return mWeights; }

  void set_name_of_predictor(std::string const& name, int index) { mNames[index] = name; }
      
  gsl_matrix* temp_mat(int nRows, int nCols);  
  gsl_vector* temp_vec(int j){ return mTempVec[j]; }
  
  // used for full input and output
  template<class Iter> 
    void
    permuted_copy_from_iterator(Iter src, gsl_vector *dest)  {
      permuted_copy_from_iterator(src,dest,length()); }

  template<class Iter> 
    void
    permuted_copy_from_iterator(Iter src, gsl_vector *dest, int n)  {
      for(int i=0; i<n; ++i, ++src)
        gsl_vector_set(dest, mPermute[i], *src);  }

  template<class Iter> 
    void
    permuted_copy_to_iterator  (gsl_vector const* src, Iter dest, int n) const {
      for(int i=0; i<n; ++i, ++dest)
        *dest = gsl_vector_get(src,mPermute[i]);  }
  
  template<class Iter> 
    void
    permuted_copy_to_iterator  (double const* src, Iter dest, int n) const {
      for(int i=0; i<n; ++i, ++dest)
        *dest = src[mPermute[i]];  }

  
private:
    void allocate(int rows, int cols, bool wts);
    void free();
    
    gslData(gslData const&);
    gslData& operator=(gslData const&);
};
  
#endif
  
