// $Id: smoothing_spline.h,v 1.24 2008/01/30 22:39:58 bob Exp $

#ifndef _SMOOTHING_SPLINE_H_
#define _SMOOTHING_SPLINE_H_

/*

  Smoothing splines.  Note that there are 4 more knots than coefs.

  The registry holds the coefficients that are kept in a spline
  operator.  This method of storing the coefficients out of the
  function operator itself avoids the endless copy-constuction of
  'heavy' function objects with hundreds of doubles.

  26 Apr 04 ... Created from xlisp module.

*/


#include "range.h"
#include "operator_traits.h"
#include "data_registry.h"

#include <sstream>
#include <vector>
#include <map>
#include <iostream>


class SmoothingSplineData
{
  int      mNCoef;
  double * mCoefs;
  double * mKnots;   // 4 + num coefs  (to handle ends of data)

 public:
  ~SmoothingSplineData () { delete_pointers(); }
  
  SmoothingSplineData (int nCoef, double const* coefs, double const* knots)
    : mNCoef(nCoef), mCoefs(0), mKnots(0)
    { allocate_pointers(); copy_data(coefs,knots); }
  
  SmoothingSplineData (SmoothingSplineData const& ssd)
    : mNCoef(ssd.nCoef()), mCoefs(0), mKnots(0)
    { allocate_pointers(); copy_data(ssd.coefs(),ssd.knots()); }

  SmoothingSplineData (std::istream& is)
    : mNCoef(0), mCoefs(0), mKnots(0)
    { read_from (is); }
  
  int     nKnot ()                  const { return mNCoef + 4; }
  int     nCoef ()                  const { return mNCoef; }

  double* coefs ()                  const { return mCoefs; }
  double* knots ()                  const { return mKnots; }

  void read_from (std::istream& is);
  void write_to  (std::ostream& os) const;
    
 private:
  void allocate_pointers();
  void delete_pointers  ();

  void copy_data        (double const* coefs, double const* knots);
};


class SmoothingSplineOperator : public std::unary_function<double,double>
{
  int                  mDF;
  double               mMinX, mMaxX, mRngX;
  int                  mKey;       // key to locate knots and coefs in registry

  static DataRegistry<SmoothingSplineData> sDataRegistry;
  
 public:
  ~SmoothingSplineOperator() { sDataRegistry.remove_reference(mKey); }
  
  SmoothingSplineOperator ()
    : mDF(0), mMinX(0), mMaxX(0), mRngX(0), mKey(-1) { }
  
  SmoothingSplineOperator (SmoothingSplineOperator const& ss)
    : mDF(ss.mDF), mMinX(ss.mMinX), mMaxX(ss.mMaxX), mRngX(ss.mRngX), mKey(ss.mKey)
    {
      sDataRegistry.add_reference(mKey);  // inform registry of another link to data
    }
  
  SmoothingSplineOperator (int df, double min, double max, int nCoef, double *coefs, double *knots)
    : mDF(df), mMinX(min), mMaxX(max), mRngX(max-min), mKey(-1)
    {
      mKey = sDataRegistry.insert_data(new SmoothingSplineData(nCoef, coefs, knots), true);  // registry should delete
    }

  SmoothingSplineOperator (std::istream& is)
    : mDF(0), mMinX(0), mMaxX(0), mRngX(0), mKey(-1)  { read_from(is); }

  SmoothingSplineOperator& operator= (SmoothingSplineOperator const& op);
    
  double operator()(double x) const;

  void   read_from (std::istream& is);
  void   print_to  (std::ostream& os) const;
  void   write_to  (std::ostream& os) const;

 private:
  double map_to_0_1 (double x) const;
};

// operator traits

template<>
inline 
std::string operator_traits<SmoothingSplineOperator>::name() { return "spline"; }

template<>
inline
std::string operator_traits<SmoothingSplineOperator>::symbol() { return "S"; }

template<>
inline
std::string operator_traits<SmoothingSplineOperator>::parameters(SmoothingSplineOperator const& f)
{ std::ostringstream os; f.write_to(os);  return os.str(); }

												       
inline
std::ostream& 
operator<<(std::ostream& os, SmoothingSplineOperator const& ssOp)
{
  ssOp.print_to(os);
  return os;
}


class SmoothingSpline
{
  int                  mN;               // number of input values
  double              *mX, *mY, *mW;     // input data and count weights
  double               mMinX, mMaxX, mRngX;
  int                  mDF;              // df for smooth
  int                  mNUnique;         // distinct X values
  double              *mUniqueX, *mUniqueY, *mUniqueW;
  double              *mMappedX;         // unique values on [0,1]
  int                  mNCoef;           // 4 more knots than coefs
  double              *mKnots;
  double              *mCoefs;
  double               mLambda;
  double              *mSmooth;
  std::map<double,int> mMap;             // maps x values to y positions
  
 public:
  typedef std::vector<double> Vector;

  ~SmoothingSpline() { delete_internal_ptrs(); }

  SmoothingSpline ()
    : mN(0), mX(), mY(), mW(), mMinX(0.0), mMaxX(0.0), mRngX(0.0), mDF(0),
    mNUnique(0), mUniqueX(), mUniqueY(), mUniqueW(), mMappedX(),
    mNCoef(0), mKnots(0), mCoefs(0), mLambda(0.0), mSmooth(), mMap()  {    }

  SmoothingSpline (int df, Vector const& x, Vector const& y)
    : mN(x.size()), mX(0), mY(0), mW(0), mMinX(0.0), mMaxX(0.0), mRngX(0.0), mDF(df), 
    mNUnique(0), mUniqueX(0), mUniqueY(0), mUniqueW(0),
    mMappedX(0), mNCoef(0), mKnots(0), mCoefs(0), mLambda(0.0), mSmooth(0), mMap()
    { initialize(x.begin(), y.begin()); }

  SmoothingSpline (int df, Vector const& x, Vector const& y, Vector const& w)
    : mN(x.size()), mX(0), mY(0), mW(0), mMinX(0.0), mMaxX(0.0), mRngX(0.0), mDF(df), 
    mNUnique(0), mUniqueX(0), mUniqueY(0), mUniqueW(0),
    mMappedX(0), mNCoef(0), mKnots(0), mCoefs(0), mLambda(0.0), mSmooth(0), mMap()
    { initialize(x.begin(), y.begin(), w.begin()); }

  template<class Iter>
    SmoothingSpline (int df, Iter x, Iter y, int n)
    : mN(n), mX(), mY(), mW(), mMinX(0.0), mMaxX(0.0), mRngX(0.0), mDF(df),
    mNUnique(0), mUniqueX(), mUniqueY(), mUniqueW(), mMappedX(),
    mNCoef(0), mKnots(0), mCoefs(0), mLambda(0.0), mSmooth(), mMap()
    { initialize(x,y);   }

  template<class Iter>
    SmoothingSpline (int df, Iter x, Iter y, Iter w, int n)
    : mN(n), mX(), mY(), mW(), mMinX(0.0), mMaxX(0.0), mRngX(0.0), mDF(df),
    mNUnique(0), mUniqueX(), mUniqueY(), mUniqueW(), mMappedX(),
    mNCoef(0), mKnots(0), mCoefs(0), mLambda(0.0), mSmooth(), mMap()
    { initialize(x,y,w);   }

  template<class Iter>
    void fill_with_smooth (int df, Iter smth)
    {
      if (df != mDF)
        compute_smooth(df);
      std::copy (mSmooth, mSmooth+mN, smth);
    }
  
  template <class Iter>
    void compute_spline_at_x (int n, Ranges::range<Iter> input, double *s, int order = 0) const
    {
      double *xPtr = new double[n];
      if (xPtr)
      { map_to_0_1(input, xPtr);
	std::cerr << "SMSP: Computing spline at " << xPtr[0] << " " << xPtr[1] << " ... with " << mNCoef << " coefs.\n";
	bvalues(&n, xPtr, s, &order);
	delete xPtr;
      }
      else std::cerr << "\nSMSP: *** Error *** Cannot allocate space to compute spline.\n";
    }
      
  SmoothingSplineOperator spline_operator() const { return SmoothingSplineOperator(mDF, mMinX, mMaxX, mNCoef, mCoefs, mKnots); }
  
 private:

  SmoothingSpline& operator=(SmoothingSpline const&) { return *this; }

  template<class Iter>
    void initialize(Iter x, Iter y)
    {
      alloc_internal_ptrs();
      
      std::copy(x, x+mN, mX);
      std::copy(y, y+mN, mY);
      for (int i=0; i<mN; ++i) mW[i] = 1.0;
      complete_initialization();
    }
	  
  template<class Iter>
    void initialize(Iter x, Iter y, Iter w)
    {
      alloc_internal_ptrs();
      
      std::copy(x, x+mN, mX);
      std::copy(y, y+mN, mY);
      std::copy(w, w+mN, mW);
      complete_initialization();
    }

  template<class Iter>
    void map_to_0_1 (Ranges::range<Iter> x, double *mapped) const
    {
      int    hiCount (0);
      int    loCount (0);
      for (Iter xIter = begin(x); xIter != end(x); ++xIter, ++mapped)
      {
	if (*xIter > mMaxX)
	{ *mapped = 1.0;
	  ++hiCount;
	}
	else if (*xIter < mMinX)
	{ *mapped = 0.0;
	  ++loCount;
	}
	else
	  *mapped = (*xIter - mMinX)/mRngX;
      }
      if (loCount || hiCount)
	std::cerr << "SMSP: Warning. Values out of range x_min to x_max (" << loCount << " low, " << hiCount << "  high).\n";
    }
  
  void   print_unique_data       () const;
  void   complete_initialization ();
      
  void   alloc_internal_ptrs     ();
  void   delete_internal_ptrs    ();

  void   setup_unique_values     ();

  void   find_knots              ();

  void   compute_smooth(int df);
  void   bvalues(int *n, double *x, double *s, int *order) const;
};

  
/*
  n 		= (int *)     XLSXargv(parms,1);   length 
  p 		= (int *)     XLSXargv(parms,2);   number unique  
  match	        = (int *)     XLSXargv(parms,3);   map into shorter       
  x 		= (double *)  XLSXargv(parms,4);   x,y,w vectors
  y 		= (double *)  XLSXargv(parms,5);
  w 		= (double *)  XLSXargv(parms,6);
  xbar	        = (double *)  XLSXargv(parms,7);   returned x avg
  ybar   	= (double *)  XLSXargv(parms,8);   returned y avg 
  wbar      	= (double *)  XLSXargv(parms,9);   returned w avg
  work	        = (double *)  XLSXargv(parms,10);	temp (n) 

      extern double bvalue_();
      
  n		= (int *)     XLSXargv(parms,1);
  knot	        = (double *)  XLSXargv(parms,2);
  coef	        = (double *)  XLSXargv(parms,3);
  nk		= (int *)     XLSXargv(parms,4);
  x		= (double *)  XLSXargv(parms,5);
  s		= (double *)  XLSXargv(parms,6);
  order	        = (int *)     XLSXargv(parms,7);

void qsbart (params)

  dblParms      = (double *) XLSXargv(params,1);   penalt, dofoff, crit, lambda 
  lngParms      = (int   *)  XLSXargv(params,2);   n, nk, isetup, ld4, ldnk, ier 
  xs 		= (double *) XLSXargv(params,3);   vec 
  ys 		= (double *) XLSXargv(params,4);   vec 
  ws 		= (double *) XLSXargv(params,5);   vec 
  knot  	= (double *) XLSXargv(params,6);  
  coef	        = (double *) XLSXargv(params,7);  
  sz		= (double *) XLSXargv(params,8); vec
  lev		= (double *) XLSXargv(params,9);  vec
  iparms	= (int *)    XLSXargv(params,10); vec
  parms	        = (double *) XLSXargv(params,11); vec
  scrtch	= (double *) XLSXargv(params,12); vec

*/

#endif
