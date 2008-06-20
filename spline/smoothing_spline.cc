// $Id: smoothing_spline.cc,v 1.24 2005/06/14 22:10:27 bob Exp $

/*
  From the spline code of Trevor for use in XLisp.

  Warning: the mNKnots held in these objects is the number of knots used.  The ftoc code likes to
  increment this by 4, so it has to be decremented prior to calling the C routines.  Go figure.

   5 Sep 04 ... Move the rounding to be done here.
  26 Apr 04 ... Adapted to include in auction modeling.
   4 Oct 95 ... Insert MW comments to remove some decs that are not ANSI.
*/

#include "smoothing_spline.h"
#include "linpack.h"
#include "range_ops.h"

#include <set>
#include <math.h>
#include <iostream>
#include <fstream>
#include <numeric>

namespace {

  const double precision (1000.0);         // rounding on 0/1 to avoid knot singularity

  double my_round(double x)  { return ((double) round(precision * x))/ precision;  }

}

//  SmoothingSplineData  SmoothingSplineData  SmoothingSplineData  SmoothingSplineData  SmoothingSplineData

void
SmoothingSplineData::allocate_pointers()
{
  mKnots = new double[mNCoef+4];
  mCoefs = new double[mNCoef];
}

void
SmoothingSplineData::delete_pointers()
{
  if (mKnots) delete mKnots;
  if (mCoefs) delete mCoefs;
}

void
SmoothingSplineData::copy_data(double const* coefs, double const* knots)
{
  if (mCoefs) std::copy(coefs, coefs+mNCoef , mCoefs);
  else std::cerr << "SMSP: Warning. Copying empty smoothing spline data record.\n";
  if (mKnots) std::copy(knots, knots+nKnot(), mKnots);
}

void
SmoothingSplineData::read_from(std::istream& is)
{
  is >> mNCoef;
  allocate_pointers();
  for (int i=0; i<mNCoef; ++i)
    is >> mCoefs[i];
  for (int i=0; i<nKnot(); ++i)
    is >> mKnots[i];
}

void
SmoothingSplineData::write_to(std::ostream& os) const
{
  os  <<  mNCoef << " ";
  for (int i=0; i<mNCoef; ++i)
    os << mCoefs[i] << " ";
  os << std::endl;
  for (int i=0; i<nKnot(); ++i)
    os << mKnots[i] << " ";
}

// SmoothingSplineOperator  SmoothingSplineOperator  SmoothingSplineOperator  SmoothingSplineOperator  SmoothingSplineOperator

DataRegistry<SmoothingSplineData> SmoothingSplineOperator::sDataRegistry;

SmoothingSplineOperator&
SmoothingSplineOperator::operator= (SmoothingSplineOperator const& op)
{
  mDF   = op.mDF;
  mMinX = op.mMinX;
  mMaxX = op.mMaxX;
  mRngX = op.mRngX;
  mKey  = op.mKey;
  sDataRegistry.add_reference(mKey);   // inform registry of another link to data
  return *this;
}

void
SmoothingSplineOperator::print_to (std::ostream& os) const
{
  os << "Smoothing spline operator on [" << mMinX << "," << mMaxX << "] using data from key " << mKey << ".";
}

void
SmoothingSplineOperator::read_from (std::istream& is)
{
  is >> mDF >> mMinX >> mMaxX;
  mRngX = mMaxX - mMinX;
  std::cerr << "SMSP: Read prefix df " << mDF << " and range " << mMinX << " to " << mMaxX << " from input stream.\n";
  mKey = sDataRegistry.insert_data(new SmoothingSplineData(is), true);
}

void
SmoothingSplineOperator::write_to (std::ostream& os) const
{
  SmoothingSplineData *data (sDataRegistry.data(mKey));
  os << mDF << " " <<  mMinX << " " <<  mMaxX << " ";
  data->write_to(os);
}

double
SmoothingSplineOperator::map_to_0_1 (double x) const
{
  if ((x > mMaxX) || (x < mMinX)) 
  { std::cout << "SMSP: Warning. " << x << " outside range [" << mMinX << ", " << mMaxX << "]\n";
    x = (x < mMinX) ? mMinX : mMaxX;
  }
  return (x - mMinX)/mRngX;
}

namespace     // Smoothing spline object and operator need these
{

#include "smoothing_spline.Fortran.cc"

} // end of namespace

double
SmoothingSplineOperator::operator()(double x) const
{
  int order (0);
  int four  (4);
  SmoothingSplineData *data (sDataRegistry.data(mKey));
  int nc    (data->nCoef());
  double mx (map_to_0_1(my_round(x)));
  return bvalue_(data->knots(), data->coefs(), &nc, &four, &mx, &order);
}


//  SmoothingSpline  SmoothingSpline  SmoothingSpline  SmoothingSpline  SmoothingSpline  SmoothingSpline  SmoothingSpline  SmoothingSpline  

void
SmoothingSpline::complete_initialization ()
{
  setup_unique_values();
  map_to_0_1(make_range(mUniqueX, mUniqueX+mNUnique), mMappedX);
  find_knots();
  std::cerr << "SMSP: Initialization in progress; finding " << mNCoef << " coefs.\n";
  compute_smooth(mDF);
  // print_unique_data();
}

void
SmoothingSpline::print_unique_data () const
{
  std::cerr << "  i    uX     mapped    uY    uW \n";
  for (int i=0; i<mNUnique; ++i)
    std::cerr << i << "  " << mUniqueX[i] << "  " << mMappedX[i] << "       " << mUniqueY[i] << "  " << mUniqueW[i] << std::endl;
} 

void
SmoothingSpline::alloc_internal_ptrs()
{
  mX       = new double[mN];
  mY       = new double[mN];
  mW       = new double[mN];
  mUniqueX = new double[mN];          // max space, will get reduced in size
  mUniqueY = new double[mN];
  mUniqueW = new double[mN];
  mMappedX = new double[mN];
  mCoefs   = new double[mN + 6];      // May have more than data
  mKnots   = new double[mN + 6 + 4];  // 4 more than coefs
  mSmooth  = new double[mN];
  if ((mX == 0) || (mKnots == 0) || (mSmooth == 0))
    std::cerr << "\nSMSP: *** Allocation of internal pointers failed. *** \n\n";
}

void
SmoothingSpline::delete_internal_ptrs()
{
  if (mX)       delete mX;
  else std::cerr << "SMSP: Warning. Smoothing spline destructor given empty object.\n";
  if (mY)       delete mY;
  if (mW)       delete mW;
  if (mUniqueX) delete mUniqueX;
  if (mUniqueY) delete mUniqueY;
  if (mUniqueW) delete mUniqueW;
  if (mMappedX) delete mMappedX;
  if (mKnots)   delete mKnots;
  if (mCoefs)   delete mCoefs;
  if (mSmooth)  delete mSmooth;
}


void
SmoothingSpline::find_knots()
{
  int allKnots (0);
  int nKnots (0);
  std::cerr << "SMSP: Calling sknot with mNUnique=" << mNUnique << " and " << mNCoef << " coefficients.\n";
  sknot(mMappedX, &mNUnique, mKnots, &nKnots, &allKnots);
  mNCoef = nKnots-4;
}

void
SmoothingSpline::compute_smooth(int df)
{
  mDF = df;
  int     nk  (mNCoef + 4);
  int     dim (nk * (nk+17) );
  double *lev    = new double[mN];
  double *smth   = new double[mN];
  double *scrtch = new double[dim];
  if ((scrtch == 0) || (lev == 0) || (smth == 0))
  { std::cerr << "SMSP: *** Error *** Unable to allocate temp space to compute smoothing spline.\n";
    if (lev) delete lev;
    if (smth) delete smth;
    return;
  }
  double  lambda (1.0);
  double  dParms[4] = {1.0, df, 1.0, lambda};
  int     iParms[6] = {mNUnique, mNCoef, 0, 4, 1, 0};
  int     iCrit[2]  = {3,0};
  /*
  std::cerr << "SMSP: Calling qsbart with arguments \n"
	    << "       dParms = " << dParms[0] << " " << dParms[1] << " " << dParms[2] << " " << dParms[3] << std::endl
	    << "       iParms = " << iParms[0] << " " << iParms[1] << " " << iParms[2] << " " << iParms[3] << " "
	    << iParms[4] << " " << iParms[5] << std::endl;
  */
  qsbart(dParms, iParms, mMappedX, mUniqueY, mUniqueW, mKnots, mCoefs, smth, lev, iCrit, scrtch);
  if (iParms[5])
    std::cerr << "SMSP: *** Non-zero error code returned from qsbart, ier/info = " << iParms[5] << std::endl;
  mLambda = dParms[3];
  double totalLev (std::accumulate(lev, lev+mNUnique, 0.0)); 
  std::cerr << "SMSP: Lambda set to " << mLambda << " with total leverage " << totalLev << std::endl;
  double ddf (df);
  if (abs((totalLev - ddf)/ddf) > 0.05)
  { std::cerr << "SMSP: *** Warning. Sum of leverages not within 5% of target df.\n";
    std::ofstream output("test/problem_xy.txt");
    if(output)
    { std::cerr << "SMSP: Dumping (x,y,y^) data for " << mN << " cases to test/problem_xy.txt.\n";
      output << "x y y_hat" << std::endl;
      for (int i=0; i<mN; ++i)
        output << mX[i] << " " << mY[i] << " " << smth[i] << std::endl;
      output.close();
    }
    else std::cerr << "SMSP: Could not open file to dump x,y data.\n";
  }
  for (int i=0; i<mN; ++i)
    mSmooth[i] = smth[ mMap[mX[i]] ];
  delete smth;
  delete lev;
  delete scrtch;
}

void
SmoothingSpline::setup_unique_values()
{
  std::set<double>         uniq;
  std::map<double, Vector> yMap;
  std::map<double, Vector> wMap;
  for (int i=0; i<mN; ++i)
  { mX[i] = my_round(mX[i]);     // round to avoid division by zero in knots
    uniq.insert(mX[i]);                // this step also sorts the x values
    yMap[mX[i]].push_back(mY[i]);
    wMap[mX[i]].push_back(mW[i]);
  }
  std::set<double>::const_iterator it (uniq.begin());
  for (int i=0; it != uniq.end(); ++it, ++i)
  { double x = *it;
    mUniqueW[i] = range_ops::accumulate(make_range(wMap[x]),0.0);
    mUniqueX[i] = x;
    mUniqueY[i] = range_ops::inner_product(make_range(yMap[x]),make_range(wMap[x]),0.0) / mUniqueW[i];
  }
  mNUnique = uniq.size();
  mMinX = mUniqueX[0];
  mMaxX = mUniqueX[mNUnique-1];
  if (mMinX == mMaxX)
  { std::cerr << "SMSP: *** Error *** X values are constant.\n";
    return;
  }
  mRngX = mMaxX - mMinX;
  std::cerr << "SMSP: Found " << mNUnique << " unique values of X on range "
	    << mUniqueX[0] << " to " << mUniqueX[mNUnique-1] << ".\n";
  // Expand range just a little
  mMinX -= 1.0/precision;
  mMaxX += 1.0/precision;
  mRngX = mMaxX - mMinX;
  for (int i=0; i<mNUnique; ++i)
    mMap[mUniqueX[i]]= i;
}

void
SmoothingSpline::bvalues(int *n, double *x, double *s, int *order) const
{
    int c__4 (4);
    int nc (mNCoef);
    for (int i = 0; i < *n; ++i)
    {
      // std::cout << "SMSP: Calling bvalue with args " << *mKnots << " " << *mCoefs << " " << nc << " " << x[i] << "  order " << *order << std::endl;
      s[i] = bvalue_(mKnots, mCoefs, &nc, &c__4, &x[i], order);
    }
} 


