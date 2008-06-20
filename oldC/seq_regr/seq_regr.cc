/* $Id: seq_regr.cc,v 1.14 2004/04/30 04:56:09 bob Exp $
   
     8 Feb 02 ... Implementation begun
     
*/

#include "seq_regr.h"
#include "coding.h"        // from utils
#include "print_utils.h"   // for printing tags

////  Model  ////

void
SequentialRegression::print_to (std::ostream& os) const
{
  os << std::endl << "   ______________ Sequential Regression Model _________________ " << std::endl;
  os << "    n =  " << number_of_observations() << std::endl;
  os << "    q =  " << number_of_predictors() << " with " << mPredictorsEvaluated
     << "      considered and " << mPredictorsSinceAdded << " since last added." << std::endl;
  os << "      Run lengths are:  ";
  for (int i=0; i<number_of_predictors(); ++i) os << mIndex[i] << " ";
  os << std::endl;
  for (int i=0; i<number_of_predictors(); ++i) os << "         [" << i+1 << "] "
						  << mTags[i] << std::endl;
  // os << " with two-part code length " << code_length() << " bits " << std::endl;
  mRegr.write_header_to(os);
}  

std::ostream&
operator<< (std::ostream& os, SequentialRegression const& regr)
{ regr.print_to(os); return os; }


////  Accessors  ////

double
SequentialRegression::y_bar() const
{ return mRegr.YBar(); }

SequentialRegression::Vector
SequentialRegression::x_bar() const
{
  int q (number_of_predictors());
  const double* ptr(mRegr.XBar());
  return make_anonymous_range(make_range(ptr, ptr+q));
}

SequentialRegression::Vector
SequentialRegression::beta() const
{
  int q (number_of_predictors());
  const double* ptr(mRegr.beta());
  return make_anonymous_range(make_range(ptr, ptr+1+q));  // leave room for intercept
}

std::vector<double>
SequentialRegression::z_scores() const
{
  Vector               b   (beta());
  int                  q   (number_of_predictors());
  std::vector<double>  z   (q);
  std::vector<double>  se  (q);
  mRegr.fill_with_se(se.begin());
  std::copy(begin(b), end(b), z.begin());
  for (int i=0; i < q; ++i)
    z[i] /= se[i];
  return z;
}


SequentialRegression::Vector
SequentialRegression::residuals() const
{
  const double* ptr(mRegr.residuals());
  return  make_anonymous_range(ptr,ptr + number_of_observations());
}
  
SequentialRegression::Vector
SequentialRegression::fitted_values() const
{
  const double* ptr(mRegr.fit());
  return  make_anonymous_range(ptr,ptr + number_of_observations());
}

double
SequentialRegression::PSS (SequentialRegression::Vector const& yCopy) const
{
  const double* pFit(mRegr.fit());
  Iter          pY  (begin(yCopy));
  double ss(0.0);
  for (int i=0; i<number_of_observations(); ++i)
  { double dev (*pFit-*pY);
    ss += dev * dev;
    ++pFit; ++pY;
  }
  return ss;
}

namespace {
  double abs_val(double x) { return (x > 0) ? x : (-x); }
}

  double
SequentialRegression::code_length () const
{
  // first account for data
  double len (gaussian_data_length(number_of_observations(),RSS()));
  // std::clog << std::endl << "SEQR: data length " << len << " ";
  // uses a run-code approach... need vector of z-scores for slopes
  if (number_of_predictors()>0)
  { std::vector<double> z (z_scores());
    std::vector< std::pair<int,double> > jz;
    for (int i=0; i<number_of_predictors(); ++i)
    { // std::clog << " with (j,z) pair " << mIndex[i] << "," << z[i] << "  "; 
      jz.push_back(std::make_pair(mIndex[i],abs_val(z[i])));
    }
    std::clog << std::endl;
    CauchyCoder cc;
    SignedCauchyCoder scc;
    RunLengthCoder<CauchyCoder, SignedCauchyCoder> rlc (cc, scc);
    len += rlc(jz);
  }
  // std::clog << std::endl;
  return len;
}



////  Calculation  ////


bool
SequentialRegression::evaluate_bennett_predictor (Vector x, double xBar)
{
  return evaluate_bennett_predictor (x, xBar, 0.05/mPredictorsSinceAdded);
}

bool
SequentialRegression::evaluate_bennett_predictor (Vector x, double xBar, double threshold)
{
  std::clog << "SEQR: Bennett predictor evaluation..." << std::endl;
  bool add(false);
  ++ mPredictorsEvaluated;
  ++ mPredictorsSinceAdded;
  std::pair<double,double> dRSSPval (gaussian_predictor_stats (x, xBar));
  if (dRSSPval.second <= threshold)
  {
    std::clog << "SEQR: Predictor passes gaussian with p-val "
	      << dRSSPval.second << " < " <<  threshold;
    std::pair<double,double> xTestPval;
    xTestPval = bennett_predictor_stats (x, xBar);
    if (xTestPval.second <= threshold)
    {
      add = true;
      std::clog << " and passes Bennett with p = " << xTestPval.second << std::endl;
    } else std::clog << " but fails Bennett with p = " << xTestPval.second << std::endl;
  }
  return add;
}


bool
SequentialRegression::evaluate_gaussian_predictor (Vector x, double xBar)
{
  return evaluate_gaussian_predictor(x,xBar, 0.05/mPredictorsSinceAdded);
}

bool
SequentialRegression::evaluate_gaussian_predictor (Vector x, double xBar, double threshold)
{
  std::cout << "SEQR: Gaussian predictor evaluation..." << std::endl;
  bool add(false);
  ++ mPredictorsEvaluated;
  ++ mPredictorsSinceAdded;
  std::pair<double,double> dRSSpVal (gaussian_predictor_stats (x, xBar));
  if (dRSSpVal.second <= threshold)
  {
    add = true;
    std::clog << "SEQR: Add predictor, dRSS = " << dRSSpVal.first
	      << ", because pval " << dRSSpVal.second << " <= " << threshold << std::endl;
  }
  else
    std::clog << "SEQR: Rejects since pval " << dRSSpVal.second << " > " << threshold << std::endl;
  return add;
}

std::pair<double,double>
SequentialRegression::ols_predictor_stats (SequentialRegression::Vector z, const double zBar)
{
  int q (number_of_predictors());
  mZ = z;
  mLastZtZ  = range_stats::sum_of_squares(z, zBar);
  mLastZtY  = range_stats::cross_product(z,zBar, mY,y_bar(), mN);
  for (int j=0; j<q; ++j)    mLastZtX[j] = 0.0;
  SequentialRegression::Vector avg(x_bar());
  range_stats::fill_cross_product_vector(z, zBar, mX, avg, mLastZtX);
  return mRegr.ols_predictor_stats(mLastZtZ, mLastZtX, mLastZtY);
}


std::pair<double,double>
SequentialRegression::gaussian_predictor_stats (SequentialRegression::Vector z, const double zBar)
{
  int q (number_of_predictors());
  mZ = z;
  mLastZtZ  = range_stats::sum_of_squares(z, zBar);
  mLastZtY  = range_stats::cross_product(z,zBar, mY,y_bar(), mN);
  // std::clog << "SEQR: for q = " << q << " y bar " << y_bar() << "  z bar " << zBar
  //    << " ztz= " << mLastZtZ << " zty " << mLastZtY << std::endl;
  for (int j=0; j<q; ++j)    mLastZtX[j] = 0.0;
  SequentialRegression::Vector avg(x_bar());
  range_stats::fill_cross_product_vector(z, zBar, mX, avg, mLastZtX);
  // std::clog << "SEQR:   x bar " << x_bar() << "   ztx[0] " << mLastZtX[0] << std::endl;
  return mRegr.Gaussian_evaluation(mLastZtZ, mLastZtX, mLastZtY);
}

std::pair<double, double>
SequentialRegression::bennett_predictor_stats (SequentialRegression::Vector z, double zBar)
{
  // test if z == mLastZ ???
  return mRegr.Bennett_evaluation(begin(z), zBar, mLastZtZ, mLastZtX);
}


double
SequentialRegression::add_predictor(SequentialRegression::ID const& id, SequentialRegression::Vector z, double zBar)
{
  double oldRSS (RSS());
  mTags.push_back (id);
  // test if z == mLastZ ???
  gaussian_predictor_stats (z,zBar); // sets up next step
  mRegr.AddPredictor(begin(z), zBar, mLastZtZ, mLastZtX, mLastZtY);
  mX.push_back(z);
  mIndex.push_back(mPredictorsSinceAdded);
  mPredictorsSinceAdded = 0;
  return oldRSS - RSS();
}  
