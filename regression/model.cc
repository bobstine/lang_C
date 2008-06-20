/* $Id: model.cc,v 1.6 2003/02/09 20:03:13 bob Exp $
   
     8 Feb 02 ... Implementation begun
     
*/

#include "model.h"

#include "statistic.h"
#include "range.h"

// #include "print_utils.h"  // debug

////  Model  ////

void
Model::print_to (std::ostream& os) const
{
  int q (number_of_predictors());
  
  os << std::endl << " ___________ Regression Model Summary ___________  \n";
  os << "  nObs =  " << mData.nRows() << "      sum wts = " << mData.sum_weights() << std::endl;
  os << "  Y    =  " << mY << std::endl;
  os <<  " X [" << q << "] : " << std::endl;
  for (int j=0; j<q; ++j)
    os << "   " << mX[j] << std::endl;
  os << mRegr;
}  

std::ostream&
operator<< (std::ostream& os, const Model& regr)
{ regr.print_to(os); return os; }


////  Accessors  ////

double
Model::y_bar() const {  return mRegr.YBar(); }

std::vector<double>
Model::x_bar() const
{
  int q (number_of_predictors());
  std::vector<double> result(q,0.0);
  const double* ptr(mRegr.XBar());
  std::copy(ptr, ptr+q, result.begin());
  return result;
}

std::vector<double>
Model::beta() const
{
  int q (number_of_predictors());
  std::vector<double> b;
  const double* bPtr(mRegr.beta());
  std::copy(bPtr, bPtr+1+q, std::back_inserter< std::vector<double> >(b));
  return b;
}
  
Model::const_range 
Model::residuals() const
{
  const double* ptr(mRegr.Residuals());
  return  std::make_pair(ptr,ptr + number_of_rows());
}
  
Model::const_range
Model::fitted_values() const
{
  const double* ptr(mRegr.Fit());
  return  std::make_pair(ptr,ptr + number_of_rows());
}


////  Calculation  ////


std::pair<double,double>
Model::gaussian_predictor_evaluation (Variable z, const double zBar)
{
  int q (number_of_predictors());
  mLastZ    = z;
  mLastZtZ  = sum_of_squares(z, zBar, mData);
  mLastZtY  = cross_product(z,zBar, mY,y_bar(), mData);
  for (int j=0; j<q; ++j)    mLastZtX[j] = 0.0;
  cross_product_vector(z, zBar, mX, x_bar(), std::make_pair(mLastZtX,mLastZtX+q), mData);
  // std::clog << "x bar " << x_bar()
  //	    << "ztz " << mLastZtZ << "  zty " << mLastZtY << "  ztx[0] " << mLastZtX[0] << std::endl;
  return mRegr.Gaussian_evaluation(mLastZtZ, mLastZtX, mLastZtY);
}

std::pair<double, double>
Model::bennett_predictor_evaluation (Variable z, double zBar)
{
  // How should we check that this is the same predictor just checked??? (and so dont need to compute again)
  // test if z == mLastZ
  return mRegr.Bennett_evaluation(begin(make_unary_range(z,mData)),
				  zBar, mLastZtZ, mLastZtX);
}


double
Model::add_predictor(Variable z, double zBar)
{
  double oldRSS (RSS());
  // How should we check that this is the same predictor just checked??? (and so dont need to compute again)
  // test if z == mLastZ
  gaussian_predictor_evaluation(z,zBar);
  mRegr.AddPredictor(begin(make_unary_range(z,mData)), zBar, mLastZtZ, mLastZtX, mLastZtY);
  mX.push_back(z);
  return oldRSS - RSS();
}

  
