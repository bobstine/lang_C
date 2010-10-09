#include "regression.h"

#include <gsl/gsl_math.h>
#include <gsl/gsl_cdf.h>

#include <iomanip>

#include <Eigen/LU>

const double epsilon (1.0e-50);          // used to detect singularlity


//     FStatistic     FStatistic     FStatistic     FStatistic     FStatistic     FStatistic     FStatistic     FStatistic

void
FStatistic::calc_p_value()
{
  if ( (mF <= 0.0) || gsl_isnan(mF) || gsl_isinf(mF) )
    mPValue = 0.0;
  else
    mPValue = gsl_cdf_fdist_Q(mF, mNumDF, mDenDF);    
}


double
FStatistic::critical_value(double p) const
{
  assert (0.0 <= p && p <= 1.0);
  return gsl_cdf_fdist_Qinv(p, mNumDF, mDenDF);
}



//   Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    Initialize

LinearRegression::Vector
LinearRegression::pad_vector(Vector const& v, int k) const
{
  Vector z (v.size()+k);
  z.start(v.size()) = v;
  z.end(k).setZero();
  return z;
}


LinearRegression::Matrix
LinearRegression::initial_x_matrix() const
{
  Matrix x(mN+1,1);
  x.setOnes();
  x(mN,0) = 0.0;
  return x;
}

LinearRegression::Matrix
LinearRegression::insert_constant(Matrix const& m) const       // add prefix 1 col, rows for shrinkage
{
  assert(m.rows() == mN);
  int nr (m.cols()+1);
  Matrix result (mN+nr, nr);
  result.corner(Eigen::TopLeft ,mN,1).setOnes();
  result.corner(Eigen::TopRight,mN,m.cols()) = m;
  result.corner(Eigen::BottomLeft, nr, nr).setZero();
  return result;
}

std::vector<std::string>
LinearRegression::name_vec(std::string name) const
{
  std::vector<std::string> vec(1);
  vec[0] = name;
  return vec;
}


namespace {
  class CenteredSquare : public std::binary_function<double,double,double> {
  private:
    double mCenter;
  public:
    CenteredSquare(double center) : mCenter(center) { }
    double operator()(double total, double x) const { double dev (x - mCenter); return total+dev*dev; }
  };
}

void
LinearRegression::initialize()
{
  double yBar (mY.sum()/mN);
  mTotalSS = (mY.cwise() - yBar).squaredNorm();
  assert(mTotalSS>0);
  build_QR_and_residuals();
}


void
LinearRegression::build_QR_and_residuals()
{
  // std::cout << "\n\nREGR: in QR, X matrix for factoring is \n" << mX << "\n\n\n";
  Eigen::QR<Eigen::MatrixXd> qr(mX);
  mR = qr.matrixR();
  mQ = qr.matrixQ().corner(Eigen::TopRight, mN, mX.cols());// avoid the extra rows used to obtain shrinkage, ie keep as n x (q+1)
  mResiduals = mY - mQ * (mQ.transpose() * mY);            // must group to get proper order of evaluation
  mResidualSS = mResiduals.squaredNorm();
  if (mX.cols()==1)                                        // set total SS using this more accurate calculation
    mTotalSS = mResidualSS;
}



//     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes

LinearRegression::Vector
LinearRegression::beta() const
{
  return mR.inverse() * (mQ.transpose() * mY);   // force order for calculations
}

LinearRegression::Vector
LinearRegression::se_beta() const
{
  Matrix Ri (mR.inverse());
  Vector diag (Ri.rows());
  for (int i=0; i<diag.size(); ++i)
    diag(i) = Ri.row(i).squaredNorm();
  return rmse() * diag.cwise().sqrt();
}

LinearRegression::Vector
LinearRegression::predict(Matrix const& x) const
{
  assert(q() == x.cols());
  Vector b (beta());
  if (q() == 0)
    return Vector::Constant(mY.size(),b(0));
  else
    return (x * b.end(x.cols())).cwise() + b(0);    // internal X has leading const column; input X lacks constant
}


//     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests

FStatistic
LinearRegression::f_test_predictor (Vector const& z, int blockSize) const
{
  // order matters, do not form the big projection matrix
  // note that Q is held only with the top n rows
  Vector zRes (z - mQ * (mQ.transpose() * z));
  int residualDF (mN-2-q());
  assert(residualDF > 0);
  double ssz (zRes.squaredNorm());
  Vector ssx(1);
  ssx(0) = ssz;
  if(ssz < epsilon)                // predictor is singular
  { debugging::debug("LINM",2) << "Predictor appears near singular; after sweeping, residual SS is " << ssz << std::endl;
    return FStatistic();
  }
  double ze  (zRes.dot(mResiduals));          //  slope of added var is  gamma (epz/zRes.squaredNorm);
  if (blockSize==0)
  { double regrss ((ze * ze)/ssz);
    return FStatistic(regrss, 1, mResidualSS-regrss, residualDF, ssx);
  }
  else                                       // compute white estimate; in scalar case, reduces to (z'e)^2/(z'e^2z)
  { double zeez (0.0);
    if (blockSize == 1)
      zeez = (zRes.cwise() * mResiduals).squaredNorm();
    else
    { assert (0 == mN % blockSize);
      for(int row=0; row<mN; row +=blockSize)
      { double ezi (mResiduals.segment(row,blockSize).dot(zRes.segment(row,blockSize)));
	zeez += ezi * ezi;
      }
    }
    return FStatistic(ze*ze/zeez, 1, residualDF, ssx);
  }
}


FStatistic
LinearRegression::f_test_predictors (Matrix const& z, int blockSize) const
{
  // note that Q is held only with the top n rows
  Matrix zRes (z - mQ * (mQ.transpose() * z));
  Vector ssx ((zRes.cwise() * zRes).colwise().sum());
  int residualDF (mN-1-q()-z.cols());
  assert(residualDF > 0);
  Eigen::QR<Eigen::MatrixXd> qr(zRes);
  Matrix R    (qr.matrixR());
  Matrix Q    (qr.matrixQ());
  for (int j=0; j<z.cols(); ++j)
  { if(abs(R(j,j)) < epsilon)                             // predictor j is singular
    { debugging::debug("LINM",2) << "Predictors appear near singular; after sweeping, diag of R = " << abs(R(j,j)) << std::endl;
      return FStatistic();
    }
  }
  Vector Qe  (Q.transpose() * mResiduals);                // projection into space of new variables
  if (blockSize==0)
  { double regrss (Qe.squaredNorm());
    return FStatistic(regrss, z.cols(), mResidualSS-regrss, residualDF, ssx);
  }
  else
  { Matrix Ri     (R.inverse());
    Vector rGamma (Ri.transpose() * Ri * Qe);
    if (blockSize == 1)
    { Matrix eQ = mResiduals.asDiagonal() * Q;
      Vector eQRg   (eQ * rGamma); 
      return FStatistic(eQRg.squaredNorm(), z.cols(), residualDF, ssx);
    }
    else // blocksize > 1
    { assert(0 == mN % blockSize);
      int p (z.cols());
      Matrix QeeQ(p,p); QeeQ.setZero();
      int row (0);
      Vector eQ(p);
      for(int block = 0; block<mN/blockSize; ++block)
      {	eQ = mResiduals.segment(row,blockSize).transpose() * Q.block(row,0,blockSize,p);
	QeeQ += eQ * eQ.transpose();
	row += blockSize;
      }
      return FStatistic(rGamma.dot(QeeQ * rGamma), z.cols(), residualDF, ssx);
    }
  }
}

//     Add predictor     Add predictor     Add predictor     Add predictor     Add predictor     Add predictor     Add predictor

void
LinearRegression::add_predictors  (std::vector<std::string> const& names, Matrix const& z, FStatistic const& fstat)
{
  debugging::debug("LINR",2) << "Adding matrix of predictors with dimension " << z.rows() << " x " << z.cols()
			     << " predictors to regression with X which is " << mX.rows() << " x " << mX.cols() << std::endl;
  if (fstat.f_stat() > 0)
    debugging::debug("LINR",2) << "Entry stats " << fstat << std::endl;
  assert(z.rows() == mN);
  assert((int)names.size() == z.cols());
  // names
  for (unsigned int j=0; j<names.size(); ++j)
    mXNames.push_back(names[j]);
  // add rows and columns
  Matrix X(mX.rows()+z.cols(),mX.cols()+z.cols());
  X.corner(Eigen::TopLeft,    mX.rows(), mX.cols()) = mX;
  X.corner(Eigen::BottomLeft,  z.cols(), mX.cols()).setZero();
  X.corner(Eigen::TopRight, mN, z.cols()) = z;
  X.corner(Eigen::BottomRight, X.cols(), z.cols()).setZero();
  if (fstat.f_stat() > 0)
  { Vector diag = fstat.sum_of_squares() / fstat.f_stat();
    X.corner(Eigen::BottomRight, z.cols(), z.cols()).diagonal() = diag.cwise().sqrt();
  }
  mX = X;
  build_QR_and_residuals();
}


//     Printing     Printing     Printing     Printing     Printing     Printing     Printing     Printing     Printing     
void
LinearRegression::print_to (std::ostream& os) const
{
  os.precision(6);
  os << "Linear Regression        y = " << mYName << std::endl
     << "            Total SS    = " << mTotalSS    << "     R^2 = " << r_squared() << std::endl
     << "            Residual SS = " << mResidualSS << "    RMSE = " << rmse() << std::endl;
  Vector b  (beta());
  Vector se (se_beta());
  os.precision(3);
  for (int j = 0; j<mX.cols(); ++j)
    os << std::setw(50) << mXNames[j]  << "  " << std::setw(9) << b[j] << "  " << std::setw(9) << se[j] << "  " << std::setw(8) << b[j]/se[j] << std::endl;
}


void
LinearRegression::write_data_to (std::ostream& os) const
{
  int k (mX.cols());
  // prefix line with var names
  os << "Role\tFit\tResidual\t" << mYName;
  for(int j=1; j<k; ++j)
    os << "\t" << mXNames[j];
  os << std::endl;
  // now put the data
  Vector fit (fitted_values());
  Vector res (residuals());
  for(int i=0; i<mN; ++i)
  { os << "est\t" << fit[i] << "\t" << res[i] << "\t" << mY[i] << "\t";
    for (int j=1; j<k-1; ++j)  // skip intercept
      os << mX(i,j) << "\t";
    os << mX(i,k-1) << std::endl;
  }
}


//     ValidatedRegression      ValidatedRegression      ValidatedRegression      ValidatedRegression      ValidatedRegression      ValidatedRegression 


double
ValidatedRegression::validation_ss() const
{
  if (n_validation_cases()>0)
  { if (q()==0)     // handle differently for initial case to avoid empty matrix
    { // Vector b (mModel.beta());
      return (mValidationY.cwise() - mModel.beta()(0)).squaredNorm();
    }
    else
      return (mValidationY - mModel.predict(mValidationX)).squaredNorm();
  }
  else
    return 0.0;
}

void
ValidatedRegression::print_to(std::ostream& os, bool useHTML) const
{
  os.precision(6);
  os << "Validated Regression      n(est) = " << mN << "    n(validate) = " << n_validation_cases() << "    ";
  if(mBlockSize > 0)
    os << " with White SE(b=" << mBlockSize << ")";
  os << std::endl
     << "            Validation SS = " << validation_ss() << std::endl
     << mModel;
}

void
ValidatedRegression::write_data_to(std::ostream& os) const
{
  mModel.write_data_to(os);
  Vector preds (mModel.predict(mValidationX));
  for(int i=0; i<mValidationX.rows(); ++i)
  { os << "val\t" << preds[i] << "\t" << mValidationY[i]-preds[i] << "\t" << mValidationY[i] << "\t";
    for (int j=0; j<mValidationX.cols()-1; ++j) 
      os << mValidationX(i,j) << "\t";
    os << mValidationX(i,mValidationX.cols()-1) << std::endl;
  }
}

