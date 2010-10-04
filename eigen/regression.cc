#include "regression.h"

#include <gsl/gsl_math.h>
#include <gsl/gsl_cdf.h>

#include <iomanip>

#include <Eigen/LU>

const double epsilon (1.0e-30);          // used to detect singularlity


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
  std::vector<std::string> thename;
  thename.push_back(name);
  return thename;
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
LinearRegression::initialize(std::string y, std::vector<std::string> const& x)
{
  mNames.push_back(y);
  for (unsigned int i=0; i<x.size(); ++i)
    mNames.push_back(x[i]);
  double yBar (mY.sum()/mN);
  mTotalSS = mY.redux(CenteredSquare(yBar));
  double y0 (mY(0));
  double d0 (y0 - yBar);
  mTotalSS += d0*d0 - y0;                          // patch Eigen reduction initialization with y[0]
  assert(mTotalSS>0);
  build_QR_and_residuals();
}


void
LinearRegression::build_QR_and_residuals()
{
  // std::cout << "\n\nREGR: in QR, X matrix for factoring is \n" << mX << "\n\n\n";
  Eigen::QR<Eigen::MatrixXd> qr(mX);
  mR = qr.matrixR();
  mQ = qr.matrixQ();
  mResiduals = mY - mQ * (mQ.transpose() * mY);    // must group to get proper order of evaluation
  mResidualSS = mResiduals.squaredNorm();  
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
  assert(p() == x.cols());
  Vector b (beta());
  return (x * b.end(x.cols())).cwise() + b(0);    // internal X has leading const column; input X lacks constant
}


//     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests

FStatistic
LinearRegression::f_test_predictor (Vector const& input, int blockSize) const
{
  // add rows to match up with padded internal size
  Matrix z (mX.rows(),1);
  z.corner(Eigen::TopLeft   , mN        ,1) = input;
  z.corner(Eigen::BottomLeft,z.rows()-mN,1).setZero();
  // order matters, do not form the big projection matrix
  Vector zRes (z - mQ * (mQ.transpose() * z));
  int residualDF (mN-2-p());
  assert(residualDF > 0);
  double ssz (zRes.squaredNorm());
  Vector ssx(1);
  ssx(0) = ssz;
  if(ssz < z.size() * epsilon)                // predictor is singular
    return FStatistic();
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
LinearRegression::f_test_predictors (Matrix const& input, int blockSize) const
{
  // add rows to match up with padded internal size
  Matrix z (mX.rows(), input.cols());
  z.corner(Eigen::TopLeft   , mN,input.cols()) = input;
  z.corner(Eigen::BottomLeft, z.rows()-mN, input.cols()).setZero();
  Matrix zRes (z - mQ * (mQ.transpose() * z));
  Vector ssx ((zRes.cwise() * zRes).colwise().sum());
  int residualDF (mN-1-p()-z.cols());
  assert(residualDF > 0);
  Eigen::QR<Eigen::MatrixXd> qr(zRes);
  Matrix R    (qr.matrixR());
  Matrix Q    (qr.matrixQ());
  for (int j=0; j<z.cols(); ++j)
  { if(abs(R(j,j)) < z.rows() * epsilon)                   // predictor j is singular
      return FStatistic();
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
  std::cout << "Adding " << z.cols() << " predictors with entry stats " << fstat << std::endl;
  assert(z.rows() == mN);
  assert((int)names.size() == z.cols());
  // names
  for (unsigned int j=0; j<names.size(); ++j)
    mNames.push_back(names[j]);
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
  os << "Linear Regression        y = " << mNames[0] << std::endl
     << "            Total SS    = " << mTotalSS    << "     R^2 = " << r_squared() << std::endl
     << "            Residual SS = " << mResidualSS << "    RMSE = " << rmse() << std::endl;
  Vector b  (beta());
  Vector se (se_beta());
  os.precision(3);
  os << std::setw(30) << "Intercept " << "  " << std::setw(8) << b[0] << "  " << std::setw(8) << se[0] << "  " << std::setw(8) << b[0]/se[0] << std::endl;
  for (int j = 1; j<mX.cols(); ++j)
    os << std::setw(30) << mNames[j]  << "  " << std::setw(8) << b[j] << "  " << std::setw(8) << se[j] << "  " << std::setw(8) << b[j]/se[j] << std::endl;
}


void
LinearRegression::write_data_to (std::ostream& os) const
{
  int k (mX.cols());
  for(int i=0; i<mN; ++i)
  { os << mY(i) << "\t";
    for (int j=1; j<k-1; ++j)  // skip intercept
      os << mX(i,j) << "\t";
    os << mX(i,k-1) << std::endl;
  }
}


//     ValidatedRegression      ValidatedRegression      ValidatedRegression      ValidatedRegression      ValidatedRegression      ValidatedRegression 


void
ValidatedRegression::print_to(std::ostream& os) const
{
  os.precision(6);
  os << "Validated Regression      n(est) = " << mN << "    n(validate) " << n_validation_cases() << "    ";
  if(mBlockSize > 0)
    os << " with White SE(b=" << mBlockSize << ")";
  os << std::endl
     << "            Validation SS = " << validation_ss() << std::endl
     << mModel;
}


