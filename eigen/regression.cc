#include "regression.h"

#include <gsl/gsl_math.h>
#include <gsl/gsl_cdf.h>

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
  z.start(v.size) = v;
  z.end(k).setZero();
  return z;
}


LinearRegression::Matrix
LinearRegression::insert_constant(Matrix const& m) const       // add prefix 1 col, rows for shrinkage
{
  assert(m.rows() == mN);
  Matrix result (mN, m.cols()+1);
  result.block(0,0,mN,1).setOnes();
  result.block(0,1,m.rows(),m.cols()) = m;
  return result;
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
  double yBar (mY.start(mN).sum()/mN);
  mTotalSS = mY.start(mN).redux(CenteredSquare(yBar));
  double y0 (mY(0));
  double d0 (y0 - yBar);
  mTotalSS += d0*d0 - y0;                          // patch Eigen reduction initialization with y[0]
  assert(mTotalSS>0);
  mShrinkage = Vector(mX.cols()+1);                // do not shrink initial variables
  mShrinkage.setZero();
  build_QR_and_residuals();
}


void
LinearRegression::build_QR_and_residuals()
{
  Eigen::QR<Eigen::MatrixXd> qr(mX);
  mR = qr.matrixR();
  mQ = Matrix(mN + mX.cols(), mX.cols());
  mQ.corner(Eigen::TopLeft, mN, mX.cols) = qr.matrixQ();
  mResiduals = mY - mQ * (mQ.transpose() * mY);    // must group to get proper order of evaluation
  mResidualSS = mResiduals.start(mN).squaredNorm();  
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
  if(ssz < z.size() * epsilon)                // predictor is singular
    return FStatistic();
  double ze  (zRes.dot(mResiduals));          //  slope of added var is  gamma (epz/zRes.squaredNorm);
  Vector ssx(1);
  ssx(0) = zRes.dot(zRes);
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
  Vector ssx ((Q.cwise() * Q).colwise().sum());
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
LinearRegression::add_predictors  (Matrix const& z, FStatistic const& fstat)
{
  std::cout << "Adding predictor of dim " << z.rows() << "x" << z.cols() << std::endl;
  assert(z.rows() == mN);
  Matrix X(mX.rows()+z.cols(),mX.cols()+z.cols());
  X.corner(Eigen::TopLeft,mX.rows(), mX.cols()) = mX;
  X.corner(Eigen::TopRight, mN, z.cols()) = z;
  X.corner(Eigen::BottomLeft,z.cols(), mX.cols()).setZero();
  X.corner(Eigen::BottomRight,X.rows()-mN,z.cols()).setZero();
  mX = X;
  double f (fstat.f_stat());   
  if (f > 0)  // add in terms for shrinkage
  { formX(mN
    std::cout << "\n\n  X matrix \n" << mX << "\n\n\n";
  build_QR_and_residuals();
}


//     Printing     Printing     Printing     Printing     Printing     Printing     Printing     Printing     Printing     
void
LinearRegression::print_to (std::ostream& os) const
{
  os.precision(4);
  os << "Linear Regression        Total SS    = " << mTotalSS    << "     R^2 = " << r_squared() << std::endl
     << "                         Residual SS = " << mResidualSS << "    RMSE = " << rmse() << std::endl;
  Vector b  (beta());
  Vector se (se_beta());
  Matrix summary(1+p(),3);
  summary << b, se, (b.cwise()/se);
  os.precision(3);
  os << summary << std::endl;
}
