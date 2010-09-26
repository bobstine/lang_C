#include "regression.h"

#include "stat_utils.h"

#include <Eigen/LU>

const double epsilon (1.0e-30);          // used to detect singularlity


//   Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    



LinearRegression::Matrix
LinearRegression::insert_constant(Matrix const& m) const
{
  Matrix result (m.rows(), m.cols()+1);
  result.block(0,0,m.rows(),1).setOnes();
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
  double yBar (mY.sum()/mY.size());
  mTotalSS = mY.redux(CenteredSquare(yBar));
  double y0 (mY(0));
  double d0 (y0 - yBar);
  mTotalSS += d0*d0 - y0;  // patch Eigen reduction initialization with y[0]
  assert(mTotalSS>0); 
  Eigen::QR<Eigen::MatrixXd> qr(mX);
  mR = qr.matrixR();
  mQ = qr.matrixQ();
  mResiduals = mY - mQ * mQ.transpose() * mY;
  mResidualSS = mResiduals.squaredNorm();
}


//     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes

LinearRegression::Vector
LinearRegression::beta() const
{
  Matrix Ri (mR.inverse());
  return Ri * Ri.transpose() * mR.transpose() * mQ.transpose() * mY;
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


std::pair<double,double>
LinearRegression::f_test_predictor (Vector const& z, int blockSize) const
{
  Vector zRes (z - mQ * mQ.transpose() * z);
  double ssz (zRes.squaredNorm());
  if(ssz < z.size() * epsilon)                // predictor is singular
    return std::make_pair(0.0,0.0);
  int residualDF (n()-2-p());
  double ze  (mResiduals.dot(zRes));          //  slope of added var is  gamma (epz/zRes.squaredNorm);
  if (blockSize==0)
  { double regrss ((ze * ze)/ssz);
    return Stat_Utils::f_test (regrss, 1, mResidualSS-regrss, residualDF);
  }
  else                                        // reduces to (z'e)^2/(z'e^2z)
  { zRes = zRes.cwise() * mResiduals;
    double zeez (zRes.squaredNorm());
    return Stat_Utils::f_test (ze*ze/zeez, 1, residualDF);
  }
}



//     Printing     Printing     Printing     Printing     Printing     Printing     Printing     Printing     Printing     
void
LinearRegression::print_to (std::ostream& os) const
{
  os.precision(4);
  os << "Linear Regression               RMSE = " << rmse()      << "        R^2 = " << r_squared() << std::endl
     << "                         Residual SS = " << mResidualSS << "   Total SS = " << mTotalSS << std::endl;
  os.precision(3);
  Vector b  (beta());
  Vector se (se_beta());
  os << "Beta : " << b.transpose()              << std::endl
     << " SE  : " << se.transpose()             << std::endl
     << " t   : " << (b.cwise()/se).transpose() << std::endl;
}
