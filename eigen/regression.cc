#include "regression.h"

#include <Eigen/LU>

LinearRegression::Matrix
LinearRegression::insert_constant(Matrix const& m) const
{
  Matrix result (m.rows(), m.cols()+1);
  result.block(0,0,m.rows(),1).setOnes();
  result.block(0,1,m.rows(),m.cols()) = m;
  return result;
}


LinearRegression::Vector
LinearRegression::beta() const
{
  Matrix Ri (mR.inverse());
  return Ri * Ri.transpose() * mR.transpose() * mQ.transpose() * mY;
}


std::pair<double,double>
LinearRegression::test_new_predictor (Vector const& z) const
{
  Vector zRes (z - mQ * mQ.transpose() * z);
  double xpy  (mResiduals.dot(zRes));
  double num  ((xpy * xpy)/zRes.squaredNorm());
  return std::make_pair( xpy/zRes.squaredNorm(), (n()-1-p()) * num/mResSS);
}


void
LinearRegression::initialize()
{
  Eigen::QR<Eigen::MatrixXd> qr(mX);
  mR = qr.matrixR();
  mQ = qr.matrixQ();
  mResiduals = mY - mQ * mQ.transpose() * mY;
  mResSS = mResiduals.squaredNorm(); 
}


void
LinearRegression::print_to (std::ostream& os) const
{
  os << "Linear Regression" << std::endl
     << beta() << std::endl;
}
