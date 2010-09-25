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

LinearRegression::Vector
LinearRegression::se_beta() const
{
  Matrix Ri (mR.inverse());
  Vector diag (Ri.rows());
  for (int i=0; i<diag.size(); ++i)
    diag(i) = Ri.row(i).squaredNorm();
  return rmse() * diag.cwise().sqrt();
}




std::pair<double,double>
LinearRegression::test_new_predictor (Vector const& z) const
{
  Vector zRes (z - mQ * mQ.transpose() * z);
  double epz  (mResiduals.dot(zRes));
  double regrss ((epz * epz)/zRes.squaredNorm());
  return std::make_pair( epz/zRes.squaredNorm(), (n()-2-p()) * regrss/(mResidualSS-regrss)  );  // -2 = 1 for intercept + 1 for new variable
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
   Eigen::QR<Eigen::MatrixXd> qr(mX);
   mR = qr.matrixR();
   mQ = qr.matrixQ();
   mResiduals = mY - mQ * mQ.transpose() * mY;
   mResidualSS = mResiduals.squaredNorm();
   double yBar (mY.sum()/mY.size());
   mTotalSS = mY.redux(CenteredSquare(yBar));
   double y0 (mY(0));
   double d0 (y0 - yBar);
   mTotalSS += d0*d0 - y0;  // patch eigen initialization with y[0]
 }


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
