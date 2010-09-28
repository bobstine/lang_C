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
  Eigen::QR<Eigen::MatrixXd> qr(zRes);
  double ssz (zRes.squaredNorm());
  if(ssz < z.size() * epsilon)                // predictor is singular
    return std::make_pair(0.0,0.0);
  int residualDF (n()-2-p());
  double ze  (zRes.dot(mResiduals));          //  slope of added var is  gamma (epz/zRes.squaredNorm);
  if (blockSize==0)
  { double regrss ((ze * ze)/ssz);
    return Stat_Utils::f_test (regrss, 1, mResidualSS-regrss, residualDF);
  }
  else                                       // compute white estimate; in scalar case, reduces to (z'e)^2/(z'e^2z)
  { double zeez (0.0);
    if (blockSize == 1)
      zeez = (zRes.cwise() * mResiduals).squaredNorm();
    else
    { assert (0 == n() % blockSize);
      for(int row=0; row<n(); row +=blockSize)
      { double ezi (mResiduals.segment(row,blockSize).dot(zRes.segment(row,blockSize)));
	zeez += ezi * ezi;
      }
    }
    return Stat_Utils::f_test (ze*ze/zeez, 1, residualDF);
  }
}


std::pair<double,double>
LinearRegression::f_test_predictors (Matrix const& z, int blockSize) const
{
  Matrix zRes (z - mQ * mQ.transpose() * z);
  Eigen::QR<Eigen::MatrixXd> qr(zRes);
  Matrix R    (qr.matrixR());
  Matrix Q    (qr.matrixQ());
  for (int j=0; j<z.cols(); ++j)
  { if(abs(R(j,j)) < z.rows() * epsilon)                   // predictor j is singular
      return std::make_pair(0.0,0.0);
  }
  int residualDF (n()-1-p()-z.cols());
  Vector Qe  (Q.transpose() * mResiduals);           // projection into space of new variables
  if (blockSize==0)
  { double regrss (Qe.squaredNorm());
    return Stat_Utils::f_test (regrss, z.cols(), mResidualSS-regrss, residualDF);
  }
  else
  { Matrix Ri     (R.inverse());
    Vector rGamma (Ri.transpose() * Ri * Qe);
    if (blockSize == 1)
    { Matrix eQ = mResiduals.asDiagonal() * Q;
      Vector eQRg   (eQ * rGamma); 
      return Stat_Utils::f_test (eQRg.squaredNorm(), z.cols(), residualDF);
    }
    else // blocksize > 1
    { assert(0 == n() % blockSize);
      int p (z.cols());
      Matrix QeeQ(p,p); QeeQ.setZero();
      int row (0);
      Vector eQ(p);
      for(int block = 0; block<n()/blockSize; ++block)
      {	eQ = mResiduals.segment(row,blockSize).transpose() * Q.block(row,0,blockSize,p);
	QeeQ += eQ * eQ.transpose();
	row += blockSize;
      }
      double f = rGamma.dot(QeeQ * rGamma);
      return Stat_Utils::f_test (f, z.cols(), residualDF);
    }
  }
}



//     Printing     Printing     Printing     Printing     Printing     Printing     Printing     Printing     Printing     
void
LinearRegression::print_to (std::ostream& os) const
{
  os.precision(4);
  os << "Linear Regression        Total SS    = " << mTotalSS    << "     R^2 = " << r_squared() << std::endl
     << "                         Residual SS = " << mResidualSS << "    RMSE = " << rmse() << std::endl;
  os.precision(3);
  Vector b  (beta());
  Vector se (se_beta());
  os << "Beta : " << b.transpose()              << std::endl
     << " SE  : " << se.transpose()             << std::endl
     << " t   : " << (b.cwise()/se).transpose() << std::endl;
}
