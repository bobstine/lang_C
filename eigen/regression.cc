#include "regression.h"

//#include <gsl/gsl_math.h>
//#include <gsl/gsl_cdf.h>

#include <iomanip>

#include <Eigen/LU>

const double epsilon (1.0e-50);          // used to detect singularlity

double abs_val(double x) { if (x < 0.0) return -x; else return x; }

//   Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    Initialize

LinearRegression::Matrix
LinearRegression::init_x_matrix() const
{
  Matrix x(mN+1,1);
  if (is_ols())
    x.setOnes();
  else
    x.col(0).start(mN) = mSqrtWeights;
  x(mN,0) = 0.0;
  return x;
}

LinearRegression::Matrix
LinearRegression::init_x_matrix(Matrix const& m) const       // add prefix 1 col, rows for shrinkage
{
  assert(m.rows() == mN);
  int nr (m.cols()+1);
  Matrix result (mN+nr, nr);
  if(is_ols())
  { result.col(0).start(mN).setOnes();
    result.corner(Eigen::TopRight,mN,m.cols()) = m;
  }
  else
  { result.col(0).start(mN) = mSqrtWeights;
    result.corner(Eigen::TopRight,mN,m.cols()) = mSqrtWeights.asDiagonal() * m;
  }
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


void
LinearRegression::initialize()
{
  if(is_wls())
    mY = mY.cwise() * mSqrtWeights;
  double yBar (mY.dot(mX.col(0).start(mN))/mX.col(0).squaredNorm());
  mTotalSS = (mY.cwise() - yBar).squaredNorm();
  // std::cout << "******  yBar = " << yBar << "   TSS = " << mTotalSS << std::endl;
  assert(mTotalSS>0);
  build_QR_and_residuals();
}


void
LinearRegression::build_QR_and_residuals()
{
  Eigen::QR<Eigen::MatrixXd> qr(mX);
  mR = qr.matrixR();
  mQ = qr.matrixQ().corner(Eigen::TopRight, mN, mX.cols());   // avoid the extra rows used to obtain shrinkage, ie keep as n x (q+1)
  mResiduals = mY - mQ * (mQ.transpose() * mY);               // must group to get proper order of evaluation
  mResidualSS = mResiduals.squaredNorm();
  if (mX.cols()==1)                                           // set total SS using this more accurate calculation
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
    return Vector::Constant(x.rows(),b(0));
  else
    return (x * b.end(x.cols())).cwise() + b(0);    // internal X has leading const column; input X lacks constant
}


//     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests

FStatistic
LinearRegression::f_test_predictor (Vector const& z, int blockSize) const
{
  // order matters, do not form the big projection matrix
  // note that Q is held only with the top n rows
  //  Vector zRes (z - mQ * (mQ.transpose() * z));
  Vector zRes;
  if (is_wls())
  { Vector wz (z.cwise() * mSqrtWeights);
    zRes = wz - mQ * (mQ.transpose() * wz);
  }
  else
    zRes = z - mQ * (mQ.transpose() * z);
  double ssz  (zRes.squaredNorm());
  assert (ssz >= 0.0);
  int residualDF (mN-2-q());
  assert(residualDF > 0);
  if(std::isnan(ssz))
  { debugging::debug("REGR",1) << " *** Error: Predictor generates NaN in regression." << std::endl;
    return FStatistic();
  }
  if(std::isinf(ssz))
  { debugging::debug("REGR",1) << " *** Error: Predictor generates Inf in regression." << std::endl;
    return FStatistic();
  }
  if(ssz < epsilon)                                 // predictor is singular
  { debugging::debug("REGR",1) << "Predictor appears near singular; after sweeping, residual SS is " << ssz << std::endl;
    return FStatistic();
  }
  Vector sszVec(1); sszVec[0] = ssz;
  double ze  (zRes.dot(mResiduals));     // slope of added var is  gamma (epz/zRes.squaredNorm);
  if (blockSize==0)
  { double regrss ((ze * ze)/ssz);
    return FStatistic(regrss, 1, mResidualSS-regrss, residualDF, sszVec);
  }
  else                                              // compute white estimate; in scalar case, reduces to (z'e)^2/(z'(e^2)z)
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
    debugging::debug("REGR",3) << "F-stat components ze = " << ze << "  zeez = " << zeez << std::endl;
    return FStatistic(ze*ze/zeez, 1, residualDF, sszVec);
  }
}


FStatistic
LinearRegression::f_test_predictors (Matrix const& z, int blockSize) const
{
  // note that Q is held only with the top n rows
  Matrix zRes;
  if (is_wls())
  { Matrix wz (mSqrtWeights.asDiagonal() * z);
    zRes = wz - mQ * (mQ.transpose() * wz);
  }
  else
    zRes = z - mQ * (mQ.transpose() * z);
  Vector zResSS (squared_norm(zRes));
  int residualDF (mN-1-q()-z.cols());
  assert(residualDF > 0);
  Eigen::QR<Eigen::MatrixXd> qr(zRes);
  Matrix Q    (qr.matrixQ());
  { // use R matrix only for checking rank conditions
    Matrix R    (qr.matrixR());
    for (int j=0; j<z.cols(); ++j)
    { if(abs_val(R(j,j)) < epsilon)
      { debugging::debug("LINM",2) << "Predictors appear near singular; after sweeping, R("
				   << j << "," << j << ") = " << abs_val(R(j,j)) << "  " << R(j,j) << std::endl;

	std::cout << "       R matrix is\n " << R << std::endl;
	std::cout << "       Collinear predictors in f_test_predictors are \n:";
	std::cout << z.corner(Eigen::TopLeft, 10, z.cols()).transpose() << std::endl;
	std::cout << "       Predictors in f_test_predictors after sweeping are \n:";
	std::cout << zRes.corner(Eigen::TopLeft, 10, z.cols()).transpose() << std::endl;
	
	return FStatistic();
      }
    }
  }
  Vector Qe  (Q.transpose() * mResiduals);                // projection into space of new variables
  int p (z.cols());
  if (blockSize==0)
  { double regrss (Qe.squaredNorm());
    debugging::debug("REGR",3) << "F-stat components (" << regrss << "/" << p << ")/(" << mResidualSS-regrss << "/" << residualDF << std::endl;
    return FStatistic(regrss, p, mResidualSS-regrss, residualDF, zResSS);
  }
  else
  { Matrix QeeQ(p,p);                                    // Q'ee'Q 
    QeeQ.setZero();
    if (blockSize == 1)
    { Matrix eQ (mResiduals.asDiagonal() * Q);  
      QeeQ = (eQ.transpose() * eQ);
    }
    else     // blocksize > 1
    { assert(0 == mN % blockSize);
      Vector eQ(p);
      for(int block = 0, row = 0; block<mN/blockSize; ++block)
      {	eQ = mResiduals.segment(row,blockSize).transpose() * Q.block(row,0,blockSize,p);
	QeeQ += eQ * eQ.transpose();
	row += blockSize;
      }
    }
    double regrSS = (Qe.transpose() * QeeQ.inverse() * Qe)(0,0);
    debugging::debug("REGR",3) << "F-stat = (" << regrSS << "/" << p << ") with " << residualDF << " residual DF." << std::endl;
    return FStatistic(regrSS, p, residualDF, zResSS);
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
  if (is_ols())
    X.corner(Eigen::TopRight, mN, z.cols()) = z;
  else
    X.corner(Eigen::TopRight, mN, z.cols()) = mSqrtWeights.asDiagonal() * z;
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
  if (is_ols())
    os << "Linear Regression          ";
  else
    os << "Weighted Linear Regression ";
  os << "    y = " << mYName << std::endl
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
  // now put the data in external coordinate system
  Vector y    (is_ols() ? mY : mY.cwise()/mSqrtWeights);
  Vector res  (raw_residuals());
  Vector fit  (is_ols() ? fitted_values() : y - res);
  for(int i=0; i<mN; ++i)
  { os << "est\t" << fit[i] << "\t" << res[i] << "\t" << y[i] << "\t";
    Vector row (mX.row(i));
    if (is_wls())
      row = row / mSqrtWeights[i];
    for (int j=1; j<k-1; ++j)  // skip intercept
      os << row[j] << "\t";
    os << row[k-1] << std::endl;
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

