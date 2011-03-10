#include "regression.h"

#include <iomanip>
#include <bennett.h>

#include <Eigen/LU>

const    double    epsilon (1.0e-50);          // used to detect singularlity
const unsigned int maxNameLen (50);            // max length shown when print model

double abs_val(double x) { if (x < 0.0) return -x; else return x; }
double max_abs(double x, double y) { double ax = abs_val(x); double ay = abs_val(y); if (ax >= ay) return ax; else return ay; }
bool   close (double a, double b) { return abs_val(a-b) < epsilon; }


//   Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    Initialize

bool
LinearRegression::is_binary_vector(Vector const& y)  const
{
  for(int i=0; i<y.size(); ++i)
    if( (y[i] != 0) && (y[i] != 1) )
    { debugging::debug("REGR",2) << "Vector is not binary; found value v[" << i << "] = " << y[i] << std::endl;
      return false;
    }
  debugging::debug("REGR",2) << "Vector is binary" << std::endl;
  return true;
}

LinearRegression::Matrix
LinearRegression::init_x_matrix() const
{
  // extra row holds shrinkage parameter for intercept (which is 0)
  Matrix x(mN+1,1);
  if (is_ols())
    x.setOnes();
  else
    x.col(0).start(mN) = mSqrtWeights;
  x(mN,0) = 0.0;
  return x;
}

LinearRegression::Matrix
LinearRegression::init_x_matrix(Matrix const& m) const 
{
  assert(m.rows() == mN);
  // need extra rows for holding shrinkage parameters
  int nr (m.cols()+1);
  Matrix result (mN+nr, nr);
  // handle ols, wls differently
  if(is_ols())
  { result.col(0).start(mN).setOnes();
    result.corner(Eigen::TopRight,mN,m.cols()) = m;
  }
  else
  { result.col(0).start(mN) = mSqrtWeights;
    result.corner(Eigen::TopRight,mN,m.cols()) = mSqrtWeights.asDiagonal() * m;
  }
  // no shrinkage for those forced into the model
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

LinearRegression::Vector
LinearRegression::fitted_values(double lo, double hi) const
{
  Vector fit = fitted_values();
  for (int i=0; i<fit.size(); ++i)
  { if (fit[i] < lo)      fit[i] = lo;
    else if (fit[i] > hi) fit[i] = hi;
  }
  return fit;
}


//     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes

LinearRegression::Vector
LinearRegression::beta() const
{
  return mR.inverse() * (mQ.transpose() * mY);   // force order for calculations
}

LinearRegression::Vector
LinearRegression::shrinkage_lambda()       const
{
  int d (mX.cols());
  return mX.corner(Eigen::BottomLeft, d,d).diagonal();
}


LinearRegression::Vector
LinearRegression::se_beta_ols() const
{
  Matrix Ri (mR.inverse());
  Vector diag (Ri.rows());
  for (int i=0; i<diag.size(); ++i)
    diag(i) = Ri.row(i).squaredNorm();
  return rmse() * diag.cwise().sqrt();
}

LinearRegression::Vector
LinearRegression::se_beta() const
{
  if (mBlockSize==0)
    return se_beta_ols();
  else // compute sandwich estimates        // Note: these will be larger than entry F since residuals are updated once added
  { int nCols (mX.cols());
    Vector se2 (nCols);
    if (mBlockSize==1)
    { Matrix EQRi (mResiduals.asDiagonal() * mQ * mR.inverse().transpose());
      for(int j=0; j < nCols; ++j)
	se2[j] = EQRi.col(j).squaredNorm();
    }
    else // larger blocks, just diagonal
    { Matrix QRi (mQ * mR.inverse().transpose());
      se2.setZero();
      for(int row = 0; row <mN; row+=mBlockSize)
      {	Vector eQRi = mResiduals.segment(row,mBlockSize).transpose() * QRi.block(row,0,mBlockSize,nCols);
	se2 += eQRi.cwise() * eQRi;
      }
    }
    return se2.cwise().sqrt();
  }
}

LinearRegression::Vector
LinearRegression::predictions(Matrix const& x) const
{
  assert(q() == x.cols());
  Vector b (beta());
  if (q() == 0)
    return Vector::Constant(x.rows(),b(0));
  else
    return (x * b.end(x.cols())).cwise() + b(0);    // internal X has leading const column; input X lacks constant
}


//     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests

bool
LinearRegression::is_invalid_ss (double ss)          const
{
  if (ss <= 0.0)
  { debugging::debug("REGR",1) << " *** Error: SS <= 0.0 in regression." << std::endl;
    return true;
  }
  if(std::isnan(ss))
  { debugging::debug("REGR",1) << " *** Error: SS = NaN in regression." << std::endl;
    return true;
  }
  if(std::isinf(ss))
  { debugging::debug("REGR",1) << " *** Error: SS = Inf in regression." << std::endl;
    return true;
  }
  if(ss < epsilon)
  { debugging::debug("REGR",1) << "SS indicates near singular;  SS = " << ss << std::endl;
    return true;
  }
  return false;
}

FStatistic
LinearRegression::f_test_predictor (Vector const& z) const
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
  if (is_invalid_ss (ssz)) return FStatistic();
  int residualDF (mN-2-q());
  assert(residualDF > 0);
  Vector sszVec(1); sszVec[0] = ssz;
  double ze  (zRes.dot(mResiduals));                 // slope of added var is  gamma (epz/zRes.squaredNorm);
  if (mBlockSize==0)
  { double regrss ((ze * ze)/ssz);
    return FStatistic(regrss, 1, mResidualSS-regrss, residualDF, sszVec);
  }
  else                                              
  { if (is_binary() && (mBlockSize == 1))            // use Bennett and fake F stat from squaring bennett t stat
    { std::pair<double,double> test (bennett_evaluation(z));
      debugging::debug("REGR",2) << "Bennett evaluation returns t = " << test.first << " with p-value = " << test.second <<std::endl;
      return FStatistic(test.first*test.first, test.second, 1, mN-q(), sszVec);
    }
    else                                             // compute white estimate; in scalar case, reduces to (z'e)^2/(z'(e^2)z)
    { double zeez (0.0);
      if (mBlockSize == 1)
	zeez = (zRes.cwise() * mResiduals).squaredNorm();
      else
      { assert (0 == mN % mBlockSize);
	for(int row=0; row<mN; row +=mBlockSize)
	{ double ezi (mResiduals.segment(row,mBlockSize).dot(zRes.segment(row,mBlockSize)));
	  zeez += ezi * ezi;
	}
      }
      debugging::debug("REGR",4) << "F-stat components ze = " << ze << "  zeez = " << zeez << std::endl;
      return FStatistic(ze*ze/zeez, 1, residualDF, sszVec);
    }
  }
}


FStatistic
LinearRegression::f_test_predictors (Matrix const& z) const
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
  if (mBlockSize==0)
  { double regrss (Qe.squaredNorm());
    debugging::debug("REGR",3) << "F-stat components (" << regrss << "/" << p << ")/(" << mResidualSS-regrss << "/" << residualDF << std::endl;
    return FStatistic(regrss, p, mResidualSS-regrss, residualDF, zResSS);
  }
  else
  { Matrix QeeQ(p,p);                                    // Q'ee'Q 
    QeeQ.setZero();
    if (mBlockSize == 1)
    { Matrix eQ (mResiduals.asDiagonal() * Q);  
      QeeQ = eQ.transpose() * eQ;
    }
    else     // blocksize > 1
    { assert(0 == mN % mBlockSize);
      Vector eQ(p);
      for(int block = 0, row = 0; block<mN/mBlockSize; ++block)
      {	eQ = mResiduals.segment(row,mBlockSize).transpose() * Q.block(row,0,mBlockSize,p);
	QeeQ += eQ * eQ.transpose();
	row += mBlockSize;
      }
    }
    double ss = (Qe.transpose() * QeeQ.inverse() * Qe)(0,0);
    debugging::debug("REGR",3) << "F-stat = " << ss << "/" << p << " with " << residualDF << " residual DF." << std::endl;
    return FStatistic(ss/p, p, residualDF, zResSS);
  }
}

//    Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     

std::pair<double,double>
LinearRegression::bennett_evaluation (Vector const& z) const
{ const double epsilon (1.0E-10);
  Vector mu      (fitted_values(epsilon, 1.0-epsilon));    // think of fit as E(Y), constrained to [eps,1-eps] interval
  Vector var     (Vector::Ones(mN));  var = mu.cwise() * (var - mu);
  Vector dev     (mY - mu);                                // would match residuals IF other fit is bounded
  double num     (dev.dot(z));                             // z'(y-y^)
  double rootZDZ (sqrt (var.dot(z.cwise() * z)));          // sqrt(z'Dz)
  double maxA    (0.0);
  for (int i=0; i<mN; ++i)
  { double absZ (abs_val(z[i]) * max_abs(mu[i], 1.0-mu[i]));   // largest possible error for this case
    if (absZ > maxA) maxA = absZ;                              // largest?
  }
  double Mz      (maxA/rootZDZ);
  double tz      (abs_val(num)/rootZDZ);                    // num = get(mZE,0)
  return std::make_pair(tz, bennett_p_value(tz,Mz));
}


//     Add predictor     Add predictor     Add predictor     Add predictor     Add predictor     Add predictor     Add predictor

void
LinearRegression::add_predictors  (std::vector<std::string> const& names, Matrix const& z, FStatistic const& fstat)
{
  debugging::debug("LINR",3) << "Adding matrix of predictors with dimension " << z.rows() << " x " << z.cols()
			     << " predictors to regression with X which is " << mX.rows() << " x " << mX.cols() << std::endl;
  debugging::debug("LINR",4) << "Entry stats for added predictor are " << fstat << std::endl;
  assert(z.rows() == mN);
  assert((int)names.size() == z.cols());
  // names
  for (unsigned int j=0; j<names.size(); ++j)
    mXNames.push_back(names[j]);
  // add rows for additional shrinkage parms and columns for new variables
  Matrix X(mX.rows()+z.cols(),mX.cols()+z.cols());
  X.corner(Eigen::TopLeft,    mX.rows(), mX.cols()) = mX;
  X.corner(Eigen::BottomLeft,  z.cols(), mX.cols()).setZero();
  if (is_ols())
    X.corner(Eigen::TopRight, mN, z.cols()) = z;
  else // wls
    X.corner(Eigen::TopRight, mN, z.cols()) = mSqrtWeights.asDiagonal() * z;
  X.corner(Eigen::BottomRight, X.cols(), z.cols()).setZero();
  // shrinkage only occurs if the entry f-stat is non-trivial
  if (fstat.f_stat() > 0) 
  { Vector diag = fstat.sum_of_squares() / fstat.f_stat();
    X.corner(Eigen::BottomRight, z.cols(), z.cols()).diagonal() = diag.cwise().sqrt();
  }
  mX = X;
  build_QR_and_residuals();
}



//     Printing     Printing     Printing     Printing     Printing     Printing     Printing     Printing     Printing     

namespace {
  std::string printed_name(std::string const& s)
  { if (s.length() > maxNameLen)
      return s.substr(0,maxNameLen-1);
    else
      return s;
  }
}

void
LinearRegression::print_to (std::ostream& os) const
{
  os.precision(6);
  if (is_ols())
    os << "Linear Regression          ";
  else
    os << "Weighted Linear Regression ";
  if (is_binary())
    os << "   Binary response";
  os << "  y = " << mYName << std::endl
     << "            Total SS    = " << mTotalSS    << "     R^2 = " << r_squared() << std::endl
     << "            Residual SS = " << mResidualSS << "    RMSE = " << rmse() << std::endl << std::endl;
  Vector b   (beta());
  Vector se  (se_beta());
  Vector lam (shrinkage_lambda());
  os.precision(3);
  if (mBlockSize == 0)
  { os << "                        Variable Name                Estimate     OLS SE       t      Lambda " << std::endl;
    for (int j = 0; j<mX.cols(); ++j)
      os << std::setw(maxNameLen) << printed_name(mXNames[j])  << "  " << std::setw(9) << b[j] << "  " << std::setw(9) << se[j] << "  "
	 << std::setw(8) << b[j]/se[j] << std::setw(8) << lam[j] << std::endl;
  }
  else // show ols and sandwich se
  { Vector olsSE (se_beta_ols());
    os << "                  Variable Name                          Estimate      Sandwich SE     (OLS)        t       Lambda " << std::endl;
    for (int j = 0; j<mX.cols(); ++j)
      os << std::setw(maxNameLen) << printed_name(mXNames[j])  << "  " << std::setw(12) << b[j] << "  " << std::setw(12) << se[j]
	 << "(" << std::setw(12) << olsSE[j] << ")  " << std::setw(8) << b[j]/se[j] << std::setw(8) << lam[j] << std::endl;
  }
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
      return (mValidationY - mModel.predictions(mValidationX)).squaredNorm();
  }
  else
    return 0.0;
}

void
ValidatedRegression::print_to(std::ostream& os, bool useHTML) const
{
  if(useHTML)
    os << "Cannot do HTML version at this point" << std::endl;
  os.precision(6);
  os << "Validated Regression      n(est) = " << mN << "    n(validate) = " << n_validation_cases() << "    ";
  if(block_size() > 0)
    os << " with White SE(b=" << block_size() << ")";
  os << std::endl
     << "            Validation SS = " << validation_ss() << std::endl
     << mModel;
}

void
ValidatedRegression::write_data_to(std::ostream& os) const
{
  // these are in the order after sorting (not permuted to the original order)
  mModel.write_data_to(os);
  Vector preds (mModel.predictions(mValidationX));
  for(int i=0; i<mValidationX.rows(); ++i)
  { os << "val\t" << preds[i] << "\t" << mValidationY[i]-preds[i] << "\t" << mValidationY[i] << "\t";
    for (int j=0; j<mValidationX.cols()-1; ++j) 
      os << mValidationX(i,j) << "\t";
    os << mValidationX(i,mValidationX.cols()-1) << std::endl;
  }
}
