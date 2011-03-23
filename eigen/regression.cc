#include "regression.h"

#include <Eigen/LU>

#include <iomanip>
#include <bennett.h>

const    double    epsilon (1.0e-50);          // used to detect singularlity
const unsigned int maxNameLen (50);            // max length shown when print model
const unsigned int numberOfAllocatedColumns(400);   

double abs_val(double x) { if (x < 0.0) return -x; else return x; }
double max_abs(double x, double y) { double ax = abs_val(x); double ay = abs_val(y); if (ax >= ay) return ax; else return ay; }
bool   close (double a, double b) { return abs_val(a-b) < epsilon; }


//   Macros      Macros      Macros      Macros      Macros

#define QQ()      (mQ.topLeftCorner(mN, mK))
#define RR()      (mR.triangularView<Eigen::Upper>.topLeftCorner(mK, mK))
#define RINV()    (mR.topLeftCorner(mK,mK).inverse())                               // should use triangular: mR.topLeftCorner(mK,mK).triangularView<Eigen::Upper>().inverse()

//   Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    Initialize

void
LinearRegression::allocate_memory()
{
  mResiduals = Vector(mN);
  mQ         = Matrix(mN, numberOfAllocatedColumns);
  mR         = Matrix(numberOfAllocatedColumns,numberOfAllocatedColumns);
  mShrinkage = Vector(numberOfAllocatedColumns);
  mGamma     = Vector(numberOfAllocatedColumns);
}


void
LinearRegression::add_constant()
{
  mK = 1;
  mXNames.push_back("Intercept");
  if (is_ols())
  { mR(0,0) = sqrt(mN);
    mQ.col(0).setConstant(1.0/sqrt(mN));
  }
  else
  { mQ.col(0) = mSqrtWeights;
    mR(0,0)   = sqrt(mSqrtWeights.squaredNorm());
    mQ.col(0)/= mR(0,0);
  }
  mGamma[0]     = mQ.col(0).dot(mY);
  mShrinkage[0] = 0.0;
  mResiduals    = mY.array() -  mGamma(0)/mR(0,0);
  mTotalSS = mResidualSS = mResiduals.squaredNorm();
}


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


std::vector<std::string>
LinearRegression::name_vec(std::string name) const
{
  std::vector<std::string> vec(1);
  vec[0] = name;
  return vec;
}

LinearRegression::Vector
LinearRegression::raw_residuals()          const
{
  if (is_ols())
    return mResiduals;
  else
    return mResiduals.array()/mSqrtWeights.array();
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
  std::cout << "TEST: solving, R matrix " << std::endl << mR.topLeftCorner(mK,mK) << std::endl << "       gamma = " << mGamma.head(mK).transpose() << std::endl;
  return mR.topLeftCorner(mK,mK).triangularView<Eigen::Upper>().solve(mGamma.head(mK));
}


LinearRegression::Vector
LinearRegression::se_beta_ols() const
{
  Matrix Ri (RINV());
  Vector diag (mK);
  for (int i=0; i<mK; ++i)
    diag(i) = Ri.row(i).squaredNorm();
  return rmse() * diag.array().sqrt();
}

LinearRegression::Vector
LinearRegression::se_beta() const
{
  if (mBlockSize==0)
    return se_beta_ols();
  else // compute sandwich estimates        // Note: these will be larger than entry F since residuals are updated once added
  { Vector se2 (mK);
    if (mBlockSize==1)
    { Matrix Ri (RINV());
      Matrix EQRi (mResiduals.asDiagonal() * mQ * Ri.transpose());
      for(int j=0; j < mK; ++j)
	se2[j] = EQRi.col(j).squaredNorm();
    }
    else // larger blocks, just diagonal
    { Matrix Ri (RINV());
      Matrix QRi (mQ * Ri.transpose());
      se2.setZero();
      for(int row = 0; row <mN; row+=mBlockSize)
      {	Vector eQRi = mResiduals.segment(row,mBlockSize).transpose() * QRi.block(row,0,mBlockSize,mK);
	se2.array() += eQRi.array() * eQRi.array();
      }
    }
    return se2.array().sqrt();
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
    return (x * b.tail(x.cols())).array() + b(0);    // internal X has leading const column; input X lacks constant
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

double
LinearRegression::sweep_Q_from_column(int col) const
{
  Vector delta  (QQ().transpose() * mQ.col(col));
  std::cout << "TEST: sweeping Q from col " << col << " gives " << delta.transpose() << std::endl;
  mQ.col(col) = mQ.col(col) - QQ() * delta;
  double ssz  (mQ.col(col).squaredNorm());
  if (is_invalid_ss (ssz)) return 0.0;
  mQ.col(col) /= sqrt(ssz);
  mR.col(col).head(col) = delta;
  mR(col,col) = sqrt(ssz);
  std::cout << "TEST: R matrix after sweeping Q is " << std::endl << mR.topLeftCorner(col+1,col+1) << std::endl;
  std::cout << "TEST: Q matrix is " << std::endl << mQ.topLeftCorner(5,col+1) << " ...." << std::endl;
  return ssz;
}
  

FStatistic
LinearRegression::f_test_predictor (std::string xName, Vector const& z) const
{
  mTempNames = name_vec(xName);
  mTempK     = 1;
  if (is_wls())
    mQ.col(mK) = z.array() * mSqrtWeights.array();
  else
    mQ.col(mK) = z;
  if (0.0 == sweep_Q_from_column(mK))
    return FStatistic();
  int residualDF (mN-2-q());
  assert(residualDF > 0);
  double ze  (mQ.col(mK).dot(mResiduals));           // slope of added var is  gamma (e'z)/(z'z = 1)
  if (mBlockSize==0)
  { double regrss (ze * ze);
    return FStatistic(regrss, 1, mResidualSS-regrss, residualDF, Vector::Ones(1));
  }
  else                                              
  { if (is_binary() && (mBlockSize == 1))            // use Bennett and fake F stat from squaring bennett t stat
    { std::pair<double,double> test (bennett_evaluation());
      debugging::debug("REGR",2) << "Bennett evaluation returns t = " << test.first << " with p-value = " << test.second <<std::endl;
      return FStatistic(test.first*test.first, test.second, 1, mN-q(), Vector::Ones(1));
    }
    else                                             // compute white estimate; in scalar case, reduces to (z'e)^2/(z'(e^2)z)
    { double zeez (0.0);
      if (mBlockSize == 1)
      { Vector temp (mQ.col(mK).array() * mResiduals.array());
	zeez = temp.squaredNorm();
      }
      else
      { assert (0 == mN % mBlockSize);
	for(int row=0; row<mN; row +=mBlockSize)
	{ double ezi (mResiduals.segment(row,mBlockSize).dot(mQ.col(mK).segment(row,mBlockSize)));
	  zeez += ezi * ezi;
	}
      }
      debugging::debug("REGR",4) << "F-stat components ze = " << ze << "  zeez = " << zeez << std::endl;
      return FStatistic(ze*ze/zeez, 1, residualDF, Vector::Ones(1));
    }
  }
}


FStatistic
LinearRegression::f_test_predictors (std::vector<std::string> const& xNames, Matrix const& z) const
{
  mTempNames = xNames;
  mTempK     = z.cols();
  if (is_wls())
    mQ.block(0,mK,mN,z.cols()) =  mSqrtWeights.asDiagonal() * z;
  else
    mQ.block(0,mK,mN,z.cols()) = z;
  for(int k = 0; k<z.cols(); ++k)
    if(0.0 == sweep_Q_from_column(mK+k))
      return FStatistic();
  int residualDF (mN-1-q()-z.cols());
  assert(residualDF > 0);
  Vector Qe  (mQ.topLeftCorner(mN,mK+z.cols()).transpose() * mResiduals);                // projection into space of new variables
  if (mBlockSize==0)
  { double regrss (Qe.squaredNorm());
    debugging::debug("REGR",3) << "F-stat components (" << regrss << "/" << mTempK << ")/(" << mResidualSS-regrss << "/" << residualDF << std::endl;
    return FStatistic(regrss, mTempK, mResidualSS-regrss, residualDF, Vector::Ones(z.cols()));
  }
  else
  { Matrix QeeQ(mTempK,mTempK);                                    // Q'ee'Q 
    QeeQ.setZero();
    if (mBlockSize == 1)
    { Matrix eQ (mResiduals.asDiagonal() * QQ());  
      QeeQ = eQ.transpose() * eQ;
    }
    else     // blocksize > 1
    { assert(0 == mN % mBlockSize);
      Vector eQ(mTempK);
      for(int block = 0, row = 0; block<mN/mBlockSize; ++block)
      {	eQ = mResiduals.segment(row,mBlockSize).transpose() * QQ().block(row,0,mBlockSize,mTempK);
	QeeQ += eQ * eQ.transpose();
	row += mBlockSize;
      }
    }
    double ss = (Qe.transpose() * QeeQ.inverse() * Qe)(0,0);
    debugging::debug("REGR",3) << "F-stat = " << ss << "/" << mTempK << " with " << residualDF << " residual DF." << std::endl;
    return FStatistic(ss/mTempK, mTempK, residualDF, Vector::Ones(z.cols()));
  }
}

//    Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     

std::pair<double,double>
LinearRegression::bennett_evaluation () const
{
  const double epsilon (1.0E-10);
  Vector mu      (fitted_values(epsilon, 1.0-epsilon));                      // think of fit as E(Y), constrained to [eps,1-eps] interval
  Vector var     (Vector::Ones(mN));  var = mu.array() * (var - mu).array();
  Vector dev     (mY - mu);                                                  // would match residuals IF other fit is bounded
  double num     (dev.dot(mQ.col(mK)));                                      // z'(y-y^)
  double rootZDZ (sqrt (var.dot(mQ.col(mK).cwiseProduct(mQ.col(mK)))));      // sqrt(z'Dz)
  double maxA    (0.0);
  for (int i=0; i<mN; ++i)
  { double absZ (abs_val(mQ(i,mK) * max_abs(mu[i], 1.0-mu[i])));             // largest possible error for this case
    if (absZ > maxA) maxA = absZ;                                            // largest?
  }
  double Mz      (maxA/rootZDZ);
  double tz      (abs_val(num)/rootZDZ);                    // num = get(mZE,0)
  return std::make_pair(tz, bennett_p_value(tz,Mz));
}


//     Add predictor     Add predictor     Add predictor     Add predictor     Add predictor     Add predictor     Add predictor

void
LinearRegression::update_fit(StringVec xNames)
{
  std::cout << "TEST: updating fit, first of " << xNames.size() << " is " << xNames[0] << "\n";
  for(unsigned int j=0; j<xNames.size(); ++j)
    mXNames.push_back(xNames[j]);
  mK += (int) xNames.size();
  mGamma.head(mK)  = (QQ().transpose() * mY).cwiseQuotient(mShrinkage.head(mK));    // RAS  Need to only change the last updated columns, not all
  mResiduals = mY - QQ() * mGamma.head(mK);
  std::cout << "TEST: after update, gamma = " << mGamma.head(mK).transpose() << " with shrinkage " << mShrinkage.head(mK).transpose() << std::endl;
}


void
LinearRegression::add_predictors (StringVec const& xNames, Matrix const& x)
{
  assert(x.rows() == mN);
  assert((int)xNames.size() == x.cols());
  if (is_wls())
    mQ.block(0,mK,mN,x.cols()) =  mSqrtWeights.asDiagonal() * x;
  else
    mQ.block(0,mK,mN,x.cols()) = x;
  for(int k = 0; k<x.cols(); ++k)
  { if(0.0 == sweep_Q_from_column(mK+k))
    { std::cout << "REGR: Column " << k << " of added predictors is collinear." << std::endl;
      return;
    }
  }
  update_fit(xNames);
}

  
void
LinearRegression::add_predictors  (FStatistic const& fstat)
{
  debugging::debug("LINR",3) << "Adding " << mTempK << " previously tested predictors; entry stat for added predictors is " << fstat << std::endl;
  double shrinkage (1.0);
  double F (fstat.f_stat());
  if (F > 0) // skip those with F == 0
  { if (F > 1)
      shrinkage /= (F - 1);
    else
    { std::cout << "REGR: Warning... Cannot shrink estimates as desired because F-stat (" << fstat.f_stat() << ") is too small." << std::endl;
      shrinkage /= F;
    }
  }
  for (unsigned int j=0; j<mTempNames.size(); ++j)
    mShrinkage[mK+j] = shrinkage;
  assert( mTempK == (int)mTempNames.size() );
  update_fit(mTempNames);
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
    os << "Linear Regression";
  else
    os << "Weighted Linear Regression";
  if (is_binary())
    os << "   Binary response";
  os << "  y = " << mYName << "    (n=" << mN << ",k=" << mK << ") " << std::endl
     << "            Total SS    = " << mTotalSS    << "     R^2 = " << r_squared() << std::endl
     << "            Residual SS = " << mResidualSS << "    RMSE = " << rmse() << std::endl << std::endl;
  Vector b   (beta());
  Vector se  (se_beta());
  os.precision(3);
  if (mBlockSize == 0)
  { os << "                        Variable Name                   Beta      OLS SE       t       Shrinkage " << std::endl;
    for (int j = 0; j<mK; ++j)
      os << std::setw(maxNameLen) << printed_name(mXNames[j])  << "  " << std::setw(9) << b[j] << "  " << std::setw(9) << se[j] << "  "
	 << std::setw(8) << b[j]/se[j] << std::setw(8) << mShrinkage[j] << std::endl;
  }
  else // show ols and sandwich se
  { Vector olsSE (se_beta_ols());
    os << "                  Variable Name                          Estimate      Sandwich SE     (OLS)        t       Shrinkage " << std::endl;
    for (int j = 0; j<mK; ++j)
      os << std::setw(maxNameLen) << printed_name(mXNames[j])  << "  " << std::setw(12) << b[j] << "  " << std::setw(12) << se[j]
	 << "(" << std::setw(12) << olsSE[j] << ")  " << std::setw(8) << b[j]/se[j] << "  " << std::setw(8) << mShrinkage[j] << std::endl;
  }
}


void
LinearRegression::write_data_to (std::ostream& os) const
{
  // prefix line with var names
  os << "Role\tFit\tResidual\t" << mYName;
  for(int j=1; j<mK; ++j)
    os << "\t" << mXNames[j];
  os << std::endl;
  // now put the data in external coordinate system
  Vector y    (is_ols() ? mY : mY.cwiseQuotient(mSqrtWeights));
  Vector res  (raw_residuals());
  Vector fit  (is_ols() ? fitted_values() : y - res);
  for(int i=0; i<mN; ++i)
  { os << "est\t" << fit[i] << "\t" << res[i] << "\t" << y[i] << "\t";
    Vector row (mQ.row(i).head(mK));
    if (is_wls())
      row = row / mSqrtWeights[i];
    for (int j=1; j<mK-1; ++j)  // skip intercept
      os << row[j] << "\t";
    os << row[mK-1] << std::endl;
  }
}


//     ValidatedRegression      ValidatedRegression      ValidatedRegression      ValidatedRegression      ValidatedRegression      ValidatedRegression 


double
ValidatedRegression::validation_ss() const
{
  double ss (0.0);
  if (n_validation_cases()>0)
  { if (q()==0)     // handle differently for initial case to avoid empty matrix
    { double mean (mModel.y_bar());
      ss =  mValidationY.unaryExpr([mean](double x)->double { return x-mean; }).squaredNorm();
    }
    else
      ss = (mValidationY - mModel.predictions(mValidationX)).squaredNorm();
  }
  return ss;
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
