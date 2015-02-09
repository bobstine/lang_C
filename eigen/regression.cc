/*

  For explanation of shrinkage, see shrinkage.tex in work/papers/notes
  
*/
#include "debug.h"

using debugging::debug;

#pragma GCC optimize ("-O4")

#include "regression.h"
#include "bennett.h"
#include "eigen_iterator.h"
#include "confusion_matrix.h"

#include <thread>

#include <Eigen/LU>
#include <Eigen/QR>

// #include "boost/shared_ptr.hpp"

#include <iomanip>
#include <random>

const unsigned int maxNameLen (50);                                                 // max length shown when print model
const unsigned int numberOfAllocatedColumns(3001);    

double abs_val(double x) { if (x < 0.0) return -x; else return x; }
double max_abs(double x, double y) { double ax = abs_val(x); double ay = abs_val(y); if (ax >= ay) return ax; else return ay; }
int    min_int(int i, int j) { if(i<j) return i; else return j; }
bool   close (double a, double b) { return abs_val(a-b) < 1.0e-50; }


//   Macros      Macros      Macros      Macros      Macros

#define QQ()      (mQ.leftCols(mK))
#define RR()      (mR.topLeftCorner(mK,mK).triangularView<Eigen::Upper>())
#define RINV()    (mR.topLeftCorner(mK,mK).inverse())                               // should use triangular: mR.topLeftCorner(mK,mK).triangularView<Eigen::Upper>().inverse()


//    Utils      Utils      Utils      Utils      Utils      Utils      Utils      Utils

double
LinearRegression::approximate_ss(Vector const& x) const
{
  double sum (x[0]);
  double ss   (0.0);

  for (int i=1; i<x.size(); ++i)
  { sum += x[i];
    double dev = x[i]-(sum/i);
    ss += dev*dev;
  }
  return ss;
}


bool
LinearRegression::is_binary_vector(Vector const& y)  const
{
  for(int i=0; i<y.size(); ++i)
    if( (y[i] != 0) && (y[i] != 1) )
    { debug("REGR",4) << "Response vector in regression is not binary; found value v[" << i << "] = " << y[i] << std::endl;
      return false;
    }
  debug("REGR",4) << "Response vector in regression is binary." << std::endl;
  return true;
}


std::vector<std::string>
LinearRegression::name_vec(std::string name) const
{
  std::vector<std::string> vec(1);
  vec[0] = name;
  return vec;
}


LinearRegression::Matrix
LinearRegression::check_orthogonality_matrix () const
{
  Eigen::HouseholderQR<Matrix> qr(QQ());
  return qr.matrixQR().topRows(mK).triangularView<Eigen::Upper>();
}
    

//   Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    Initialize    Initialize

void
LinearRegression::allocate_memory()
{
  mResiduals = Vector(mN);
  mQ         = Matrix(mN, numberOfAllocatedColumns);
  mR         = Matrix(numberOfAllocatedColumns,numberOfAllocatedColumns);
  mLambda    = Vector(numberOfAllocatedColumns);
  mGamma     = Vector(numberOfAllocatedColumns);
}


void
LinearRegression::add_constant()
{
  mK = 1;
  mXNames.push_back("Intercept");
  mQ.col(0)    = mSqrtWeights;
  mR(0,0)      = sqrt(mSqrtWeights.squaredNorm());
  mQ.col(0)   /= mR(0,0);
  mGamma[0]    = mQ.col(0).dot(mY);
  mLambda[0]   = 0.0;                               // dont shrink intercept
  mResiduals   = mY -  mGamma[0] * mQ.col(0);
  mTotalSS = mResidualSS = mResiduals.squaredNorm();
}


//     Accessors      Accessors      Accessors      Accessors      Accessors      Accessors      


LinearRegression::Vector
LinearRegression::x_row (int i)            const
{
  return mQ.row(i).head(mK) * RR() / mSqrtWeights[i];
}


LinearRegression::Vector
LinearRegression::raw_fitted_values(double lo, double hi) const
{
  Vector fit = raw_fitted_values();
  return fit.unaryExpr([lo,hi] (double x) -> double { if(x < lo) return lo; if(x < hi) return x; return hi; });
}


//     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes


LinearRegression::Vector
LinearRegression::se_gamma_ls() const
{
  double s2 (mResidualSS/(mN-mK));
  Vector se (mLambda.unaryExpr([s2] (double lam)->double { return sqrt(s2/(1+lam)); }));
  return se;
}


LinearRegression::Vector
LinearRegression::se_gamma() const
{
  if (mBlockSize==0)
    return se_gamma_ls();
  else // compute sandwich estimates; differ from entry F since residuals are updated once added
  { Vector se2 (mK);
    if (mBlockSize==1)
    { Matrix EQ (mResiduals.asDiagonal() * QQ());
      for(int j=0; j < mK; ++j)
	se2[j] = EQ.col(j).squaredNorm();
    }
    else // larger blocks, just diagonal
    { se2.setZero();
      for(int row = 0; row <mN; row+=mBlockSize)
      {	Vector eQ = mResiduals.segment(row,mBlockSize).transpose() * mQ.block(row,0,mBlockSize,mK);
	se2.array() += eQ.array() * eQ.array();
      }
    }
    return se2.array().sqrt();
  }
}



LinearRegression::Vector
LinearRegression::beta() const
{
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
      Matrix EQRi (mResiduals.asDiagonal() * QQ() * Ri.transpose());
      for(int j=0; j < mK; ++j)
	se2[j] = EQRi.col(j).squaredNorm();
    }
    else // larger blocks, just diagonal
    { Matrix Ri (RINV());
      Matrix QRi (QQ() * Ri.transpose());
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

LinearRegression::Vector
LinearRegression::predictions(Matrix const& x, double lo, double hi) const
{
  Vector preds (predictions(x));
  for(int i=0; i<preds.size(); ++i)
  { if      (preds[i] < lo) preds[i] = lo;
    else if (preds[i] > hi) preds[i] = hi;
  }
  return preds;  
}


//     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests

bool
LinearRegression::is_invalid_ss (double ss, double ssz)          const
{
  const    double    epsilon (1.0e-5);
  debugging::debug("REGR",4) << "Initial SSz = " << ss << " becomes " << ssz << " after sweeping." << std::endl; 
  if (ssz <= 0.0)
  { debugging::debug("REGR",1) << " *** Error: SSz <= 0.0 in regression." << std::endl;
    return true;
  }
  if(std::isnan(ssz))
  { debugging::debug("REGR",1) << " *** Error: SSz = NaN in regression." << std::endl;
    return true;
  }
  if(std::isinf(ssz))
  { debugging::debug("REGR",1) << " *** Error: SSz = Inf in regression." << std::endl;
    return true;
  }
  if(ssz/ss < epsilon)
  { debugging::debug("REGR",1) << "SSZ indicates near singular;  SSZ = " << ssz << std::endl;
    return true;
  }
  return false;
}

double
LinearRegression::sweep_Q_from_column(int col) const
{
  // all at once, classical GS
  double ss (approximate_ss(mQ.col(col)));
  #ifdef _CLASSICAL_GS_
  Vector delta  (mQ.leftCols(col).transpose() * mQ.col(col));
  debugging::debug("REGR",4) << "Classical GS; sweeping Q from col " << col << "; delta=" << delta.transpose() << std::endl;
  mQ.col(col) = mQ.col(col) - mQ.leftCols(col) * delta;
  mR.col(col).head(col) = delta;
  #else
  //residual from each first, modified GS
  for(int j=0; j<col; ++j)
  { mR(j,col) = mQ.col(j).dot(mQ.col(col));
    mQ.col(col) -= mQ.col(j) * mR(j,col);
  }
  #endif
  double ssz  (mQ.col(col).squaredNorm());
  if (is_invalid_ss (ss, ssz)) return 0.0;
  mQ.col(col) /= sqrt(ssz);
  mR(col,col) = sqrt(ssz);
  // std::cout << "TEST: R matrix after sweeping Q is " << std::endl << mR.topLeftCorner(col+1,col+1) << std::endl;
  // std::cout << "TEST: Q matrix is " << std::endl << mQ.topLeftCorner(5,col+1) << " ...." << std::endl;
  return ssz;
}
  

FStatistic
LinearRegression::f_test_predictor (std::string xName, Vector const& z) const
{
  mTempNames = name_vec(xName);
  mTempK     = 1;
  mQ.col(mK) = z.array() * mSqrtWeights.array();
  if (0.0 == sweep_Q_from_column(mK))
  { std::cout << "REGR: Singularity detected. SSz = 0 for predictor " << xName << "; returning empty F stat" << std::endl;
    return FStatistic();
  }
  int residualDF (mN-mK);
  assert(residualDF > 0);
  double qe  (mQ.col(mK).dot(mResiduals));           // slope of added var is  gamma (e'z)/(z'z = 1)
  if (mBlockSize==0)
  { double regrss (qe * qe);
    return FStatistic(regrss, 1, mResidualSS-regrss, residualDF, Vector::Ones(1));
  }
  else                                              
  { if (is_binary() && (mBlockSize == 1))            // use Bennett and fake F stat from squaring bennett t stat
    { std::pair<double,double> test (bennett_evaluation());
      debugging::debug("REGR",2) << "Bennett evaluation returns t = " << test.first << " with p-value = " << test.second <<std::endl;
      return FStatistic(test.first*test.first, test.second, 1, mN-q(), Vector::Ones(1));
    }
    else                                             // compute white estimate; in scalar case, reduces to (z'e)^2/(z'(e^2)z)
    { double qeeq (0.0);
      if (mBlockSize == 1)
      { Vector temp (mQ.col(mK).array() * mResiduals.array());
	qeeq = temp.squaredNorm();
      }
      else
      { assert (0 == mN % mBlockSize);
	for(int row=0; row<mN; row +=mBlockSize)
	{ double ezi (mResiduals.segment(row,mBlockSize).dot(mQ.col(mK).segment(row,mBlockSize)));
	  qeeq += ezi * ezi;
	}
      }
      debugging::debug("REGR",4) << "F-stat components qe = " << qe << "  qeeq = " << qeeq << std::endl;
      return FStatistic(qe*qe/qeeq, 1, residualDF, Vector::Ones(1));
    }
  }
}


FStatistic
LinearRegression::f_test_predictors (std::vector<std::string> const& xNames, Matrix const& z) const
{
  mTempNames = xNames;
  mTempK     = (int) z.cols();
  mQ.block(0,mK,mN,z.cols()) =  mSqrtWeights.asDiagonal() * z;
  for(int k = 0; k<z.cols(); ++k)
    if(0.0 == sweep_Q_from_column(mK+k))
      return FStatistic();
  int residualDF (mN-mK-(int)z.cols());
  assert(residualDF > 0);
  Vector Qe  (mQ.block(0,mK,mN,mTempK).transpose() * mResiduals);    // new gamma coefs
  if (mBlockSize==0)
  { double regrss (Qe.squaredNorm());
    debugging::debug("REGR",3) << "F-stat components (" << regrss << "/" << mTempK << ")/(" << mResidualSS-regrss << "/" << residualDF << ")" << std::endl;
    return FStatistic(regrss, mTempK, mResidualSS-regrss, residualDF, Vector::Ones(z.cols()));
  }
  else
  { Matrix QeeQ(mTempK,mTempK);                                    // Q'ee'Q 
    QeeQ.setZero();
    if (mBlockSize == 1)
    { Matrix eQ (mResiduals.asDiagonal() * mQ.block(0,mK,mN,mTempK));  
      QeeQ = eQ.transpose() * eQ;
    }
    else     // blocksize > 1
    { assert(0 == mN % mBlockSize);
      Vector eQ(mTempK);
      for(int block = 0, row = 0; block<mN/mBlockSize; ++block)
      {	eQ = mResiduals.segment(row,mBlockSize).transpose() * mQ.block(row,mK,mBlockSize,mTempK);
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

// RAS: pretty sure this is wrong when the response gets weighted

std::pair<double,double>
LinearRegression::bennett_evaluation () const
{
  const double epsilon (1.0E-10);
  Vector mu      (raw_fitted_values(epsilon, 1.0-epsilon));                  // think of fit as E(Y), constrained to [eps,1-eps] interval
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
  debugging::debug("REGR",3) << "Updating fit, first of " << xNames.size() << " is " << xNames[0] << "\n";
  assert (mTempK == (int)xNames.size());
  for(unsigned int j=0; j<xNames.size(); ++j)
  { mXNames.push_back(xNames[j]);
    mGamma[mK+j]  = (mQ.col(mK+j).dot(mY)/(1+mLambda[mK+j]));
  }
  mK += mTempK;
  mResiduals = mY - QQ() * mGamma.head(mK);   // Faster?  just mod the residuals rather than recalc
  mResidualSS= mResiduals.squaredNorm();      // Faster?  subtract new gamma'gamma
}


void
LinearRegression::add_predictors (StringVec const& zNames, Matrix const& z)
{
  assert(z.rows() == mN);
  assert((int)zNames.size() == z.cols());
  mTempNames = zNames;
  mTempK     = (int) z.cols();
  mQ.block(0,mK,mN,z.cols()) =  mSqrtWeights.asDiagonal() * z;
  for(int k = 0; k<z.cols(); ++k)
    if(0.0 == sweep_Q_from_column(mK+k))
    { std::cerr << "REGR: *** Error *** Column " << k << " of forced-to-add predictors is collinear." << std::endl;
      return;
    }
  int residualDF (mN-mK-(int)z.cols());
  assert(residualDF > 0);
  add_predictors();
}

  
void
LinearRegression::add_predictors  (FStatistic const& fstat)
{
  debugging::debug("REGR",3) << "Adding " << mTempK << " previously tested predictors; entry stat for added predictors is " << fstat << std::endl;
  double lambda (0);
  double F (fstat.f_stat());
  if (F > 0) // dont shrink those with F == 0
  { if (F > 1)
      lambda = 1/(F - 1);
    else
    { std::cout << "REGR: Warning... Cannot shrink estimates as desired because F-stat (" << fstat.f_stat() << ") is too small." << std::endl;
      lambda = 1/F;
    }
  }
  for (unsigned int j=0; j<mTempNames.size(); ++j)
    mLambda[mK+j] = lambda;
  update_fit(mTempNames);
}

  
void
LinearRegression::add_predictors  ()   // no shrinkage
{
  debugging::debug("REGR",3) << "Adding " << mTempK << " previously tested predictors; forced addition." << std::endl;
  double lambda (0);
  for (unsigned int j=0; j<mTempNames.size(); ++j)
    mLambda[mK+j] = lambda;
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
  os << mWeightStr << "Linear Regression";
  if (is_binary())
    os << "   Binary response";
  os << "  y = " << mYName << "    (n=" << mN << ",k=" << mK << ") " << std::endl
     << "            Total SS    = " << mTotalSS    << "     R^2 = " << r_squared() << std::endl
     << "            Residual SS = " << mResidualSS << "    RMSE = " << rmse() << std::endl << std::endl;
  print_gamma_to(os);
}



void
LinearRegression::print_gamma_to (std::ostream&os) const
{
  Vector se (se_gamma());
  if (mBlockSize == 0)
  { os << "                        Variable Name                   Gamma       OLS SE      t     Lambda" << std::endl;
    for (int j = 0; j<mK; ++j)
      os << std::setw(maxNameLen) << printed_name(mXNames[j])  << "    " << std::setw(9) << mGamma[j] << "  " << std::setw(8) << se[j] << "  "
	 << std::setw(8) << mGamma[j]/se[j] << "   " << mLambda[j] << std::endl;
  }
  else // show ols and sandwich
  { Vector olsSE (se_gamma_ls());
    os << "                        Variable Name                   Gamma    Sandwich SE     (OLS)        t     Lambda " << std::endl;
    for (int j = 0; j<mK; ++j)
      os << std::setw(maxNameLen) << printed_name(mXNames[j])  << "    " << std::setw(9) << mGamma[j] << "  " << std::setw(8) << se[j]
	 << "(" << std::setw(12) << olsSE[j] << ")  "
	 << std::setw(8) << mGamma[j]/se[j] << "   " << mLambda[j] << std::endl;
  }
}



void
LinearRegression::print_beta_to (std::ostream&os) const
{
  Vector b   (beta());
  Vector se  (se_beta());
  os.precision(4);
  if (mBlockSize == 0)
  { os << "                        Variable Name                   Beta      OLS SE       t  " << std::endl;
    for (int j = 0; j<mK; ++j)
      os << std::setw(maxNameLen) << printed_name(mXNames[j])  << "  " << std::setw(9) << b[j] << "  " << std::setw(9) << se[j] << "  "
	 << std::setw(8) << b[j]/se[j] << std::endl;
  }
  else // show ols and sandwich se
  { Vector olsSE (se_beta_ols());
    os << "                  Variable Name                          Beta      Sandwich SE     (OLS)        t  " << std::endl;
    for (int j = 0; j<mK; ++j)
      os << std::setw(maxNameLen) << printed_name(mXNames[j])  << "  " << std::setw(12) << b[j] << "  " << std::setw(12) << se[j]
	 << "(" << std::setw(12) << olsSE[j] << ")  " << std::setw(8) << b[j]/se[j] << std::endl;
  }
}


void
LinearRegression::write_data_to (std::ostream& os, int maxNumXCols) const
{
  // number of columns of predictors
  int numX = min_int(mK-1,maxNumXCols);
  // prefix line with var names; intercept is name[0]
  os << "Role\tFit\tResidual\t" << mYName;
  // skip the intercept in column 0
  for(int j=1; j<=numX; ++j)  
    os << "\t" << mXNames[j];
  os << std::endl;
  // put the data in external coordinate system
  Vector y    (raw_y());
  Vector res  (raw_residuals());
  Vector fit  (y - res);
  for(int i=0; i<mN; ++i)
  { os << "est\t" << fit[i] << "\t" << res[i] << "\t" << y[i] ;
    if(numX>0)
    { Vector row (x_row(i));
      for (int j=1; j<=numX; ++j)  // skip intercept
	os << '\t' << row[j];
    }
    os << std::endl;
  }
}

//     ValidatedRegression      ValidatedRegression      ValidatedRegression      ValidatedRegression      ValidatedRegression      ValidatedRegression 

void
ValidatedRegression::initialize_validation_ss()
{ 
  double mean (mModel.y_bar());
  mValidationSS = mValidationY.unaryExpr([mean](double x)->double { return x-mean; }).squaredNorm();
}

//     confusion_matrix     confusion_matrix     confusion_matrix
  
ConfusionMatrix
ValidatedRegression::estimation_confusion_matrix(float threshold) const
{
  assert (mModel.is_binary());
  Vector y = mModel.raw_y();
  Vector fit = mModel.fitted_values();
  return ConfusionMatrix(y.size(), EigenVectorIterator(&y), EigenVectorIterator(&fit), threshold);
}

ConfusionMatrix
ValidatedRegression::validation_confusion_matrix(float threshold) const
{
  assert (mModel.is_binary());
  assert (n_validation_cases() > 0);
  Vector pred = mModel.predictions(mValidationX);
  return ConfusionMatrix(mValidationY.size(), EigenVectorIterator(&mValidationY), EigenVectorIterator(&pred), threshold);
}


//     print_to     print_to     print_to     

void
ValidatedRegression::print_to(std::ostream& os, bool useHTML) const
{
  if(useHTML)
    os << "*** Note *** Code for ValidatedRegression does not implement HTML version of model summary" << std::endl;
  os.precision(6);
  os << "Validated Regression      n(est) = " << mN << "    n(validate) = " << n_validation_cases() << "    ";
  if(block_size() > 0)
    os << " with White SE(b=" << block_size() << ")";
  os << std::endl
     << "            Validation SS = " << validation_ss() << std::endl;
  if (mModel.is_binary())
  { os << "            Training   confusion matrix \n"
       << estimation_confusion_matrix()
       << std::endl;
    if (n_validation_cases())
    { os << "            Validation confusion matrix = \n"
	 << validation_confusion_matrix()
	 << std::endl;
    }
  }
  os << mModel;
}

void
ValidatedRegression::write_data_to(std::ostream& os, int maxNumXCols) const
{
  // Note: does not return the data to the original ordering
  mModel.write_data_to(os, maxNumXCols);
  Vector preds (mModel.predictions(mValidationX));
  for(int i=0; i<mValidationX.rows(); ++i)
  { os << "val\t" << preds[i] << '\t' << mValidationY[i]-preds[i] << '\t' << mValidationY[i];
    for (int j=0; j<min_int((int)mValidationX.cols(), maxNumXCols); ++j) 
      os << '\t' << mValidationX(i,j);
    os << std::endl;
  }
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//     cross validation     cross validation     cross validation     cross validation     cross validation
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


const int    noBlocking    = 0;     // no white blocking
const bool   noShrinkage   = false; // no shrinkage
const double singularPval  = 0.995; // threshold for deciding if added variable is 'too close' to singular


/*
  Regression worker fills a pre-allocated matrix with summary statistics from fitting a sequence
  of progressively larger regression models constructed -- in given order -- from the input X
  matrix.  The model is inialized with the columns in Xi (and included a constant).  The summary
  statistics are RSS, R2 and AIC.
*/

class RegressionWorker
{
private:
  Eigen::VectorXd const*   mY;
  Eigen::MatrixXd const*   mXi;
  Eigen::MatrixXd const*   mX;
  int                      mSkipped;
  Eigen::MatrixXd      *   mResults;
  bool                     mTrace;

public:    
  RegressionWorker(Eigen::VectorXd const* y, Eigen::MatrixXd const* Xi, Eigen::MatrixXd const* X, Eigen::MatrixXd *results, bool trace)
    :  mY(y), mXi(Xi), mX(X), mSkipped(0), mResults(results), mTrace(trace)
    {
      assert( (mY->size() == mXi->rows()) && (mY->size() == mX->rows()) );
      assert( (mResults->rows() == (1+mX->cols())) && (mResults->cols() >= 3));  // fills first 3 columns of results; add row for initial model
    }
    
  RegressionWorker(const RegressionWorker& rhs)
    : mY(rhs.mY), mXi(rhs.mXi), mX(rhs.mX), mSkipped(rhs.mSkipped), mResults(rhs.mResults), mTrace(rhs.mTrace) { }

  void operator()()
  {
    std::vector<std::string> xNames;
    for(int j=0; j<mXi->cols(); ++j) xNames.push_back("Xi_"+std::to_string(j));
    LinearRegression regr("Y", *mY, noBlocking);
    // check one at a time in case of singular model
    for (int k=0; k<mXi->cols(); ++k)
      add_predictor_if_useful(&regr, "Xi_"+std::to_string(k), mXi->col(k));
    fill_results(regr, 0);
    for (int k=0; k<mX ->cols(); ++k)
    { add_predictor_if_useful(&regr, "XX_" +std::to_string(k), mX ->col(k));
      fill_results(regr,k+1);
    }
  }

  int number_skipped() const { return mSkipped; }
  
private:

  void fill_results(LinearRegression const& regr, int k)
    { (*mResults)(k,0) = regr.r_squared();
      (*mResults)(k,1) = regr.residual_ss();
      (*mResults)(k,2) = regr.aic_c();
    }
  
  void add_predictor_if_useful(LinearRegression *regr, std::string name, Eigen::VectorXd const& x, bool verbose=false)
    { FStatistic f = regr->f_test_predictor(name, x);
      if(f.f_stat() != 0.0)
	regr->add_predictors();
      else
	++mSkipped;
      if (verbose) std::clog << "REGR: Trace of regression worker, q = " << regr->q() << " predictors (" << mSkipped << " skipped)" << std::endl
			    << "      beta = " << regr->beta().transpose() << std::endl
			    << "      r^2 = " << regr->r_squared() << "   RSS = " << regr->residual_ss() << std::endl;
    }
  
};


/*
  Validation worker is similar to a regression worker, but rather than compute summary
  statistics, it computes a CVSS based on the input iteratorr that tells it which cases
  to exclude from the analysis.  The results are placed in the external CVSS array.
*/
class ValidationWorker
{
private:
  Eigen::VectorXd const*             mY;
  Eigen::MatrixXd const*             mXi;
  Eigen::MatrixXd const*             mX;
  std::vector<bool>::const_iterator  mSelector;
  int                                mSkipped;
  Eigen::VectorXd                 *  mCVSS;
  const double                       mPtoEnter = 0.995;  // avoid singular variables

public:    
  ValidationWorker(Eigen::VectorXd const* y, Eigen::MatrixXd const* Xi, Eigen::MatrixXd const* X,
		   std::vector<bool>::const_iterator sel, Eigen::VectorXd *cvss)
    : mY(y), mXi(Xi), mX(X), mSelector(sel), mSkipped(0), mCVSS(cvss)
    { 
      assert( (mY->size() == mXi->rows()) && (mY->size() == mX->rows()) );
      assert( (1+mX->cols())== (int)mCVSS->size() );   // need room for intial model
    }
  
  ValidationWorker(const ValidationWorker& rhs)
    : mY(rhs.mY), mXi(rhs.mXi), mX(rhs.mX), mSelector(rhs.mSelector), mSkipped(rhs.mSkipped), mCVSS(rhs.mCVSS), mPtoEnter(rhs.mPtoEnter) { }
    
  void operator()()
  {
    ValidatedRegression regr("yy", EigenVectorIterator(mY), mSelector, (int)mY->size(), noBlocking, noShrinkage);
    std::vector< std::pair<std::string,EigenColumnIterator> > namedIter;
    namedIter.push_back( std::make_pair("vXi",EigenColumnIterator(mXi,-1)) );
    if(mXi->cols()>0)
    { for (int k=0; k<mXi->cols(); ++k)
      { namedIter[0].second = EigenColumnIterator(mXi, k);
	regr.add_predictors_if_useful(namedIter, singularPval);
      }
    }
    (*mCVSS)[0] = regr.validation_ss();
    namedIter[0].first = "vXX";
    for (int k=0; k<mX->cols(); ++k)
    { namedIter[0].second = EigenColumnIterator(mX, k);
      std::pair<double,double> fAndP = regr.add_predictors_if_useful(namedIter, mPtoEnter);
      if (fAndP.first == 0) ++mSkipped;  
      (*mCVSS)[k+1] = regr.validation_ss();           // offset by 1 to accomodate first model
    }
  }
  
  int number_skipped() const { return mSkipped; }

};


void
validate_regression(Eigen::VectorXd const& Y,
		    Eigen::MatrixXd const& Xi,      // preconditioning variables
		    Eigen::MatrixXd const& X,       // variables to add one-at-a-time
		    int nFolds,
		    Eigen::MatrixXd  &results,
		    unsigned randomSeed)
{
  if ((results.rows() != (1+X.cols())) || (results.cols()!=4) )
  { std::cerr << "REGR: Arguments to validate_regression misformed. dim(result)= (" << results.rows() << "," << results.cols()
	      << ") with input data X having " << X.cols() << " columns." << std::endl;
    return;
  }
  int totalSkipped = 0;  // used to block threads till finish
  // fit main model, filling first 3 columns of results with R2, RSS, AICc
  bool trace = (randomSeed == 0);
  RegressionWorker regrWorker(&Y, &Xi, &X, &results, trace);
  debug("REGR",2) << "Starting main regression thread" << std::endl;
  std::thread regrThread(regrWorker);
  // construct random folds
  if (nFolds > 0)
  { std::vector<int> folds (Y.size());
    for(int i=0; i<(int)folds.size(); ++i)
      folds[i] = i % nFolds;
    if (randomSeed != 0)
      shuffle(folds.begin(), folds.end(), std::default_random_engine(randomSeed));
    // write data to check code
    if (false)
    { Eigen::MatrixXd data(X.rows(),2+Xi.cols()+X.cols());
      Eigen::VectorXd dFold(folds.size());
      for(int i=0; i<(int)folds.size(); ++i) dFold[i] = folds[i];
      data << dFold, Y , Xi , X; 
      std::cout << "TEST:  Writing data in external order as created, first four rows are\n" << data.topRows(4) << std::endl;
      std::string fileName ("regr_test_data.txt");
      std::ofstream output(fileName.c_str());
      output << "folds \t y";
      for(int i=0; i<Xi.cols(); ++i) output << "\tXi_" << i;
      for(int i=0; i<X.cols(); ++i) output << "\tX_" << i;
      output.precision(7);
      output << std::endl << data << std::endl;
    }
    // build validated regressions (construct each worker then launch thread)
    debug("REGR",2) << "Building validation threads" << std::endl;
    std::vector<std::vector<bool>>  selectors (nFolds);
    std::vector<Eigen::VectorXd *>  cvss      (nFolds);
    std::vector<ValidationWorker *> workers   (nFolds);
    std::vector<std::thread>        validationThreads(nFolds);
    for (int fold=0; fold<nFolds; ++fold)
    { for(int i=0; i<(int)folds.size(); ++i)
	selectors[fold].push_back( folds[i] != fold );
      cvss[fold]    = new Eigen::VectorXd(1+X.cols());   // +1 for initial model results
      workers[fold] = new ValidationWorker(&Y, &Xi, &X, selectors[fold].begin(), cvss[fold]);
      debug("REGR",2) << "Starting validation thread #" << fold << std::endl;
      validationThreads[fold] = std::thread(*workers[fold]);
    }
    for (int fold=0; fold<nFolds; ++fold)
    { validationThreads[fold].join();
      totalSkipped += workers[fold]->number_skipped();
    }
    debug("REGR",3) << "Total number of terms skipped during cross-validation was " << totalSkipped << std::endl;
    //  accumulate CVSS vectors over folds
    for(int fold=1; fold<nFolds; ++fold)
      (*cvss[0]) += *cvss[fold];
    // join initial thread that writes into results before adding cvss to results
    regrThread.join();
    results.col(3) = *cvss[0];
    // free space
    for(int fold=0; fold<nFolds; ++fold)
    { delete cvss[fold];
      delete workers[fold];
    }
  }
}
  
