// -*- c++; c-file-style: "bobstyle" -*-


//  For explanation of shrinkage, see shrinkage.tex in work/papers/notes

#include "debug.h"
using debugging::debug;

#include "linear_regression.h"
#include "little_functions.h"
#include "bennett.h"

#include <iomanip>
#include <Eigen/LU>
#include <Eigen/QR>

const unsigned int maxNameLen (50);                                                 // max length shown when print model
const unsigned int numberOfAllocatedColumns(5001);    


//   Macros      Macros      Macros      Macros      Macros

#define QQ()      (mQ.topLeftCorner(mN,mK))
#define QQtest()  (mQ.bottomLeftCorner(mTest,mK))
#define RR()      (mR.topLeftCorner(mK,mK).triangularView<Eigen::Upper>())
#define RINV()    (mR.topLeftCorner(mK,mK).inverse())          // should use triangular: mR.topLeftCorner(mK,mK).triangularView<Eigen::Upper>().inverse()


//    Utils      Utils      Utils      Utils      Utils      Utils      Utils      Utils

LinearRegression::Scalar
LinearRegression::approximate_ss(Vector const& x) const
{
  Scalar sum (x[0]);
  Scalar ss   (0.0);

  for (int i=1; i<mN; ++i)   // only for est cases
  { sum += x[i];
    LinearRegression::Scalar dev = x[i]-(sum/(Scalar)i);
    ss += dev*dev;
  }
  return ss;
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
  mFitted    = Vector(mN+mTest);
  mQ         = Matrix(mN+mTest, numberOfAllocatedColumns);
  mR         = Matrix(numberOfAllocatedColumns,numberOfAllocatedColumns);
  mLambda    = Vector(numberOfAllocatedColumns);
  mGamma     = Vector(numberOfAllocatedColumns);
}


void
LinearRegression::add_constant()
{
  mK = 1;
  mXNames.push_back("Intercept");
  mQ.col(0).head(mN)    = mSqrtWeights;
  mQ.col(0).tail(mTest) = Vector::Constant(mTest,(Scalar)1.0);
  mR(0,0)      = (Scalar) sqrt(mSqrtWeights.squaredNorm());
  mQ.col(0)   /= mR(0,0);
  mGamma[0]    = mQ.col(0).head(mN).dot(mY);
  mLambda[0]   = 0.0;                               // dont shrink intercept
  mFitted      = mGamma[0] * mQ.col(0);
  mResiduals   = mY -  mFitted.head(mN);
  mTotalSS = mResidualSS = mResiduals.squaredNorm();
  debug("REGR",4) <<"Intercept gamma[0]=" << mGamma[0] << ", RSS=" << mResidualSS
		  << "    Initial fit=" << mFitted.head(5).transpose()
		  << " Initial res=" << mResiduals.head(5).transpose() << std::endl;
}


//     Accessors      Accessors      Accessors      Accessors      Accessors      Accessors

LinearRegression::Vector
LinearRegression::raw_fitted_values()    const
{
  Vector fit = mFitted;
  fit.segment(0,mN).array() /= mSqrtWeights.array();
  return fit;
}

LinearRegression::Vector
LinearRegression::x_row (int i)            const
{
  if(i < mN)
    return mQ.row(i).head(mK) * RR() / mSqrtWeights[i];
  else
    return mQ.row(i).head(mK) * RR();
}

namespace {


  const SCALAR soft_rate = 0.5;

  SCALAR
  soft_min_at_zero (SCALAR x)    // pushes value below zero toward 0
  { assert (x <= 0);
    return soft_rate * ((SCALAR)exp(x) - (SCALAR) 1) ;
  }
  
  SCALAR
  soft_max_at_one (SCALAR x)     // pushes value > 1 toward 1
  { assert (x >= 1.0);
    return (SCALAR)1 + soft_rate * ((SCALAR)1 - (SCALAR)exp(1.0 - x));
  }

  SCALAR
  soft_limits (SCALAR x)
  { if (x < 0.0)
      return soft_min_at_zero (x);
    else
    { if(x <= 1.0) return x;
      else return soft_max_at_one(x);
    }
  }
}

/*

  This code was used in experiments to truncate predictions in binary models
  to lie closer to the interval [0,1].  Not so useful in the end, and mostly
  relevant in the context of calibration.
  
  inline
  LinearRegression::Vector
  LinearRegression::raw_fitted_values() const
  {
  Vector fit = fitted_values().array()/mSqrtWeights.array(); 
  if (!mBinary)
  return fit;
  else
  return fit.unaryExpr(&soft_limits);
  }
*/

//     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes     Slopes


LinearRegression::Vector
LinearRegression::se_gamma_ls() const
{
  LinearRegression::Scalar s2 (mResidualSS/(Scalar)(mN-mK));
  Vector se (mLambda.head(mK).unaryExpr([s2] (Scalar lam)->Scalar { return (Scalar)sqrt(s2/(1+lam)); }));
  return se;
}


LinearRegression::Vector
LinearRegression::se_gamma() const
{
  if (mBlockSize == 0)
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
      for(int row = 0; row < mN; row += mBlockSize)
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

/*
  #pragma GCC diagnostic push
  #pragma GCC diagnostic ignored "-Wconversion"

LinearRegression::Vector
LinearRegression::test_predictions  (bool truncate)   const
{
  if (q()==0)
    return Vector::Constant(mN,y_bar());
  clock_t timeCalc = clock();
  Vector preds = QQtest() * mGamma.head(mK);
  debugging::debug("VALM",0) << "LRegr Time  ... calc pred in add predictors used " << (float)(clock()-timeCalc)/CLOCKS_PER_SEC << " sec.\n";
  if (!truncate)
    return preds;
  else
    return preds.unaryExpr(&soft_limits);
}

#pragma GCC diagnostic pop
*/

LinearRegression::Vector
LinearRegression::predict(Matrix const& x, bool truncate) const
{
  assert(q() == x.cols());
  assert(can_return_beta());
  Vector b (beta());
  if (q() == 0)
    return Vector::Constant(x.rows(),b(0));
  Vector preds =  (x * b.tail(x.cols())).array() + b(0);    // internal X has leading const column; input X lacks constant
  if (!truncate)
    return preds;
  else
    return preds.unaryExpr(&soft_limits);
}


//     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests     Tests

bool
LinearRegression::is_invalid_ss (LinearRegression::Scalar ss, LinearRegression::Scalar ssz)          const
{
  const    double    epsilon (1.0e-5);
  debug("REGR",4) << "Initial SSz = " << ss << " becomes " << ssz << " after sweeping." << std::endl; 
  if (ssz <= 0.0)
  { debug("REGR",1) << " *** Error: SSz <= 0.0 in regression." << std::endl;
    return true;
  }
  if(std::isnan(ssz))
  { debug("REGR",1) << " *** Error: SSz = NaN in regression." << std::endl;
    return true;
  }
  if(std::isinf(ssz))
  { debug("REGR",1) << " *** Error: SSz = Inf in regression." << std::endl;
    return true;
  }
  if(ssz/ss < epsilon)
  { debug("REGR",1) << "SSZ indicates near singular;  SSZ = " << ssz << std::endl;
    return true;
  }
  return false;
}

LinearRegression::Scalar
LinearRegression::sweep_Q_from_column_and_normalize(int col) const
{
  // all at once, classical GS
  Scalar ss (approximate_ss(mQ.col(col).head(mN)));
  #ifdef _CLASSICAL_GS_
  Vector delta = mQ.block(0,0,mN,col).transpose() * mQ.col(col).head(mN);
  debug("REGR",4) << "Classical GS; sweeping Q from col " << col << "; delta=" << delta.transpose() << std::endl;
  mQ.col(col) = mQ.col(col) - mQ.leftCols(col) * delta;
  mR.col(col).head(col) = delta;
  #else
  //residual from each first, modified GS
  for(int j=0; j<col; ++j)
  { mR(j,col) = mQ.col(j).head(mN).dot(mQ.col(col).head(mN));
    mQ.col(col).noalias() -= mQ.col(j) * mR(j,col);
  }
  #endif
  Scalar ssz  (mQ.col(col).head(mN).squaredNorm());
  if (is_invalid_ss (ss, ssz)) return 0.0;
  mQ.col(col) /= (Scalar) sqrt(ssz);
  mR(col,col)  = (Scalar) sqrt(ssz);
  debug("REGR",4) << "Head of Z after sweep/norm   "              << mQ.col(col).head(8) << std::endl;
  //  std::cout << "TESTING: R matrix after sweeping Q is " << std::endl << mR.topLeftCorner(col+1,col+1) << std::endl;
  //  std::cout << "TESTING: Q matrix is "                  << std::endl << mQ.topLeftCorner(5,col+1) << " ...." << std::endl;
  return ssz;
}
  

FStatistic
LinearRegression::f_test_predictor (std::string xName, Vector const& z) const
{
  assert (z.size() == (mN + mTest));
  mTempNames = name_vec(xName);
  mTempK     = 1;
  if ((int)numberOfAllocatedColumns <= mK)
  { std::cerr << "\n******************\n"
	      << "ERROR: Aborting. Model size has reached internal dimension limit."
	      << "\n******************\n";
    return FStatistic();
  }
  mQ.col(mK).head( mN  ) = z.head(mN).array() * mSqrtWeights.array();     // will die here if mK is too large
  mQ.col(mK).tail(mTest) = z.tail(mTest);
  if (0.0 == sweep_Q_from_column_and_normalize(mK))
  { std::cout << "REGR: Singularity detected. SSz = 0 for predictor " << xName << "; returning empty F stat" << std::endl;
    return FStatistic();
  }
  debug("REGR",4) << "Computing F-statistic for one predictor with block size = " << mBlockSize << std::endl;
  int residualDF (mN-mK);
  assert(residualDF > 0);
  Scalar qe  (mQ.col(mK).head(mN).dot(mResiduals));           // slope of added var is  gamma (e'z)/(z'z = 1)
  if (mBlockSize==0)
  { Scalar regrss (qe * qe);
    return FStatistic(regrss, 1, mResidualSS-regrss, residualDF, Vector::Ones(1));
  }
  // turn off bennett
  if (has_binary_response() && (is_ols_regression()) && (mBlockSize == 1))             // use Bennett and fake F stat from squaring bennett t stat
  { std::pair<Scalar,Scalar> test (bennett_evaluation());
    debug("REGR",2) << "Bennett evaluation returns t = " << test.first << " with p-value = " << test.second <<std::endl;
    return FStatistic(test.first*test.first, test.second, 1, mN-q(), Vector::Ones(1));
  }
  Scalar qeeq (0.0);                                          // compute white estimate; in scalar case, reduces to (z'e)^2/(z'(e^2)z)
  if (mBlockSize == 1)
  { Vector temp (mQ.col(mK).head(mN).array() * mResiduals.array());
    qeeq = temp.squaredNorm();
  }
  else
  { assert (0 == mN % mBlockSize);
    for(int row=0; row<mN; row +=mBlockSize)
    { Scalar ezi (mResiduals.segment(row,mBlockSize).dot(mQ.col(mK).segment(row,mBlockSize)));
      qeeq += ezi * ezi;
    }
  }
  debug("REGR",4) << "White F-stat components qe = " << qe << "  qeeq = " << qeeq << std::endl;
  return FStatistic(qe*qe/qeeq, 1, residualDF, Vector::Ones(1));
}


FStatistic
LinearRegression::f_test_predictors (std::vector<std::string> const& xNames, Matrix const& z) const
{
  mTempNames = xNames;
  mTempK     = (int) z.cols();
  if ((int)numberOfAllocatedColumns <= mK+mTempK)
  { std::cerr << "\n******************\n"
	      << "ERROR: Aborting. Model size has reached internal dimension limit."
	      << "\n******************\n";
    return FStatistic();
  }
  mQ.block(0 ,mK, mN  ,z.cols()) =  mSqrtWeights.asDiagonal() * z.topRows(mN);
  mQ.block(mN,mK,mTest,z.cols()) =  z.bottomRows(mTest);
  for(int k = 0; k<z.cols(); ++k)
    if(0.0 == sweep_Q_from_column_and_normalize(mK+k))
      return FStatistic();
  debug("REGR",5) << "Computing F-statistic for " << xNames.size() << " predictors.\n" << std::endl;
  int residualDF (mN-mK-(int)z.cols());
  assert(residualDF > 0);
  Vector Qe  (mQ.block(0,mK,mN,mTempK).transpose() * mResiduals);    // new gamma coefs
  if (mBlockSize==0)
  { Scalar regrss (Qe.squaredNorm());
    debug("REGR",3) << "F-stat components (" << regrss << "/" << mTempK << ")/(" << mResidualSS-regrss << "/" << residualDF << ")" << std::endl;
    return FStatistic(regrss, mTempK, mResidualSS-regrss, residualDF, Vector::Ones(z.cols()));
  }
  else
  { Matrix QeeQ(mTempK,mTempK);                                    // Q'ee'Q 
    QeeQ.setZero();
    if (mBlockSize == 1)
    { Matrix eQ (mResiduals.asDiagonal() * mQ.block(0,mK,mN,mTempK));  
      QeeQ.noalias() = eQ.transpose() * eQ;
    }
    else     // blocksize > 1
    { assert(0 == mN % mBlockSize);
      Vector eQ(mTempK);
      for(int block = 0, row = 0; block<mN/mBlockSize; ++block)
      {	eQ = mResiduals.segment(row,mBlockSize).transpose() * mQ.block(row,mK,mBlockSize,mTempK);
	QeeQ.noalias() += eQ * eQ.transpose();
	row += mBlockSize;
      }
    }
    Scalar ss = (Qe.transpose() * QeeQ.inverse() * Qe)(0,0);
    debug("REGR",3) << "F-stat = " << ss << "/" << mTempK << " with " << residualDF << " residual DF." << std::endl;
    return FStatistic(ss/(Scalar)mTempK, mTempK, residualDF, Vector::Ones(z.cols()));
  }
}

//    Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     Bennett     

// RAS: pretty sure this is wrong when the response gets weighted

std::pair<LinearRegression::Scalar,LinearRegression::Scalar>
LinearRegression::bennett_evaluation () const
{
  debug("REGR",4) << "Bennett: Entering Bennett evaluation" << std::endl;
  const Scalar up = (Scalar) 0.99999999999;
  const Scalar dn = (Scalar) 0.00000000001;
  Vector mu       = raw_fitted_values().head(mN);                                  // think of fit as E(Y), constrained to [eps,1-eps] interval
  debug("REGR",4) << "Bennett: mu head=" << mu.head(5).transpose() << "   tail=" << mu.tail(5).transpose() << std::endl;
  debug("REGR",4) << "Bennett: wts head=" << mWeights.head(5).transpose() << "   tail=" << mWeights.tail(5).transpose() << std::endl;
  mu.unaryExpr([&up,&dn](Scalar x)->Scalar { if(x>up) return up; if(x<dn) return dn; return x;}) ;
  Vector var     (Vector::Ones(mN));
  var = mu.array() * (var - mu).array();
  Vector dev     (mY - mu);                                                        // would match residuals IF other fit is bounded
  Scalar num     (dev.dot(mQ.col(mK).head(mN)));                                                  // z'(y-y^)
  Scalar rootZDZ ((Scalar)sqrt (var.dot(mQ.col(mK).head(mN).cwiseProduct(mQ.col(mK).head(mN))))); // sqrt(z'Dz)
  debug("REGR",4) << "Bennett: num=" << num << "   root=" << rootZDZ << std::endl;
  Scalar maxA    (0.0);
  for (int i=0; i<mN; ++i)
  { Scalar absZ (abs_val(mQ(i,mK) * max_abs(mu[i], (Scalar)1.0-mu[i])));         // largest possible error for this case
    if (absZ > maxA) maxA = absZ;                                                  // largest?
  }
  Scalar Mz      (maxA/rootZDZ);
  Scalar tz      (abs_val(num)/rootZDZ);                                           // num = get(mZE,0)
  debug("REGR",4) << "Bennett: maxA=" << maxA << "   tz=" << tz << std::endl;
  return std::make_pair(tz, bennett_p_value(tz,Mz));
}


//     Add predictor     Add predictor     Add predictor     Add predictor     Add predictor     Add predictor     Add predictor

void
LinearRegression::update_names_and_gamma(StringVec xNames)
{
  debug("REGR",3) << "Updating fit, first of " << xNames.size() << " is " << xNames[0] << "\n";
  if ((int)numberOfAllocatedColumns-5 < mK+(int)xNames.size())
    std::cerr << "\n********************\n"
	      << " WARNING: mK = " << mK << " is approaching upper dimension limit " << numberOfAllocatedColumns
	      << "\n********************\n";
  assert (mTempK == (int)xNames.size());
  for(unsigned int j=0; j<xNames.size(); ++j)
  { mXNames.push_back(xNames[j]);
    mGamma[mK+j]  = (mQ.col(mK+j).head(mN).dot(mResiduals)/(1+mLambda[mK+j]));
  }
}

void
LinearRegression::update_fit(StringVec xNames)
{
  update_names_and_gamma(xNames);
  for(int j=0; j<mTempK; ++j)
  { Vector delta = mQ.col(mK+j) * mGamma[mK+j];
    mFitted    += delta; 
    mResiduals -= delta.head(mN);
  }
  mK += mTempK;
  mResidualSS= mResiduals.squaredNorm();      // Faster?  subtract new gamma'gamma
}


void
LinearRegression::add_predictors (StringVec const& zNames, Matrix const& z)
{
  assert(z.rows() == mN);
  assert((int)zNames.size() == z.cols());
  mTempNames = zNames;
  mTempK     = (int) z.cols();
  mQ.block( 0,mK, mN  ,z.cols()) = mSqrtWeights.asDiagonal() * z.topRows(mN);
  mQ.block(mN,mK,mTest,z.cols()) = z.bottomRows(mTest);
  for(int k = 0; k<z.cols(); ++k)
    if(0.0 == sweep_Q_from_column_and_normalize(mK+k))
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
  debug("REGR",3) << "Adding " << mTempK << " previously tested predictors; entry stat for added predictors is " << fstat << std::endl;
  Scalar lambda (0);
  Scalar F (fstat.f_stat());
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
  debug("REGR",3) << "Adding " << mTempK << " previously tested predictors; forced addition." << std::endl;
  for (unsigned int j=0; j<mTempNames.size(); ++j)
    mLambda[mK+j] = (Scalar) 0;
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
LinearRegression::print_to (std::ostream& os, bool compact) const
{
  os.precision(6);
  if (compact)
  { os << " R2=" << r_squared() << " RMSE=" << rmse() << std::endl;
    std::vector<size_t> indices;
    size_t Q = (size_t) q();
    if (Q > 100)
      indices = {0, Q-2, Q-1, Q};
    else if (Q > 10)
      indices = {0,Q-1,Q};
    else
      indices = {Q};
    compact_print_gamma_to(os,indices);
  }
  else
  { os << mWeightStr << output_label();
    if (has_binary_response())
      os << " (Binary response)";
    os << "  y = " << mYName << "    (n=" << mN << ", nTest=" << mTest << ", k=" << mK << ") " << std::endl
       << "            Total SS    = " << mTotalSS    << "     R^2 = " << r_squared() << std::endl
       << "            Residual SS = " << mResidualSS << "    RMSE = " << rmse() << std::endl << std::endl;
    print_gamma_to(os);
  }
}

void
LinearRegression::compact_print_gamma_to (std::ostream&os, std::vector<size_t>indices) const
{
  Vector se (se_gamma());
  { for (size_t j = 0; j<indices.size(); ++j)
    { size_t k = indices[j];
      os << "[" << std::setw(5) << k << "] "
	 << std::setw(maxNameLen-15) << printed_name(mXNames[k])  << "    "
	 << std::setw(9) << mGamma[k] << "  "
	 << std::setw(8) << se[k]     << "  "
	 << std::setw(8) << mGamma[k]/se[k] << std::endl;
    }
  }
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
  assert (can_return_beta());
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
LinearRegression::write_data_to (std::ostream& os, int maxNumXCols, bool includeValidation) const
{
  // number of columns of predictors
  int numX = min_int(mK-1,maxNumXCols);
  // prefix line with var names; intercept is name[0]
  os << "Role\tWeight\tFit\tResidual\t" << mYName;
  // skip the intercept in column 0
  for(int j=1; j<=numX; ++j)  
    os << "\t" << mXNames[j];
  os << std::endl;
  // put the data in external coordinate system
  Vector y    (raw_y            ());
  Vector res  (raw_residuals    ());
  Vector fit  (raw_fitted_values());
  int limit = (includeValidation) ? mN + mTest : mN;
  for(int i=0; i<limit; ++i)
  { if (i<mN) os << "est\t" << mWeights[i] << '\t' << fit[i] << '\t' << res[i] << "\t" << y[i] ;
    else      os << "val\t" <<    1        << '\t' << fit[i] << '\t' << "NA"   << "\t" << "NA" ;
    if(numX>0)
    { Vector row (x_row(i));
      for (int j=1; j<=numX; ++j)  // skip intercept
	os << '\t' << row[j];
    }
    os << std::endl;
  }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//     FastLinearRegression     FastLinearRegression     FastLinearRegression     FastLinearRegression     FastLinearRegression
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void
FastLinearRegression::allocate_projection_memory()
{
  mA      = Matrix::Zero(mN + mTest, mOmegaDim);
  mAtA    = Matrix::Zero( mOmegaDim, mOmegaDim);
}

void
FastLinearRegression::apply_gradient_correction()
{
  debug("FREG",3) << "Apply gradient correction with mK=" << mK << std::endl;
  const int q = (mK < 11) ? mK : 10;
  debug("FREG",2) << "Prior to gradient, RSS= " << mResiduals.squaredNorm()
		  << "  Tail  pre-gamma = " << mGamma.segment(mK-q+1,q).transpose() << std::endl;
  for (int j=1; j<mK; ++j)
  { Scalar dGamma = mQ.col(j).head(mN).dot(mResiduals);
    mGamma(j) += dGamma;
    Vector delta = dGamma * mQ.col(j);
    mFitted    += delta;
    mResiduals -= delta.head(mN);
  }
  mResidualSS = mResiduals.squaredNorm();
 debug("FREG",2) << "After    gradient, RSS= " << mResidualSS
		  << "  Tail post-gamma = " << mGamma.segment(mK-q+1,q).transpose() << std::endl;
}

// suppress warnings regarding Eigen conversions
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"

LinearRegression::Scalar
FastLinearRegression::sweep_Q_from_column_and_normalize(int col)      const
{
  debug("FREG",4) << "Sweep_Q_from_col (fast after omega cols); col= " << col << std::endl;
  if ((size_t)mK <= mOmegaDim)                                     // small models are handled classically
    return LinearRegression::sweep_Q_from_column_and_normalize(col);
  mQ.col(col).array() -= mQ.col(col).head(mN).sum() / (Scalar) mN; // subtract mean
  Scalar ss = mQ.col(col).head(mN).squaredNorm();                  // compare initial SS to post sweep SS to check for singularities
  Vector b = mA.topRows(mN).transpose() * mQ.col(col).head(mN);    // compute b = (A'A)-1 A'z
  mCholAtA.solveInPlace (b);                                       // mAtA.selfadjointView<Eigen::Upper>().ldlt().solveInPlace(b);
  mQ.col(col) -= mA * b;                                           // compute z = z - Ab
  Scalar ssz  (mQ.col(col).head(mN).squaredNorm());                // check that SS dropped and is not nan
  if (is_invalid_ss (ss, ssz)) return 0.0;
  Scalar norm = (Scalar) sqrt(ssz);
  mQ.col(col) /= norm;                                             // normalize swept column
  mR(col,col)  = norm;
  return ssz;
}
 
void
FastLinearRegression::update_fit(StringVec xNames)
{
  update_names_and_gamma(xNames);
  Matrix w = Matrix::Random(mTempK,mOmegaDim);                        // update random projection, uni[-1,1]
  mA += mQ.block(0,mK, mQ.rows(), mTempK) * w;                        // A <- A + z w'
  Matrix Atz = mA.topRows(mN).transpose() * mQ.block(0,mK,mN,mTempK); // A'z
  if (1 == mTempK)
  { for(int j=0; j< (int) mOmegaDim; ++j)                             // update upper half of A'A; z~ = mQ[mK]
      for(int k=j; k< (int) mOmegaDim; ++k)
	mAtA(j,k) +=  w(0,j)*w(0,k) + Atz(j,0)*w(0,k) + w(0,j)*Atz(k,0);
  }
  else
  { Matrix wtw = w.transpose() * w;
    for(int j=0; j<(int)mOmegaDim; ++j)                               // update upper half of A'A; z~ = mQ[mK]
      for(int k=j; k<(int)mOmegaDim; ++k)
	mAtA(j,k) +=    wtw(j,k)    + Atz.row(j).dot(w.col(k)) + Atz.row(k).dot(w.col(k));
  }
  mCholAtA = Eigen::LDLT<Matrix,Eigen::Upper> (mAtA);
  Vector delta = mQ.block(0,mK,mN+mTest,mTempK) * mGamma.segment(mK,mTempK); 
  mFitted     += delta;
  mResiduals  -= delta.head(mN);
  mResidualSS  = mResiduals.squaredNorm();
  mK += mTempK;
}

#pragma GCC diagnostic push


