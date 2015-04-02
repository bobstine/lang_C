#ifndef _VALIDATED_REGRESSION_TEMPLATE_H_
#define _VALIDATED_REGRESSION_TEMPLATE_H_

#include "validated_regression.h"
#include "little_functions.h"

#include "debug.h"
  
template <class Regr>
template <class Iter>
  std::pair<typename ValidatedRegression<Regr>::Scalar,typename ValidatedRegression<Regr>::Scalar>
  ValidatedRegression<Regr>::add_predictors_if_useful (std::vector<std::pair<std::string, Iter> > const& c, Scalar pToEnter)
{
  FStatistic f;
  int k ((int)c.size());                                                    // k denotes the number of added variables
  std::vector<std::string> xNames;
  typename Regr::Matrix predictors(mLength,k);
  for(int j=0; j<k; ++j)
  { xNames.push_back(c[j].first);
    predictors.col(j) = permuted_vector_from_iterator(c[j].second);
  }
  if (k == 1)
    f = mModel.f_test_predictor(xNames[0], predictors.col(0).head(mN));
  else
    f = mModel.f_test_predictors(xNames, predictors.topLeftCorner(mN,k));
  debugging::debug("VALM",3) << k << " predictors; p-value=" << f.p_value() << " with bid=" << pToEnter << ". SE block size=" << block_size() << std::endl;
  if((f.f_stat() == 0) || (f.p_value() > pToEnter))
    return std::make_pair(f.f_stat(), f.p_value());
  debugging::debug("VALM",3) << "Adding " << k << " predictors to model; first is " << c[0].first << std::endl;
  if (0 == mModel.q())                                                        // first variable in model; rows & cols relative to a corner
    mValidationX = predictors.bottomLeftCorner(n_validation_cases(), k);
  else                                                                        // add additional columns
  { Matrix x(mValidationX.rows(),mValidationX.cols()+k);
    x.topLeftCorner(mValidationX.rows(), mValidationX.cols())  = mValidationX; // insert prior columns, then add new ones
    x.topRightCorner(mValidationX.rows(), k                  ) = predictors.bottomLeftCorner(mValidationX.rows(), k);
    mValidationX = x;
  }
  if(mShrink)
    mModel.add_predictors(f);
  else                            // omit f-stat to omit shrinkage
    mModel.add_predictors();
  if (n_validation_cases() > 0)   // update validation SS if have some 
    mValidationSS = (mValidationY - mModel.predictions(mValidationX)).squaredNorm();
  return std::make_pair(f.f_stat(), f.p_value());
}


template<class Regr>
template<class Iter, class BIter, class WIter>
  void
  ValidatedRegression<Regr>::initialize(std::string yName, Iter Y, BIter B, WIter W, int blockSize)
{
  Vector w (mLength);
  Vector y (mLength);
  int  k (mLength);
  for(int i=0; i<mLength; ++i, ++Y, ++W, ++B)
  { if (*B)   // use for estimation, increment mN
    { y[mN] = *Y; w[mN] = *W; mPermute[i] = mN; ++mN; }
    else      // reverse to the end for validation
    { --k; y[k] = *Y; w[k] = *W; mPermute[i]=k; }
  }
  mValidationY = y.tail(mLength-mN);
  debugging::debug("VALM",3) << "Initializing weighted validation model, estimation size = " << mN << " with validation size = " << mValidationY.size() << std::endl;
  mModel = Regr(yName, y.head(mN), w.head(mN), blockSize);
  if (mValidationY.size() > 0)
    initialize_validation_ss();  // needs mModel and mValidationY
}


template<class Regr>
template<class Iter, class BIter>
  void
  ValidatedRegression<Regr>::initialize(std::string yName, Iter Y, BIter B, int blockSize)
{ 
  Vector y(mLength);
  int k  (mLength);
  for(int i=0; i<mLength; ++i, ++Y, ++B)
  { if (*B)   // use for estimation, increment mN
    { y[mN] = *Y; mPermute[i] = mN; ++mN; }
    else      // reverse to the end for validation
    { --k; y[k] = *Y; mPermute[i]=k; }
  }
  if (blockSize != 0) assert(mN % blockSize == 0);
  mValidationY = y.tail(mLength-mN);
  debugging::debug("VALM",3) << "Initializing validation model, estimation size = " << mN << " with validation size = " << mValidationY.size() << std::endl;
  mModel = Regr(yName, y.head(mN), blockSize);
  if (mValidationY.size() > 0)
    initialize_validation_ss();  // needs mModel and mValidationY
}


template<class Regr>
template<class Iter>
typename Regr::Vector
ValidatedRegression<Regr>::permuted_vector_from_iterator(Iter it) const
{
  Vector v(mLength);
  if (n_validation_cases()>0)
  { for(int i=0; i<mLength; ++i, ++it)
      v[ mPermute[i] ] = *it;
  }
  else // just copy directly into a eigen::vector
  { for(int i=0; i<mLength; ++i, ++it)
      v[ i ] = *it;
  }
  return v;
}


template <class Regr>
template <class Iter>
void
ValidatedRegression<Regr>::fill_with_fit(Iter it, bool truncate) const
{
  Vector results (mLength);
  
  results.segment(        0           , n_estimation_cases()) = mModel.raw_fitted_values(truncate);
  results.segment(n_estimation_cases(), n_validation_cases()) = mModel.predictions(mValidationX,truncate);
  for(int i = 0; i<mLength; ++i)
    *it++ = results(mPermute[i]);
}


template <class Regr>
template <class Iter>
void
ValidatedRegression<Regr>::fill_with_residuals(Iter it) const
{
  Vector results (mLength);
  // stuff into Eigen temp first
  results.segment(        0           , n_estimation_cases()) = mModel.raw_residuals();
  results.segment(n_estimation_cases(), n_validation_cases()) = mValidationY - mModel.predictions(mValidationX);
  // then reverse the cv twiddle
  for(int i = 0; i<mLength; ++i)
    *it++ = results(mPermute[i]);
}


template <class Regr>
void
ValidatedRegression<Regr>::initialize_validation_ss()
{ 
  Scalar mean (mModel.y_bar());
  mValidationSS = mValidationY.unaryExpr([mean](Scalar x)->Scalar { return x-mean; }).squaredNorm();
}

//     confusion_matrix     confusion_matrix     confusion_matrix
  
template<class Regr>
ConfusionMatrix
ValidatedRegression<Regr>::estimation_confusion_matrix(Scalar threshold) const
{
  assert (mModel.is_binary());
  Vector y = mModel.raw_y();
  Vector fit = mModel.fitted_values();
  return ConfusionMatrix(y.size(), EigenVectorIterator(&y), EigenVectorIterator(&fit), threshold);
}

template<class Regr>
ConfusionMatrix
ValidatedRegression<Regr>::validation_confusion_matrix(Scalar threshold) const
{
  assert (mModel.is_binary());
  assert (n_validation_cases() > 0);
  Vector pred = mModel.predictions(mValidationX);
  return ConfusionMatrix(mValidationY.size(), EigenVectorIterator(&mValidationY), EigenVectorIterator(&pred), threshold);
}


//     print_to     print_to     print_to     

template<class Regr>
void
ValidatedRegression<Regr>::print_to(std::ostream& os, bool compact) const
{
  os.precision(6);
  if (compact)
  { os << " CVSS=" << validation_ss() << " ";
    mModel.print_to(os,true);
  }
  else
  { os << "Validated Regression      n(est) = " << mN << "    n(validate) = " << n_validation_cases() << "    ";
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
}

template<class Regr>
void
ValidatedRegression<Regr>::write_data_to(std::ostream& os, int maxNumXCols) const
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

#endif
