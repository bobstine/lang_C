#ifndef _VALIDATED_REGRESSION_TEMPLATE_H_
#define _VALIDATED_REGRESSION_TEMPLATE_H_

#include "validated_regression.h"
#include "little_functions.h"
#include "debug.h"


template<class Regr>
std::vector<typename ValidatedRegression<Regr>::Scalar>
ValidatedRegression<Regr>::beta()  const
{
  std::vector<Scalar> b;
  if(mModel.can_return_beta())
  { b.resize(mModel.q()+1);
    mModel.fill_with_beta(b.begin());
  }
  return b;
}

//     initialize     initialize     initialize     initialize     initialize     initialize     initialize     initialize     

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
  mModel = Regr(yName, y.head(mN), w.head(mN), mValidationY.size(), blockSize);
  if (mValidationY.size() > 0)
    initialize_validation_ss();  // needs mModel and mValidationY
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"

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
  mModel = Regr(yName, y.head(mN), mValidationY.size(), blockSize);
  if (mValidationY.size() > 0)
    initialize_validation_ss();  // needs mModel and mValidationY
}

#pragma GCC diagnostic pop


//     add_predictors_if_useful     add_predictors_if_useful     add_predictors_if_useful     add_predictors_if_useful

template <class Regr>
template <class Iter>
  std::pair<typename ValidatedRegression<Regr>::Scalar,typename ValidatedRegression<Regr>::Scalar>
  ValidatedRegression<Regr>::add_predictors_if_useful (std::vector<std::pair<std::string, Iter> > const& c, Scalar pToEnter)
{
  FStatistic f;
  int k ((int)c.size());                                                // k denotes the number of added variables
  std::vector<std::string> xNames;
  typename Regr::Matrix predictors(mLength,k);
  for(int j=0; j<k; ++j)
  { xNames.push_back(c[j].first);
    predictors.col(j) = permuted_vector_from_iterator(c[j].second);
  }
  if (k == 1)
    f = mModel.f_test_predictor(xNames[0], predictors.col(0));
  else
    f = mModel.f_test_predictors(xNames, predictors.leftCols(k));
  debugging::debug("VALM",3) << k << " predictors; p-value=" << f.p_value() << " with bid=" << pToEnter << ". SE block size=" << block_size() << std::endl;
  if((f.f_stat() == 0) || (f.p_value() > pToEnter))
    return std::make_pair(f.f_stat(), f.p_value());
  debugging::debug("VALM",3) << "Adding " << k << " predictors to model; first is " << c[0].first << std::endl;
  if(mShrink)  mModel.add_predictors(f);
  else         mModel.add_predictors();                                  // omit f-stat to omit shrinkage
  if (0 < mValidationY.size())                                           // update validation SS if have some 
    mValidationSS = (mValidationY - mModel.test_predictions()).squaredNorm();
  return std::make_pair(f.f_stat(), f.p_value());
}

//     permuted_vector_from_iterator     permuted_vector_from_iterator     permuted_vector_from_iterator     permuted_vector_from_iterator

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
  else // just copy directly into Eigen::vector
  { for(int i=0; i<mLength; ++i, ++it)
      v[ i ] = *it;
  }
  return v;
}


//     fill_with_     fill_with_     fill_with_     fill_with_     fill_with_     fill_with_     fill_with_     fill_with_

template <class Regr>
template <class Iter>
void
ValidatedRegression<Regr>::fill_with_fit(Iter it, bool truncate) const
{
  Vector results (mLength);
  
  results.segment(        0           , n_estimation_cases()) = mModel.raw_fitted_values(truncate);
  results.segment(n_estimation_cases(), n_validation_cases()) = mModel.test_predictions(truncate);
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
  results.segment(n_estimation_cases(), n_validation_cases()) = mValidationY - mModel.test_predictions();
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


//     confusion_matrix     confusion_matrix     confusion_matrix     confusion_matrix     confusion_matrix     confusion_matrix     
  
template<class Regr>
ConfusionMatrix
ValidatedRegression<Regr>::estimation_confusion_matrix(Scalar threshold) const
{
  assert (mModel.has_binary_response());
  Vector y = mModel.raw_y();
  Vector fit = mModel.fitted_values();
  return ConfusionMatrix(y.size(), EigenVectorIterator(&y), EigenVectorIterator(&fit), threshold);
}

template<class Regr>
ConfusionMatrix
ValidatedRegression<Regr>::validation_confusion_matrix(Scalar threshold) const
{
  assert (mModel.has_binary_response());
  assert (n_validation_cases() > 0);
  Vector pred = mModel.test_predictions();
  return ConfusionMatrix(mValidationY.size(), EigenVectorIterator(&mValidationY), EigenVectorIterator(&pred), threshold);
}


//     print_to     print_to     print_to     print_to     print_to     print_to     print_to     print_to     print_to     

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
       << "          Validation SS = " << validation_ss() << std::endl;
    if (mModel.has_binary_response())
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
ValidatedRegression<Regr>::write_data_to(std::ostream& os, int maxNumXCols) const    // Note: does not return the data to the original ordering
{
  const bool showValidation = false;
  mModel.write_data_to(os, maxNumXCols, showValidation);  // show here with y value
  Vector        preds = mModel.test_predictions();
  Matrix const& Q     = mModel.Q_basis_matrix();
  for(int i=0; i<n_validation_cases(); ++i)
  { os << "val\t" << preds[i] << '\t' << mValidationY[i]-preds[i] << '\t' << mValidationY[i];
    for (int j=0; j<min_int((int)Q.cols(), maxNumXCols); ++j) 
      os << '\t' << Q(i+mN,j);                            // skip estimation rows in Q
    os << std::endl;
  }
}

#endif
