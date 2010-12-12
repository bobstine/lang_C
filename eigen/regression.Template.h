#include "debug.h"

template <class Iter>
void LinearRegression::fill_with_beta (Iter begin) const
{
  Vector b (beta());
  for(int i=0; i<b.size(); ++i)
    *begin++ = b[i];
  return;
}


template <class Iter>
void LinearRegression::fill_with_fitted_values (Iter begin) const
{
  Vector fit (fitted_values());
  for (int i = 0; i<fit.size(); ++i)
    *begin++ = fit[i];
}


template <class Iter>
void LinearRegression::fill_with_predictions (Matrix const& x, Iter begin) const
{
  Vector yhat (predict(x));
  for (int i = 0; i<yhat.size(); ++i)
    *begin++ = yhat[i];
}


//     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression

template <class Iter>
std::pair<double,double>
ValidatedRegression::add_predictors_if_useful (std::vector<std::pair<std::string, Iter> > const& c, double pToEnter)
{
  int k (c.size());                                                                  // k denotes the number of added variables
  LinearRegression::Matrix preds(mLength,k);
  for(int j=0; j<k; ++j)
    preds.col(j) = split_iterator(c[j].second);
  FStatistic f;
  if (k == 1)
    f = mModel.f_test_predictor(preds.col(0).start(mN),mBlockSize);                  // block size determines if use white
  else
    f =  mModel.f_test_predictors(preds.corner(Eigen::TopRight,mN,k),mBlockSize);   
  debugging::debug("VALM",3) << "Predictor obtains p-value " << f.p_value() << " with bid " << pToEnter << " and std error block size " << mBlockSize << std::endl;
  if (f.p_value() > pToEnter)
    return std::make_pair(f.f_stat(), f.p_value());
  debugging::debug("VALM",3) << "Adding " << k << " predictors to model; first is " << c[0].first << std::endl;
  if (0 == mModel.q())  // first added variables
    mValidationX = preds.corner(Eigen::BottomLeft, n_validation_cases(), k);
  else                  // add additional columns
  { Matrix x(mValidationX.rows(),mValidationX.cols()+k);
    x.corner(Eigen::TopLeft , mValidationX.rows(), mValidationX.cols()) = mValidationX;
    x.corner(Eigen::TopRight, mValidationX.rows(), k                  ) = preds.corner(Eigen::BottomLeft, mValidationX.rows(), k);
    mValidationX = x;
  }
  std::vector<std::string> xNames;
  for(int j=0; j<k; ++j)
    xNames.push_back(c[j].first);
  mModel.add_predictors(xNames, preds.corner(Eigen::TopRight,mN,k));
  return std::make_pair(f.f_stat(), f.p_value());
}


template<class Iter, class BIter>
void
ValidatedRegression::initialize(std::string yName, Iter Y, BIter B)
{ 
  Eigen::VectorXd y(mLength);
  int k (mLength);
  for(int i=0; i<mLength; ++i, ++Y, ++B)
  { if (*B)
    { y[mN] = *Y; mPermute[i] = mN; ++mN; }
    else
    { --k; y[k] = *Y; mPermute[i]=k; }
  }
  if (mBlockSize != 0) assert(mN % mBlockSize == 0);
  mValidationY = y.end(mLength-mN);
  debugging::debug("VALM",3) << "Initializing validation model, Estimation size = " << mN << " with validation size = " << mValidationY.size() << std::endl;
  mModel = LinearRegression(yName, y.start(mN));
}


template<class Iter>
LinearRegression::Vector
ValidatedRegression::split_iterator(Iter it) const
{
  LinearRegression::Vector v(mLength);
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


template <class Iter>
void
ValidatedRegression::fill_with_fit(Iter it) const
{
  Vector fit (mModel.fitted_values());
  for(int i = 0; i<n_estimation_cases(); ++i)   // these are first n, in order (no need to permute)
    *it++ = fit(i);
  mModel.fill_with_predictions(mValidationX, it);
}
