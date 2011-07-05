#include "debug.h"

template <class Iter>
void LinearRegression::fill_with_beta (Iter begin) const
{
  Vector b (beta());
  for(int i=0; i<b.size(); ++i)
    *begin++ = b[i];
  return;
}


//     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression     ValidatedRegression

template <class Iter>
std::pair<double,double>
  ValidatedRegression::add_predictors_if_useful (std::vector<std::pair<std::string, Iter> > const& c, double pToEnter)
{
  FStatistic f;
  int k (c.size());                                                                  // k denotes the number of added variables
  std::vector<std::string> xNames;
  LinearRegression::Matrix predictors(mLength,k);
  for(int j=0; j<k; ++j)
  { xNames.push_back(c[j].first);
    predictors.col(j) = permuted_vector_from_iterator(c[j].second);
  }
  if (k == 1)
  { f = mModel.f_test_predictor(xNames[0], predictors.col(0).head(mN));
    debugging::debug("VALM",3) << "Predictor obtains p-value " << f.p_value() << " with bid " << pToEnter << " and std error block size " << block_size() << std::endl;
  } else
  { f = mModel.f_test_predictors(xNames, predictors.topLeftCorner(mN,k));           // was top right... same but weird
    debugging::debug("VALM",3) << k << " predictors obtain p-value " << f.p_value() << " with bid " << pToEnter << " and std error block size " << block_size() << std::endl;
  }
  if (f.p_value() > pToEnter)
    return std::make_pair(f.f_stat(), f.p_value());
  debugging::debug("VALM",3) << "Adding " << k << " predictors to model; first is " << c[0].first << std::endl;
  if (0 == mModel.q())  // first added variables; rows & cols relative to a corner
    mValidationX = predictors.bottomLeftCorner(n_validation_cases(), k);
  else                  // add additional columns
  { Matrix x(mValidationX.rows(),mValidationX.cols()+k);
    x.topLeftCorner(mValidationX.rows(), mValidationX.cols()) = mValidationX; // insert prior columns, then add new ones
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



template<class Iter, class BIter, class WIter>
  void
  ValidatedRegression::initialize(std::string yName, Iter Y, BIter B, WIter W, int blockSize)
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
  mModel = LinearRegression(yName, y.head(mN), w.head(mN), blockSize);
  if (mValidationY.size() > 0)
    initialize_validation_ss();  // needs mModel and mValidationY
}


template<class Iter, class BIter>
  void
  ValidatedRegression::initialize(std::string yName, Iter Y, BIter B, int blockSize)
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
  mModel = LinearRegression(yName, y.head(mN), blockSize);
  if (mValidationY.size() > 0)
    initialize_validation_ss();  // needs mModel and mValidationY
}


template<class Iter>
LinearRegression::Vector
ValidatedRegression::permuted_vector_from_iterator(Iter it) const
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
ValidatedRegression::fill_with_fit(Iter it, bool truncate) const
{
  Vector results (mLength);
  
  if (truncate)
  { double min (0.0);
    double max (1.0);
    results.segment(        0           , n_estimation_cases()) = mModel.raw_fitted_values(min,max);
    results.segment(n_estimation_cases(), n_validation_cases()) = mModel.predictions(mValidationX,min,max);
  }
  else
  { results.segment(        0           , n_estimation_cases()) = mModel.raw_fitted_values();
    results.segment(n_estimation_cases(), n_validation_cases()) = mModel.predictions(mValidationX);
  } 
  for(int i = 0; i<mLength; ++i)
    *it++ = results(mPermute[i]);
}
