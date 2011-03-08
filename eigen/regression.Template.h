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
  int k (c.size());                                                                  // k denotes the number of added variables
  LinearRegression::Matrix predictors(mLength,k);
  for(int j=0; j<k; ++j)
    predictors.col(j) = permuted_vector_from_iterator(c[j].second);
  FStatistic f;
  if (k == 1)
  { f = mModel.f_test_predictor(predictors.col(0).start(mN));
    debugging::debug("VALM",3) << "Predictor obtains p-value " << f.p_value() << " with bid " << pToEnter << " and std error block size " << block_size() << std::endl;
  } else
  { f = mModel.f_test_predictors(predictors.corner(Eigen::TopLeft,mN,k));           // was top right... same but weird
    debugging::debug("VALM",3) << k << " predictors obtain p-value " << f.p_value() << " with bid " << pToEnter << " and std error block size " << block_size() << std::endl;
  }
  if (f.p_value() > pToEnter)
    return std::make_pair(f.f_stat(), f.p_value());
  debugging::debug("VALM",3) << "Adding " << k << " predictors to model; first is " << c[0].first << std::endl;
  if (0 == mModel.q())  // first added variables; rows & cols relative to a corner
    mValidationX = predictors.corner(Eigen::BottomLeft, n_validation_cases(), k);
  else                  // add additional columns
  { Matrix x(mValidationX.rows(),mValidationX.cols()+k);
    x.corner(Eigen::TopLeft , mValidationX.rows(), mValidationX.cols()) = mValidationX; // insert prior columns, then add new ones
    x.corner(Eigen::TopRight, mValidationX.rows(), k                  ) = predictors.corner(Eigen::BottomLeft, mValidationX.rows(), k);
    mValidationX = x;
  }
  std::vector<std::string> xNames;
  for(int j=0; j<k; ++j)
    xNames.push_back(c[j].first);
  mModel.add_predictors(xNames, predictors.corner(Eigen::TopRight,mN,k));
  return std::make_pair(f.f_stat(), f.p_value());
}


template<class Iter, class BIter>
void
  ValidatedRegression::initialize(std::string yName, Iter Y, BIter B, int blockSize)
{ 
  Eigen::VectorXd y(mLength);
  int k (mLength);
  for(int i=0; i<mLength; ++i, ++Y, ++B)
  { if (*B)   // use for estimation, increment mN
    { y[mN] = *Y; mPermute[i] = mN; ++mN; }
    else      // reverse to the end for validation
    { --k; y[k] = *Y; mPermute[i]=k; }
  }
  if (blockSize != 0) assert(mN % blockSize == 0);
  mValidationY = y.end(mLength-mN);
  debugging::debug("VALM",3) << "Initializing validation model, estimation size = " << mN << " with validation size = " << mValidationY.size() << std::endl;
  mModel = LinearRegression(yName, y.start(mN), blockSize);
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
ValidatedRegression::fill_with_fit(Iter it) const
{
  Vector results (mLength);
  results.segment(        0           , n_estimation_cases()) = mModel.fitted_values();
  results.segment(n_estimation_cases(), n_validation_cases()) = mModel.predictions(mValidationX);
  for(int i = 0; i<mLength; ++i)
    *it++ = results(mPermute[i]);
}
