#include "validated_regression.Template.h"
#include "little_functions.h"

#include "debug.h"

#include <thread>


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//     cross validation     cross validation     cross validation     cross validation     cross validation
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


const int    noBlocking    = 0;             // no white blocking
const bool   noShrinkage   = false;         // no shrinkage
const SCALAR singularPval  = (SCALAR)0.995; // threshold for deciding if added variable is 'too close' to singular


/*
  Regression worker fills a pre-allocated matrix with summary statistics from fitting a sequence
  of progressively larger regression models constructed -- in given order -- from the input X
  matrix.  The model is inialized with the columns in Xi (and included a constant).  The summary
  statistics are RSS, R2 and AIC.
*/

class RegressionWorker
{
public:
  typedef SCALAR Scalar;
  typedef VECTOR Vector;
  typedef MATRIX Matrix;
  
private:
  Vector const*   mY;
  Matrix const*   mXi;
  Matrix const*   mX;
  int             mSkipped;
  Matrix      *   mResults;
  bool            mTrace;

public:    
  RegressionWorker(Vector const* y, Matrix const* Xi, Matrix const* X, Matrix *results, bool trace)
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
  
  void add_predictor_if_useful(LinearRegression *regr, std::string name, Vector const& x, bool verbose=false)
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
  statistics, it computes a CVSS based on the input iterator that tells it which cases
  to exclude from the analysis.  The results are placed in the external CVSS array.
*/
class ValidationWorker
{
public:
  typedef LinearRegression   Regression;
  typedef Regression::Scalar Scalar;
  typedef Regression::Vector Vector;
  typedef Regression::Matrix Matrix;
  
private:
  Vector const*                      mY;
  Matrix const*                      mXi;
  Matrix const*                      mX;
  std::vector<bool>::const_iterator  mSelector;
  int                                mSkipped;
  Vector                          *  mCVSS;
  const Scalar                       mPtoEnter = (Scalar) 0.995;  // avoid singular variables

public:    
  ValidationWorker(Vector const* y, Matrix const* Xi, Matrix const* X,
		   std::vector<bool>::const_iterator sel, Vector *cvss)
    : mY(y), mXi(Xi), mX(X), mSelector(sel), mSkipped(0), mCVSS(cvss)
    { 
      assert( (mY->size() == mXi->rows()) && (mY->size() == mX->rows()) );
      assert( (1+mX->cols())== (int)mCVSS->size() );   // need room for intial model
    }
  
  ValidationWorker(const ValidationWorker& rhs)
    : mY(rhs.mY), mXi(rhs.mXi), mX(rhs.mX), mSelector(rhs.mSelector), mSkipped(rhs.mSkipped), mCVSS(rhs.mCVSS), mPtoEnter(rhs.mPtoEnter) { }
    
  void operator()()
  {
    ValidatedRegression<Regression> regr("yy", EigenVectorIterator(mY), mSelector, (int)mY->size(), noBlocking, noShrinkage);
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
      std::pair<Scalar,Scalar> fAndP = regr.add_predictors_if_useful(namedIter, mPtoEnter);
      if (fAndP.first == 0) ++mSkipped;  
      (*mCVSS)[k+1] = regr.validation_ss();           // offset by 1 to accomodate first model
    }
  }
  
  int number_skipped() const { return mSkipped; }

};


void
cross_validate_regression(VECTOR    const& Y,
			  MATRIX    const& Xi,      // preconditioning variables
			  MATRIX    const& X,       // variables to add one-at-a-time
			  int nFolds,
			  MATRIX   &results,
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
  debugging::debug("REGR",2) << "Starting main regression thread" << std::endl;
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
    { MATRIX data(X.rows(),2+Xi.cols()+X.cols());
      VECTOR dFold(folds.size());
      for(int i=0; i<(int)folds.size(); ++i) dFold[i] = (SCALAR)folds[i];
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
    debugging::debug("REGR",2) << "Building validation threads" << std::endl;
    std::vector<std::vector<bool>>  selectors (nFolds);
    std::vector<VECTOR *>           cvss      (nFolds);
    std::vector<ValidationWorker *> workers   (nFolds);
    std::vector<std::thread>        validationThreads(nFolds);
    for (int fold=0; fold<nFolds; ++fold)
    { for(int i=0; i<(int)folds.size(); ++i)
	selectors[fold].push_back( folds[i] != fold );
      cvss[fold]    = new VECTOR(1+X.cols());   // +1 for initial model results
      workers[fold] = new ValidationWorker(&Y, &Xi, &X, selectors[fold].begin(), cvss[fold]);
      debugging::debug("REGR",2) << "Starting validation thread #" << fold << std::endl;
      validationThreads[fold] = std::thread(*workers[fold]);
    }
    for (int fold=0; fold<nFolds; ++fold)
    { validationThreads[fold].join();
      totalSkipped += workers[fold]->number_skipped();
    }
    debugging::debug("REGR",3) << "Total number of terms skipped during cross-validation was " << totalSkipped << std::endl;
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
  
