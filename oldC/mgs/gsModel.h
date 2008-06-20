// $Id: gsModel.h,v 1.7 2002/03/13 15:44:26 bob Exp $

/*
   8 Feb 02 ... C++ version inserted
  22 Jul 01 ... Start to add covariance features.
  30 Jun 01 ... Alter structure of gsModel, adding XtX, XtY names
  21 Jun 01 ... Created for new version of sweeper.
*/

#ifndef _GSMODEL_H_
#define _GSMODEL_H_ 

#include <vector>
#include <string>

#include "dataset.h"
#include "variable.h"
#include "statistic.h"
#include "matrix.h"

////////////////////////////////////////////////////////////////////////////////

class GSModel
{
  Variable               mY;
  Dataset&               mData;
  string                 mFilePath;    // where to put files
  vector<Variable>       mX;
  vector<double>         mBeta;        // slopes for Q predictors
  Matrix                 mQ, mQDQ;     // orthogonal predictors (no constant)
  TriangularMatrix       mR;           // lower triangular matrix holding R' of X = Q R
  vector<double>         mError;       // current residuals
  double                 mTSS, mRSS;   // total SS and current residual SS
  double                 mYBar;        // mean of input response
  vector<double>         mXBar;        // means of input predictors
  vector<double>         mEstWts;      // recomputed as vars are added

 public:
  GSModel (Variable y, Dataset& data, string filePath)
    : mY(y), mData(data), mFilePath(filePath),
    mX(), mBeta(), mQ(), mQDQ(), mR(), mError(),
    mTSS(0.0), mRSS(0.0), mYBar(0.0), mXBar(), mEstWts() { initialize(); }

  size_t number_of_rows() const
    { return mData.nRows(); }
  size_t number_of_predictors() const
    { return mX.size(); }

  
  vector<double> residuals() const;
  vector<double> predictions() const;
  vector<double> estimation_weights() const;

  pair<double, double>
    evaluate_predictor (const Variable& var, const double vBar) const;  // returns dRSS, t^2
  pair<double, double>
    evaluate_predictor (const Variable& var) const;

  pair<double, Variable>
    find_best_predictor(const vector<Variable>& vars);

  pair<double,double>
    add_variable(Variable x, double xBar);    // returns <est coefficent, t^2>
  pair<double,double>
    GSModel::add_variable(Variable var)
    { return add_variable(var,average(var,mData)); }    
  
  void
    write_to (ostream& os) const;
  void
    print_to (ostream& os) const;
  void
    read_from (istream& is);
  
 private:
  void initialize();
  void update_QDQ();
  
  vector<double> initial_estimation_weights() const;
  vector<double> current_estimation_weights() const;

  // These assume that means have been removed
  vector<double>                              // coefs that regress Q from input var
    gamma (const Variable& var) const;

  pair<vector<double>,vector<double> > 
    sweep_Q_from_predictor (const Variable& var) const;
  
  pair<double, double> // A and B
    bennett_properties (const double beta, const vector<double>& z, const double prob) const;
      
  binary_iterator<multiplies<double>,DatasetWeightIterator,vector<double>::const_iterator>::range
    weighted_residual_range () const
    {  return make_binary_range(multiplies<double>(), mData.range_weights(), make_const_range(mError) ); }
  
  pair<DatasetWeightIterator, DatasetWeightIterator>
    sampling_weights() const { return mData.range_weights(); }
  
};

ostream&
operator<< (ostream& os, const GSModel& regr); 

istream&
operator>> (istream& is, GSModel& regr);
// { regr.read_from(is); return is; }

#endif
