// $Id: model.h,v 1.8 2003/12/03 16:41:02 bob Exp $

/*
  18 Dec 02 ... Revised with range code in mind, GSL numerics.
   8 Feb 02 ... C++ version inserted
  22 Jul 01 ... Start to add covariance features.
  30 Jun 01 ... Alter structure of gsModel, adding XtX, XtY names
  21 Jun 01 ... Created for new version of sweeper.
*/

#ifndef _MODEL_H_
#define _MODEL_H_ 

#include <vector>
#include <string>

#include "variable.h"
#include "gsl_regr.h"

////////////////////////////////////////////////////////////////////////////////

const int maxRegressionSize (500);

class Model
{
  const Variable         mY;                  // hold vars by value since lightweight
  const std::string      mFilePath;           // where to put files
  std::vector<Variable>  mX;                  // those that are used as predictors in model
  Variable               mLastZ;              // most recent feature evaluated
  double                 mLastZtZ, mLastZtY;
  double*                mLastZtX;
  gslRegression          mRegr;               // gritty numerical details
  
 public:

  typedef std::pair<const double*, const double*> const_range;
  
  Model (Variable y, const Dataset& data, const std::string& filePath)
    :
    mY(y), mFilePath(filePath), mX(),
    mLastZ(y), mLastZtZ(0.0), mLastZtY(0.0), mLastZtX(new double[maxRegressionSize]),
    mRegr(maxRegressionSize, begin(y), end(y))
    { std::clog << "Y in regr model is " << mY << std::endl; }

  size_t number_of_rows() const          { return mData.nRows(); }
  size_t number_of_predictors() const    { return mX.size(); }
  
  double y_bar() const;
  std::vector<double> x_bar() const;
  std::vector<double> beta() const;  // beta[0] is intercept
  
  double TSS() const  {  return mRegr.TotalSS(); }
  double RSS() const  {  return mRegr.ResidualSS(); }
  
  const_range residuals() const;
  const_range fitted_values() const;
  const_range weights() const;

  std::pair<double,double>
    gaussian_predictor_evaluation (Variable x, double xBar) ;  // returns F-stat, p-value; saves Z for later
  std::pair<double,double>
    gaussian_predictor_evaluation (Variable x) { return gaussian_predictor_evaluation (x, average(x,mData)); }

  std::pair<double, double>
    bennett_predictor_evaluation (Variable x, double xBar);
  std::pair<double, double>
    bennett_predictor_evaluation (Variable x) { return bennett_predictor_evaluation(x, average(x,mData)); }

  double
    add_predictor(Variable x, double xBar);    // returns change in RSS
  double
    add_predictor(Variable x)  { return add_predictor(x,average(x,mData)); }    
  
  void
    write_to (std::ostream& os) const;
  void
    print_to (std::ostream& os) const;
  void
    read_from (std::istream& is);
  
 private:
  
  Dataset::weight_range
    sampling_weights() const { return mData.weights(); }

  double*
    predictor_cross_products (Variable z, double zBar) const;

};

std::ostream&
operator<< (std::ostream& os, const Model& regr); 

std::istream&
operator>> (std::istream& is, Model& regr);
// { regr.read_from(is); return is; }

#endif
