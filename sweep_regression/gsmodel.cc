/* $Id: gsmodel.cc,v 1.1 2005/06/13 20:47:51 bob Exp $
   
     8 Feb 02 ... Implementation begun
     
*/

#include "gsmodel.h"

#include "statistic.h"
#include "statistic_base.h"
#include "utility.h"
#include "bennett.h"
#include "matrix.h"
#include "range.h"
#include "compose.h"

#include <algorithm>
#include <numeric>
#include <utility>
#include <assert.h>

////  GS Model  ////

void
GSModel::print_to (ostream& os) const
{
  os << endl << " ____ GS Model Summary ____  \n";
  os << "  nObs =  " << mData.nRows() << "      sum wts = " << mData.sum_weights() << endl;
  os << "  Y       " << mY << " avg = " << mYBar << "  TSS = " << mTSS << endl;
  os << "  X       " << mX;  // vector print adds eol
  os << "  Xbar    " << mXBar; 
  os << "  Beta(q) " << mBeta;
  os << "  Q dim   " ; print_pair(cout, mQ.dimension()); cout << endl;
  os << "  RSS     " << mRSS << endl;
  os << "  Resids    (0-3)  " << make_short_range(mError,4);
  os << " -----------------------------------\n";
  os << " R matrix: \n " << mR << endl;
  os << " -----------------------------------\n";
}  

ostream&
operator<< (ostream& os, const GSModel& regr)
{ regr.print_to(os); return os; }


////  Accessors  ////

vector<double>
GSModel::residuals() const
{
  vector<double> res(mError);
  return res;
}
  
vector<double>
GSModel::predictions() const
{
  vector<double> preds(mError.size());
  transform (make_unary_range(mY,mData.range()), make_range(mError), make_range(preds),
	     minus<double>());
  return preds;
}

vector<double>
GSModel::estimation_weights() const
{
  vector<double> wts(mEstWts);
  return wts;
}


////  Calculation  ////

void
GSModel::initialize()
{
  mYBar   = average(mY, mData);
  mError  = centered_realization(mY, mYBar, mData);
  mTSS    = sum_of_squares (mY, mYBar, mData);
  mRSS    = mTSS;
  mEstWts = initial_estimation_weights();
}

vector<double>
GSModel::initial_estimation_weights() const
{
  vector<double> mean(mError.size(), mYBar);
  return mean;
}

vector<double>
GSModel::current_estimation_weights() const
{
  vector<double> preds = predictions();
  transform(preds.begin(), preds.end(), preds.begin(),
	    Function::BinomialVariance());
  return preds;
}


vector<double>
GSModel::gamma (const Variable& var) const
{
  size_t q = number_of_predictors();
  assert (q > 0);
  vector<double> g = matrix_product(mQ.row_range(), make_unary_range(var,mData.range()));
  transform (g.begin(), g.end(), mR.begin_diagonal(), g.begin(), divides<double>());
  return g;
}

namespace {
  class Dot : public unary_function<const vector<double>&,double> {
    const vector<double>& mV;
  public:
    Dot(const vector<double>& v)
      : mV(v) { }
    double operator()(const vector<double>& x) const
    { return inner_product(mV.begin(),mV.end(),x.begin(),0.0); }
  };
}

pair<vector<double>,vector<double> > // gamma and residuals
GSModel::sweep_Q_from_predictor (const Variable& var) const
{
  vector<double> z = vector<double>(number_of_rows());
  vector<double> g = gamma(var);
  transform(mQ.begin(), mQ.end(), z.begin(), Dot(g));
  return make_pair(g,z);
}

pair<double, double>
GSModel::evaluate_predictor (const Variable& var, const double vBar) const
{
  Variable centeredVar(var,vBar);
  pair<vector<double>,vector<double> > results = sweep_Q_from_predictor(centeredVar);
  cout << "Gamma test" << results.first;
  cout << "SSz       "
       << weighted_inner_product(make_range(results.second), make_range(results.second),
				 mData.range_weights(), 0.0);
  return make_pair(1.0,2.0);
}

pair<double, double>
GSModel::evaluate_predictor (const Variable& var) const
{ return evaluate_predictor(var, average(var,mData)); }


pair<double, Variable>
GSModel::find_best_predictor(const vector<Variable>& vars)
{
  // compute average (weighted) of each possible predictor
  vector<double> avgs = average(vars, mData);
  // form centered variables
  vector<Variable> centeredVars (vars.size());
  transform(vars.begin(), vars.end(), avgs.begin(), centeredVars.begin(),
	    make_centered_variable );
  // accumulate e'Z vector
  vector<double> init (vars.size(),0.0);
  vector<double> epZ = accumulate_range (make_weighted_row_range(make_range(centeredVars),
								 mData.range(),
								 weighted_residual_range()),
					 make_range(init));
  // compute weighted SS in Z'DZ
  vector<double> zdz = weighted_sum_of_squares (centeredVars, mData, mEstWts);
			     
  map<double,Variable> resultMap;
  resultMap.insert( make_pair(1.0, *vars.begin()) );
  return *resultMap.begin();
}

pair<double, double>  // A and B, assuming predictor has been centered and normalized
GSModel::bennett_properties (const double beta, const vector<double>& z, const double prob) const
{
  double a = *max_element (make_unary_range(Function::AbsValue(), make_const_range(z)));
  double b2 = weighted_inner_product(make_unary_range(Function::Square(), make_const_range(z)),
				     make_const_range(mEstWts),
				     sampling_weights(), 0.0);
  assert (b2 >= 0.0);
  double b = sqrt(b2);
  cout << "Bennett: a = " << a << "   b = " << b << endl;
  return make_pair(bennett_p_value(beta,a,b), bennett_lower_bound(beta, a, b, prob));
}

pair<double,double>
GSModel::add_variable(Variable var, double avg)
{
  unsigned q = number_of_predictors();
  pair<DatasetWeightIterator, DatasetWeightIterator> wts = sampling_weights();
  // center input predictor
  mXBar.push_back(avg);
  vector<double> z = centered_realization(var, avg, mData);
  // sweep each col of Q from input
  vector<double> r(q+1);
  Matrix Qt = transpose(mQ);
  for (unsigned k=0; k<q; ++k) {
    r[k] = weighted_inner_product(make_range(Qt[k]), make_range(z), wts, 0.0);  // z'q_k
    transform (make_range(z),
	       make_unary_range(bind2nd(multiplies<double>(),r[k]), make_range(Qt[k])),
	       make_range(z), minus<double>());  // z - (z'q_k) q_k
  }
  // put remaining SS into R on diagonal
  r[q] = weighted_sum_of_squares(make_range(z), wts);
  // normalize new predictor before adding to Q
  assert (r[q] > 0.0);
  transform(z.begin(), z.end(), z.begin(), bind2nd(divides<double>(), sqrt(r[q])));
  // find slope, save it, get bennett properties
  double beta = weighted_inner_product(make_range(z), make_range(mError), wts, 0.0);
  mBeta.push_back(beta);
  pair<double,double> pvalue_and_bound = bennett_properties (beta,z,.001);
  cout << " predictor properties a and b: " << pvalue_and_bound;
  // sweep effect from mError, e = e - g q, RSS
  transform(make_range(mError),
	    make_unary_range(bind2nd(multiplies<double>(),beta), make_range(z)),
	    make_range(mError),
	    minus<double>());
  mRSS = accumulate(make_weighted_unary_range(Function::Square(),make_range(mError),wts),0.0);
  // augment g, X, Q and R
  mX.push_back(var);
  mQ = mQ | z;
  mQDQ = matrix_cross_product(mQ.row_range(), sampling_weights()); 
  mR.append(r);
  mEstWts = current_estimation_weights();
  // done
  return make_pair(beta,0.0);
}

  
