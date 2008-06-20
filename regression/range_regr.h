// $Id: range_regr.h,v 1.1 2003/12/03 16:41:02 bob Exp $

/*
  This code handles regression as a collection of ranges.  It expects
  all input data to come in the form of a range.  So, if you want to
  use 'Variable' objects, just make sure that they implement the range
  semantics (begin,end).

  Basically, this code sits over the "real" regression implemented
  using the gsl numerics code.  After all, you really do not want to
  "store" a range, since they are of heterogenous types.

  14 Mar 03 ... Created.
*/

#ifndef _RANGE_REGRESSION_H_
#define _RANGE_REGRESSION_H_ 

#include <iostreams>

#include "gsl_regr.h"
#include "range_traits.h"
#include "range.h"
#include "range_ops.h"
#include "range_stats.h"

////////////////////////////////////////////////////////////////////////////////

const int maxRangeRegressionSize 200;

class RangeRegression
{
  gslRegression          mRegr;
  
 public:

  typedef std::pair<const double*, const double*> const_range;
  
  template<class Range>
  RangeRegression (Range y)
    :
    mRegr(maxRangeRegressionSize,
	  length(y), begin(y), average(y, length(y)), sum_of_squares(y, average(y,length(y))))
    { }

  size_t n() const          { return mRegr.N(); }
 
  size_t q() const          { return mRegr.Q(); }
  
  const_range beta() const;      // beta[0] is intercept

  
  double TSS() const        {  return mRegr.TotalSS(); }
  double RSS() const        {  return mRegr.ResidualSS(); }
  
  const_range residuals() const
                            {  const double* pR (mRegr.Residuals());
			       return make_range(pR, pR+n());
			    }
  const_range fitted_values() const
                            { const double* pF (mRegr.Fit());
			      return make_range(pF, pF+n());
			    }

  template <class Range>
    bool
    evaluate_predictor(Range r)
    { double avg (average(r, length(r)));
      std::pair<double,double> eval (gaussian_predictor_evaluation (r,avg));
      std::clog << "RR: Gaussian evaluation summary " << eval << std::endl;
      if (eval.second > 0.05)
	return 0;
      else
      { eval = bennett_predictor_evaluation (r, avg);
	std::clog << "RR: Bennett evaluation summary " << eval << std::endl;
	if (eval.second > 0.05)
	  return 0;
	else
	{ std::clog << "RR: predictor added, RSS = " << add_predictor (r, avg);
	  return 1;
	}
      }
    }
  
 private:  

  template <class Range>
    std::pair<double,double>
    gaussian_predictor_evaluation (Range r, double avg);

  template <class Range>
    std::pair<double,double>
    bennett_predictor_evaluation (Range r, double avg);

  template <class Range>
    double      
    add_predictor (Range r, double avg)       // returns new RSS
    {
    };

  
  void
    write_to (std::ostream& os) const;
  void
    print_to (std::ostream& os) const;
  void
    read_from (std::istream& is);
  

};

std::ostream&
operator<< (std::ostream& os, const RangeRegression& regr); 

std::istream&
operator>> (std::istream& is, RangeRegression& regr);

#endif
