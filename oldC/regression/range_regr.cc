/* $Id: range_regr.cc,v 1.1 2003/12/03 16:41:02 bob Exp $
   
  14 Mar 03 ... Created.
     
*/

#include "rangeRegression.h"

RangeRegression::const_range
RangeRegression::beta() const 
{
  const double *pB (mRegr.beta);
  return make_range(pB, pB+1+q());
}

  
