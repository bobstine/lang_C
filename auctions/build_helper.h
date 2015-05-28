#ifndef _BUILD_HELPER_H_
#define _BUILD_HELPER_H_

#include "auction_base_types.h"
#include "auction.h"
#include "validated_regression.h"

class FiniteCauchyShare
{
private:
  SCALAR mTotalAlpha;
  int    mCount;
  SCALAR mSum;

public:
  FiniteCauchyShare (SCALAR alpha, int count) :  mTotalAlpha(alpha), mCount(count), mSum(0.0)
    { init(); }

  SCALAR operator()(int j) const;
  
 private:
  void init();
  SCALAR p(int j) const;
};

//  -------------------------------------------------------------------------------------
//          Define regression type
typedef  ValidatedRegression<FastLinearRegression>  Regression;
//  -------------------------------------------------------------------------------------


Regression
build_regression_model(Column<SCALAR> y, Column<SCALAR> inOut, Column<SCALAR> wts, int prefixRows, int blockSize, bool useShrinkage, std::ostream& os);

Regression
build_regression_model(Column<SCALAR> y, Column<SCALAR> inOut, int prefixRows, int blockSize, bool useShrinkage, std::ostream& os)
{
  Column<SCALAR> wts("wts","none",0);
  return build_regression_model(y, inOut, wts, prefixRows, blockSize, useShrinkage, os);
}

int
parse_column_format(std::string const& dataFileName, std::ostream&);

Column<SCALAR>
identify_cv_indicator(std::vector<Column<SCALAR>> const& columns);

void
round_elements_into_vector(Column<SCALAR> const& c, std::vector<int>::iterator b); 

void
add_source_experts_to_auction (FeatureSource const& src, int contextCases, SCALAR wealth,
			       std::vector<FeatureVector> &featureStreams,
			       Auction<ValidatedRegression<FastLinearRegression>> &auction);

SCALAR
time_since(time_t const& start);


std::pair< std::pair<int,SCALAR>, std::pair<int,SCALAR> >
initialize_sums_of_squares(std::vector<Column<SCALAR>> y);

#endif
