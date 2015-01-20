#ifndef __build_helper__
#define __build_helper__

#include "auction.h"
#include "regression.h"


class FiniteCauchyShare
{
private:
  double mTotalAlpha;
  int    mCount;
  double mSum;

public:
  FiniteCauchyShare (double alpha, int count) :  mTotalAlpha(alpha), mCount(count), mSum(0.0)
    { init(); }

  double operator()(int j) const;
  
 private:
  void init();
  double p(int j) const;
};


ValidatedRegression
build_regression_model(Column y, Column inOut, int prefixRows, int blockSize, bool useShrinkage, std::ostream& os);

void
add_source_experts_to_auction (std::vector<Column> const& xColumns, int prefixCases, int contextCases, double wealth, Auction<ValidatedRegression> &auction);

double
time_since(time_t const& start);


std::pair< std::pair<int,double>, std::pair<int,double> >
initialize_sums_of_squares(std::vector<Column> y);



#endif
