#include "range_stats.h"

#include "print_utils.h"

#include <vector>


const int n(10);
const int p( 3);

using namespace range_stats;

int main()
{

  // define data and wts
  
  std::vector<double> X, Y, W;
  for(int i = 1; i <= n; ++i)
    {
      X.push_back(i);
      Y.push_back(i*i);
      W.push_back(1.0);   // set all wts to 1.0
    }
  
  // raw and weighted averages, SD
  
  double xBar = average(Ranges::make_range(X),n);  double xSD = standard_deviation(Ranges::make_range(X), xBar, n-1);
  double yBar = average(Ranges::make_range(Y),n);  double ySD = standard_deviation(Ranges::make_range(Y), yBar, n-1);
  std::cout << "Raw     : avg(X)=" << xBar << "   avg(Y)=" << yBar << std::endl;
  std::cout << "Raw     :  sd(X)=" << xSD  << "    sd(Y)=" << ySD  << std::endl;

  double sumW  = range_ops::accumulate(Ranges::make_range(W), 0.0);
  double wxBar = weighted_average(Ranges::make_range(X),sumW, Ranges::make_range(W));
  double wxSD  = weighted_standard_deviation(Ranges::make_range(X), wxBar, sumW-1.0, Ranges::make_range(W));
  double wyBar = weighted_average(Ranges::make_range(Y),sumW, Ranges::make_range(W));
  double wySD  = weighted_standard_deviation(Ranges::make_range(Y), wyBar, sumW-1.0, Ranges::make_range(W));
  std::cout << "Weighted: avg(X)=" << wxBar << "   avg(Y)=" << wyBar << std::endl;
  std::cout << "Weighted:  sd(X)=" << wxSD  << "    sd(Y)=" << wySD  << std::endl;

  // covariances (centers if give it a mean)

  double cov = covariance(Ranges::make_range(X), xBar, Ranges::make_range(Y), yBar, n);
  double wCov= weighted_covariance(Ranges::make_range(X), xBar, Ranges::make_range(Y), yBar, n, Ranges::make_range(W));
  std::cout << "Cov(X,Y) = " << cov << " wCov(X,Y) = " << wCov << std::endl;

  // cross-product

  std::cout << std::endl
	    << "Cross-product = " << cross_product(Ranges::make_range(Y), 0.0, Ranges::make_range(X),  0.0, n)
	    << std::endl;

  // vector cross product
  
  std::vector< std::vector<double> > xMat(p);
  for (int j=0; j < p; ++j)
  { std::vector<double> x(10);
    for (int i=0; i < n; ++i) x[i] = i+j;
    xMat[j] = x;
  }
  std::vector<double> xAverages(p);
  for (int j=0; j < p; ++j)
  { 
    xAverages[j] = average(Ranges::make_range(xMat[j]), n);
  }

  std::cout << "  first col of x is " << Ranges::make_range(xMat[0]) << std::endl;
  std::cout << "  by hand, cp 1 is " << cross_product(Ranges::make_range(Y), 0.0, Ranges::make_range(xMat[0]),  0.0, n)
	    << std::endl;
  std::vector<double> cp(3);
  fill(cp.begin(), cp.end(), 0.0);
  std::cout << "  cp zeroed    " << cp << std::endl;

  fill_cross_product_vector(Y, 0.0, xMat, xAverages, cp.begin());

  std::cout << "  cp filled    " << cp << std::endl;
  
  return 0;
}
