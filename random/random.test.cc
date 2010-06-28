//

#include "random.h"
#include "print_utils.h"
#include "stat_utils.h"
#include "polyshrink.h"
#include "functor.h"

#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

using namespace std; 

template <class T>
void test( T f )
{
  vector<double> x(10000);
  generate(x.begin(), x.end(), f);
  cout << "  mean and sd   " << mands(x) << endl << endl;
}

int
main (void)
{
  // Create the underlying random number generator
  cout << "Making a uniform generator \n";
  MarsagliaGenerator uni(12);
  cout << uni() << endl;
  cout << "Making a random generator\n";
  RandomGenerator rand(3);
  cout << "Initial state of generator: " << rand << endl;
  // Use the generator to make samples from various populations
  cout << "Uniform:\n";
  test(member_as_operator(rand, &RandomGenerator::uniform));
  cout << "Normal:\n";
  test(member_as_operator(rand, &RandomGenerator::normal));
  cout << "Cauchy:\n";
  test(member_as_operator(rand, &RandomGenerator::cauchy));
  cout << "Gamma(4.4):\n";
  test(member_as_operator1(rand, &RandomGenerator::gamma, 4.4));
  cout << "Chi-square(2):\n";
  test(member_as_operator1(rand, &RandomGenerator::chi_square, 2));
  // Do a some estimation
  const int    n  (40);
  const double mu (2.2);
  std::vector<double> y    (n);
  std::vector<double> yHat (n);
  generate(y.begin(), y.end(), member_as_operator(rand, &RandomGenerator::normal));
  transform(y.begin(), y.end(), y.begin(), std::bind1st(std::plus<double>(), mu));
  std::cout << "Raw estimates: " << y << std::endl;
  std::cout << "SD of raw: " << standard_deviation(y,mu) << std::endl;
  polyshrink(y, yHat);
  std::cout << "Polyshrink estimates: " << yHat << std::endl;
  std::cout << "SD of estimates: " << standard_deviation(yHat,mu) << std::endl;  
  
  cout << "Final state of generator: " << rand << endl;

  return 0;
}
    
