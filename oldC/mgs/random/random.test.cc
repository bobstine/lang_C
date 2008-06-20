// $Id: random.test.cc,v 1.3 2002/06/17 14:32:50 bob Exp $

#include "random.h"
#include "utility.h"
#include "functor.h"

#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

template <class T>
void test( T f )
{
  vector<double> x(10000);
  generate(x.begin(), x.end(), f);
  // cout << "Random vector " << endl << x << endl;
  cout << "  mean and sd   " << mands(x) << endl << endl;
}

int main (void)
{
  cout << "Making a uniform generator \n";
  MarsagliaGenerator uni(0);
  cout << uni() << endl;

  cout << "Making a random generator\n";
  RandomGenerator rand;
  cout << "Initial state of generator: " << rand << endl;
  
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
  
  cout << "Final state of generator: " << rand << endl;

  return 0;
}
    
