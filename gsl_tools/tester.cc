/*
 *  tester.cc
 *  seq_regr
 *
 *  Created by Robert Stine on 11/26/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include <iostream>

// Two types of dot product

class Dot
{
public:
  double dot(double a, double b) const { return a * b; }
}; 


class WeightedDot
{
private:
  double mWeight;
public:
  WeightedDot(double w) : mWeight(w) { }
  double dot(double a, double b) const { return a * b * mWeight; }
};

// Combine them in a class as policy

template<class D>
class Tester: public D 
{
public:
  Tester () { }
  Tester (double w) : WeightedDot(w) { }
  double dot_product(double a, double b){return D::dot(a,b);};
};

/*
 Main
*/

int
main(int argc, char** argv)
{
  Tester<Dot> mine;
  std::cout << " Dot: Dot of numbers = " << mine.dot_product(10.,10.) << std::endl;
  
  Tester<WeightedDot> mine2(10.0);
  std::cout << "WDot: Dot of numbers = " << mine2.dot_product(10.,10.) << std::endl;
}