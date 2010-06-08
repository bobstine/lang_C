// $Id: anonymous_iterator.test.cc,v 1.21 2007/12/19 01:02:41 bob Exp $    
  
#include "anonymous_iterator.h"
#include "range_stats.h"
#include "range_ops.h"
#include "range.h"

#include <iostream>
#include <vector>
#include <iterator>
#include <list>
#include <string>
#include <algorithm>
#include <utility>
#include <numeric>


class Shifter : public std::unary_function<double,double> {
  double mShift;
public:
  Shifter() { }
  Shifter(double s) : mShift(s) { }
  double operator()(double x) const { return x + mShift; }
};

template <class Iter>
typename std::iterator_traits<Iter>::value_type
print_something (Iter a)
{
  std::cout << *a << std::endl;
  return *a;
}
  

int main()
{
  // First an empty range
  std::cout << "empty anonymous range (" << make_anonymous_range() << ")" << std::endl;
  
  // now with vector of content
  std::vector<double> iz;
  for(int i=0; i<5; ++i)
    iz.push_back(10 * i);

  // use typedefs
  typedef anonymous_iterator_envelope<std::random_access_iterator_tag,double> AnonIter;
  
  AnonIter c = make_anonymous_iterator(iz.begin());
  
  std::cout << *c << std::endl;
  ++c;
  std::cout << *c << std::endl;
  ++c;
  std::cout << *c << std::endl;

  // explicitly make anonymous range from the vector of doubles
  Ranges::range<  AnonIter > aRange (make_anonymous_range(iz));
  std::cout << "TEST: A range is " << aRange << std::endl;
  std::cout << "TEST: Range begins at " << *Ranges::begin(aRange) << std::endl;
  std::cout << "TEST: Print something function returns " ; print_something(begin(aRange)); std::cout << std::endl;
  std::cout << "TEST: Sum of this of anonymous range is " << range_ops::accumulate(aRange,0.0) << std::endl;
  std::cout << "TEST: Shifted is " << make_unary_range(Shifter(3.0),make_anonymous_range(iz.begin(),iz.end())) << std::endl;

  // make an empty vector of them
  // not like this... std::vector<AnonIter> anonVec (3);
  // have to use a back-inserter
  std::vector<AnonIter> anonVec;
  anonVec.push_back(begin(aRange));
  anonVec.push_back(begin(aRange));
  
  // copy one
  AnonIter cpy (begin(aRange));

  // make anonymous range from  a list of doubles
  std::list<double> ix;                           // does this really work with a list ???
  int nx (5);
  for(int i=0; i<nx; ++i)
    ix.push_back(10 * i);

  std::cout << make_anonymous_range(ix.begin(),ix.end()) << std::endl;
  std::cout << "TEST: average "
	    << range_stats::average(make_anonymous_range(ix.begin(),ix.end()), nx) << std::endl;
  std::cout << "TEST: total "
	    << range_ops::accumulate(make_anonymous_range(ix.begin(),ix.end()),0.0) << std::endl;
  std::cout << "TEST: shifted "
  	    << make_unary_range(Shifter(3.0),make_anonymous_range(ix.begin(),ix.end())) << std::endl;
  

  /*
    
  //  now anonymize a range made from a list and a vector
  std::cout << "Anonymized list range ix: "
	    << make_anonymous_range(make_range(ix)) << std::endl;
  std::cout << "Anonymized vector range iz: "
	    << make_anonymous_range(make_range(iz)) << std::endl;

  //  and now anonymize a unary range
  std::cout << "Anonymized shifted list range ix: "
	    << make_anonymous_range(make_unary_range(Shifter(3.0),make_anonymous_range(ix.begin(),ix.end())))
	    << std::endl;
  std::cout << "Anonymized shifted vector range iz: "
	    << make_anonymous_range(make_unary_range(Shifter(40.0),make_anonymous_range(iz.begin(),iz.end())))
	    << std::endl;

  //  and now anonymize a binary range
  std::cout << "Binary range: "
	    << make_anonymous_range(make_binary_range(std::plus<double>(),
						      make_anonymous_range(make_range(ix)),
						      make_anonymous_range(make_range(ix)) ))
	    << std::endl;

  std::cout << "Binary range: "
	    << make_anonymous_range(make_binary_range(std::multiplies<double>(),
						      make_range(ix),make_range(ix)
						      ))
	    << std::endl;

  // Make a list of heterogenous ranges by anonymizing them first
  typedef std::list<range<anonymous_iterator_envelope<std::bidirectional_iterator_tag,double> > >  list_of_ranges;
  list_of_ranges foo;
  foo.push_back(make_anonymous_range(ix));
  // cannot add a range tagged differently
  //  foo.push_back(make_anonymous_range(iz));
  // but can add a range that is defined using the unary functions of the same tag 
  foo.push_back(make_anonymous_range(make_unary_range(Shifter(3.0),make_anonymous_range(ix.begin(),ix.end()))));
  std::cout << make_range(foo) << std::endl;

  */
  
  return 0;
}
