// $Id: variable.test.cc,v 1.11 2003/12/03 16:41:02 bob Exp $
/*
   3 Jan 03 ... New variables.
  12 Dec 02 ... Reopen for new gsl code.
*/

#include <time.h>
#include <iostream>

#include "variable.h"
#include "random.h"
#include "range_ops.h"

RandomGenerator ranGen(12);

double genVal(int column)
{ return column + ranGen.normal(); }

// used this code to read from a stream
//
// std::istream&
// operator>>(std::istream& input, std::vector<Variable>& vec)
// {
//   std::string line;
//   while(std::getline(input,line)) {
//     if (line.length()>3) {   // Need better check for empty stream
//       std::stringstream ss;
//       ss << line;
//       std::cout << "Reading a var from line of length " << line.length() << std::endl;
//       Variable var(ss);
//       std::cout << var << std::endl;
//       vec.push_back(var);
//     }
//   }
//   return input;
// }


int main (void)
{
  int n(10);
  std::vector<double> vY;
  for (int i=0; i<n; ++i)
    vY.push_back(i);
  
  Variable y ("empty", make_anonymous_range(vY));
  std::cout << y ;

  Variable::range x(y);
  std::cout << "Get the range from the variable "
	    << x;
  return 0;

  /*
  clock_t start = clock();
  std::cout << "Sum v1 = "
	    <<  range_ops::accumulate(make_unary_range(v1, data.observation_range()), 0.0)
	    << std::endl;
  std::cout << "Time = " << clock() - start << " ticks" << std::endl;
  start = clock();
  std::cout << "Compiled sum (v1) = " << v1.sum(data.observation_range()) << std::endl;
  std::cout << "Time = " << clock() - start << " ticks" << std::endl;

  std::cout << "Sum v2 = "
	    <<  range_ops::accumulate(make_unary_range(v2, data.observation_range()), 0.0)
	    << std::endl;
  std::cout << "Compiled sum of v2  = " << v2.sum(data.observation_range()) << std::endl;

  start = clock();
  std::cout << "Sum sqr(v1) = "
    	    <<  range_ops::accumulate(make_unary_range(vt1, data.observation_range()), 0.0)
	    << std::endl;
  std::cout << "Time = " << clock() - start << " ticks" << std::endl;
  start = clock();
  std::cout << "Compiled sum sqr (v1) = " << vt1.sum(data.observation_range()) << std::endl;
  std::cout << "Time = " << clock() - start << " ticks" << std::endl;

  start = clock();
  std::cout << "Sum (v1*v2) = "
    	    <<  range_ops::accumulate(make_unary_range(vb1, data.observation_range()), 0.0)
	    << std::endl;
  std::cout << "Time = " << clock() - start << " ticks" << std::endl;
  start = clock();
  std::cout << "Compiled sum (v1+v2) = " << vb1.sum(data.observation_range()) << std::endl;
  std::cout << "Time = " << clock() - start << " ticks" << std::endl;

  std::cout.flush();
  */

}
    
