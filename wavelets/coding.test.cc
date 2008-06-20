// $Id: coding.test.cc,v 1.2 2000/04/21 16:41:28 bob Exp $-*- c++ -*-

#include <iostream.h>

#include <vector.h>
#include <map.h>
#include <algo.h>
#include <math.h>

#include "utils.h"
#include "coding.h"

/////////////////////////////////////////////////////

			 
main()
{

  cout << endl << endl
       << " -----------------  Start of coding test  -----------------"
       << endl << endl;
  
  cout << "Code length for 1 = " << monotone_universal_length(1) << endl;
  cout << "Code length for 2 = " << monotone_universal_length(2) << endl;
  cout << "Code length for 5 = " << monotone_universal_length(5) << endl;
  

  double sum;
  for (int i=1; i < 100000; ++i)
    sum += pow(2.0, ( - monotone_universal_length(i) ));
  cout << endl << "Sum of implied probabilities is " << sum << endl;
  

}

/*
  bool b[10] = {1,1,1,1,1,0,0,0,0,0};
  vector<bool> bits(b, b+10);
  cout << "Compressed bit length is " << compressed_bit_length(bits) << endl
  << " from parameter " << (0.5 * log_2(10.)) 
  << " and entropy contribution " << entropy(10, 5.0/10) << endl;
*/

/*
  cout << "Univ for  0 = " << universal_bit_length(0) << universal_bits( 0);
  cout << "     for  1 = " << universal_bit_length(1) << universal_bits( 1);
  cout << "     for  2 = " << universal_bit_length(2) << universal_bits( 2);
  cout << "     for  3 = " << universal_bit_length(3) << universal_bits( 3);
  cout << "     for  4 = " << universal_bit_length(4) << universal_bits( 4);
  cout << "     for  6 = " << universal_bit_length(6) << universal_bits( 6);
  cout << "     for 15 = " << universal_bit_length(15)<< universal_bits(15);
*/

/*
  cout << "Bits for  0 = " << binary_bit_length( 0) << binary_bits( 0);
  cout << "     for  8 = " << binary_bit_length( 8) << binary_bits( 8);
  cout << "     for 27 = " << binary_bit_length(27) << binary_bits(27);
*/

/*
  cout << "Cauchy for  0 = " << cauchy_bit_length(0) << cauchy_bits( 0);
  cout << "       for  8 = " << cauchy_bit_length(8) << cauchy_bits( 8);
  cout << "       for 27 = " << cauchy_bit_length(27)<< cauchy_bits(27);
*/

/*
  cout << "Geometric for  0 = " << geometric_bit_length(0) << geometric_bits( 0);
  cout << "          for  8 = " << geometric_bit_length(8) << geometric_bits( 8);
  cout << "          for 27 = " << geometric_bit_length(27)<< geometric_bits(27);
*/






