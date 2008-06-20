// $Id: encoder.test.cc,v 1.3 2000/04/25 15:35:55 bob Exp $-*- c++ -*-

#include <iostream.h>

#include <vector.h>
#include <math.h>
#include <algo.h>

#include "utils.h"
#include "encoder.h"

/////////////////////////////////////////////////////


namespace
{
  class Printer
  {
  public:
    Printer(const Encoder &coder):
      mCoder(coder)
      {
	mCoder.print();
      }
    void operator()(double coef) const
      {
	pair<double, int> codePair = mCoder.encode_as_pair(coef);
	cout << "Coding " << coef << "  as " << codePair.first
	     << " requires " << mCoder.bits_to_code(coef) << " bits  "
	     << "using loop number " << codePair.second << endl;
      }
  private:
    Encoder mCoder;
  };
}

	  
main()
{

  cout << endl << endl
       << " -----------------  Start of encoder test  -----------------"
       << endl << endl;
  
  double c[20] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		  2.2, -2.9, 4.2, 6.2, 26.5, 27.2, 28.2, -27.2, -28.2};
  vector <double> coefs(c,c+20);
  
  cout << "Best code for these is " << endl
       << encode_vector(coefs);

  Encoder loopie(0.1);
  for_each(coefs.begin(), coefs.end(), Printer(loopie));
  
  cout << "Total bits for this vector are "
       << loopie.bits_to_code(coefs) << endl;

  Encoder loop2(0.05);
  cout << "Using second loop coder with p=0.05 requires "
       << loop2.bits_to_code(coefs) << endl;
  
}

/////////////////////// eof /////////////////////////


