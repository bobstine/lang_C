// $Id: fwt.cc,v 1.9 2000/05/05 21:39:42 bob Exp $-*- c++ -*-

#include <iostream>
#include <vector.h>
#include <string>
#include <assert.h>

#include "wavelet.h"
#include "wavecoef.h"
#include "utils.h"
#include "threshold.h"

#define ABS(x) (((x) > 0) ? (x) : (-x))

bool is_power_of_2 (int n)
{
  int j=1;

  assert (n > 0);
  while (n > j)
    j *= 2;
  return (n == j);
}

int main()
{
  // select wavelet basis from input string name
  string basisName;
  cin >> basisName;
  const Wave_coef coefs = Wave_coef::find(basisName);

  int n(0);
  for (cin >> n; n != 0; cin >> n)  // while
  {
    // read associated data values from standard input
    assert (is_power_of_2(n));
    vector<double> data (n);
    for(int j=0; j<n; ++j)
      cin >> data[j];
        
    // make the filter and build decomposition
    Wavelet wavelet(coefs);
    wavelet.decompose(data);
    
    // read threshold method name and initializing parameter
    // negative thresh parms get the coefs associated with abs value threshold
    string threshName;
    double threshParm;
    vector<double> fit;
    for(cin >> threshName; threshName != "done"; cin >> threshName)
    {
      cin >> threshParm;
      
      if (threshName == "coefs")
      {
	fit = wavelet.estimated_coefficient_vector( );
      }
      else if (threshName == "hard")
      {
	HardThreshold thresh(ABS(threshParm));
	if (threshParm >= 0)
	  fit = wavelet.reconstruction( &thresh );
	else
	  fit = wavelet.estimated_coefficient_vector( &thresh );
      }
      else if (threshName == "soft")
      {
	SoftThreshold thresh(ABS(threshParm));
	if (threshParm >= 0)
	  fit = wavelet.reconstruction( &thresh );
	else
	  fit = wavelet.estimated_coefficient_vector ( &thresh );
      }
      else if (threshName == "loop")
      {
	LoopThreshold thresh(ABS(threshParm));
	if (threshParm >= 0)
	  fit = wavelet.reconstruction( &thresh );
	else
	  fit = wavelet.estimated_coefficient_vector ( &thresh );
      }
      else
      {
	cout << "Failing: thresh name = " << threshName
	     << " with parameter = " << threshParm << endl;
	assert(false);
      }
      
      // return the reconstructed fit with simple () parens
      cout << "((fit " << threshName << " " << threshParm << ")" << endl;
      cout << "(";
      for (vector<double>::iterator i = fit.begin(); i!=fit.end(); ++i)
	cout << " " << *i ;
      cout << "))" << endl;
    }
  }
}


////////////////////////////   EOF   //////////////////////////////
