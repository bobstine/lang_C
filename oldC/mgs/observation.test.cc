// $Id: observation.test.cc,v 1.1 2001/12/18 13:21:34 bob Exp $

#include "observation.h"

int main (void)
{
  Observation obs1(cin);
  cout << obs1 << endl;

  Observation obs2;
  cin >> obs2;
  cout << obs2 << endl;

  return 0;
}
    
