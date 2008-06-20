/*  $Id: bennett.test.cc,v 1.2 2005/06/14 22:36:21 bob Exp $

    26 Feb 02 ... Created.

*/

#include <iostream>

#include "bennett.h"

using namespace std;

int main(void)
{
  cout << "\nSome p-values, done via two calls...\n";
  cout << "p-val " << bennett_p_value (1.5, 3.0, 1.0) << endl;
  cout << "p-val " << bennett_p_value (1.5, 3.0) << endl;

  cout << "\nSome critical values... \n";
  double a=0.05;
  cout << "c-val .2   " << bennett_critical_value (a, .2 ) << endl;
  cout << "c-val .1   " << bennett_critical_value (a, .1 ) << endl;
  cout << "c-val .05  " << bennett_critical_value (a, .05) << endl;
  cout << "c-val .01  " << bennett_critical_value (a, .01) << endl;
  cout << "c-val .001 " << bennett_critical_value (a, .001) << endl;

  cout << "\nLower bounds for a = " << a << " and b = 1.0 ... \n";
  cout << "p-val " << bennett_p_value     (2.5, a, 1.0) << endl;
  cout << "b_low " << bennett_bound (2.5, a, 1.0, .001) << endl;
  cout << "p-val " << bennett_p_value     (3.5, a, 1.0) << endl;
  cout << "b_low " << bennett_bound (3.5, a, 1.0, .001) << endl;
  cout << "p-val " << bennett_p_value     (4.5, a, 1.0) << endl;
  cout << "b_low " << bennett_bound (4.5, a, 1.0, .001) << endl;
  cout << "p-val " << bennett_p_value     (5.5, a, 1.0) << endl;
  cout << "b_low " << bennett_bound (5.5, a, 1.0, .001) << endl;

  return 0;
}
