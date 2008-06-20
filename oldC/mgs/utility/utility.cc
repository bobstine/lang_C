/*
  $Id: utility.cc,v 1.1.1.1 2002/06/16 15:10:17 bob Exp $
   
*/


#include "utility.h"

#include <iostream>
#include <string>
#include <strstream>
#include <numeric>
#include <vector>
#include <math.h>

///////////////////////////////////////////////////////////////////////////////


istream&
operator>>(istream& input,  vector<double>& vec)
{
  string line;
  if(getline(input,line)) {
    strstream ss;
    ss << line;
    double x;
    while (ss >> x)
      vec.push_back(x);
  }
  return input;
}

namespace {

  class CenterSquare {
    double mCenter;
  public:
    CenterSquare(double center)
      : mCenter(center) { }
    double operator()(double ss, double x)
    { return ss + (x - mCenter)*(x - mCenter); }
  };
}

pair<double,double>
mands (const vector<double>& x)
{
  assert (x.size() > 1);
  double xBar = accumulate(x.begin(), x.end(), 0.0)/x.size();
  double ss = accumulate(x.begin(), x.end(), 0.0, CenterSquare(xBar));
  assert (ss >= 0);
  return make_pair(xBar, sqrt(ss/(x.size()-1)));
}
