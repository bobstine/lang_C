/*  $Id: observation.cc,v 1.2 2002/01/08 22:23:09 bob Exp $
    
  29 Nov 01 ... Created.
 
*/

#include "observation.h"
#include <string>
#include <strstream>

void
Observation::write_to (ostream& output) const
{
  output << mSampleWeight << "   " ; // << " [" << mData.size() << "] "
  for(vector<double>::const_iterator it=mData.begin(); it != mData.end(); ++ it)
    output << *it << " ";
}

void
Observation::read_from (istream& input)
{
  string line;

  if(getline(input,line)) {
    strstream ss;
    ss << line;
    ss >> mSampleWeight;
    double x;
    while (ss >> x)
      mData.push_back(x);
  }
}

istream&
operator>> (istream& input, Observation& obs)
{
  obs.read_from (input); return input;
}

ostream&
operator<< (ostream& output, const Observation& obs)
{
  obs.write_to (output); return output;
}
