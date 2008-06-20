/*  $Id: observation.cc,v 1.3 2003/11/26 16:49:18 bob Exp $
    
  29 Nov 01 ... Created.
 
*/

#include "observation.h"

#include <string>
#include <sstream>

void
Observation::write_to (std::ostream& output) const
{
  output << "(w " << mSampleWeight << ")   " ; // << " [" << mData.size() << "] "
  for(std::vector<double>::const_iterator it=mData.begin(); it != mData.end(); ++ it)
    output << *it << " ";
}

void
Observation::read_from (std::istream& input)
{
  std::string line;

  if(getline(input,line)) {
    std::stringstream ss;
    ss << line;
    ss >> mSampleWeight;
    double x;
    while (ss >> x)
      mData.push_back(x);
  }
}

std::istream&
operator>> (std::istream& input, Observation& obs)
{
  obs.read_from (input); return input;
}

std::ostream&
operator<< (std::ostream& output, const Observation& obs)
{
  obs.write_to (output); return output;
}
