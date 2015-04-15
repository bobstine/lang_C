#include "attributes.h"
#include "read_utils.h"

using std::string;

const string equalStr {"="};
const string commaStr {","};

void
Attributes::parse_attributes_from_string (string const& line)
{ 
  size_t pos0 = 0, pos1 = 0;
  while (true)
  { pos1 = line.find(equalStr, pos0);
    if(pos1 == std::string::npos) break;            // not found
    string name = read_utils::trim(line.substr(pos0,pos1-pos0));
    pos0 = pos1+1;
    pos1 = line.find(commaStr, pos0);
    if(pos1 == std::string::npos) pos1=line.size(); // no more options
    string value = read_utils::trim(line.substr(pos0,pos1-pos0));
    mMap[name]=value;
    pos0 = pos1+1;
  }
}



void
Attributes::print_to (std::ostream& os) const
{
  if (mMap.empty()) return;
  auto it = mMap.begin();
  os << it->first << "=" << it->second;
  for (++it; it != mMap.end(); ++it) 
    os << ", " << it->first << "=" << it->second;
}
