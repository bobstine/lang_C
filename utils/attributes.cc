#include "attributes.h"
#include "read_utils.h"

using std::string;

const string equalStr {"="};
const string commaStr {","};

void
Attributes::set_attribute(std::string name, std::string value)
{
  using read_utils::trim;
  mMap[trim(name)] = trim(value);
}


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


string
Attributes::as_string () const
{
  if (mMap.empty()) return "";
  string result;
  auto it = mMap.begin();
  result = it->first + equalStr + it->second;
  for (++it; it != mMap.end(); ++it) 
    result += commaStr + it->first + equalStr + it->second;
  return result;
}

void
Attributes::print_to (std::ostream& os) const
{
  if (mMap.empty()) return;
  os << as_string();
}
