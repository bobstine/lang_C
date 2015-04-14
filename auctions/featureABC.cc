#include "featureABC.h"

#include "debug.h"
#include "string_trim.h"
#include "range_stats.h"

FeatureABC::Arguments
FeatureABC::join_arguments(FeatureABC::Arguments const& a1, FeatureABC::Arguments const& a2) const
{
  Arguments args = a1;
  for (Arguments::const_iterator it = a2.begin(); it != a2.end(); ++it)
    args[it->first] += it->second;
  return args;
}

//  Attributes  Attributes  Attributes  Attributes  Attributes  Attributes  Attributes  Attributes  Attributes  Attributes

bool
FeatureABC::has_attribute(std::string attrName)           const
{
  Attributes::const_iterator it (mAttributes.find(attrName));
  return ((mAttributes.end() != it) && (it->second.size() > 0));
}

void
FeatureABC::set_attribute(std::string name, std::string value)
{
  name = trim(name);
  mAttributes[name]=trim(value);
}

const std::string equalStr {"="};
const std::string commaStr {","};

void
FeatureABC::add_attributes_from_descriptive_string (std::string line)
{
  using std::string;
  
  std::cout << "TESTING: adding attributes from comma delimited paired assignment list:  " << line << std::endl;
  size_t pos0 = 0, pos1 = 0;
  while (true)
  { pos1 = line.find(equalStr, pos0);
    if(pos1 == std::string::npos) break;            // not found
    string name = line.substr(pos0,pos1-pos0);
    pos0 = pos1+1;
    pos1 = line.find(commaStr, pos0);
    if(pos1 == std::string::npos) pos1=line.size(); // no more options
    string value = line.substr(pos0,pos1-pos0);
    std::cout << "TESTING: name = " << name << "  value = " << value << std::endl;
    set_attribute(name, value);
    pos0 = pos1+1;
  }
  std::cout << "TESTING: attributes added are " << attributes() << std::endl;
}


std::string
FeatureABC::attribute_str_value(std::string attr) const
{
  for(Attributes::const_iterator it=mAttributes.begin(); it != mAttributes.end(); ++it)
  { if (it->first == attr)
      return it->second;
  }
  return std::string();
}
      

int
FeatureABC::attribute_int_value(std::string attr) const
{
  return std::stoi(attribute_str_value(attr));
}
      


FeatureABC::Scalar
FeatureABC::attribute_scalar_value(std::string attr) const
{
  return (Scalar)(std::stod(attribute_str_value(attr)));
}



bool
FeatureABC::is_dummy() const
{
  return false;
}


void
FeatureABC::read_from (std::istream& is)
{
  std::string prefix;
  is >> prefix;
  if (prefix != "FeatureABC")
  { std::cout << "FABC: Read prefix did not match FeatureABC, got " << prefix << " instead.\n";
    return;
  }
  int attributeCount (0);
  is >> attributeCount;
  while (attributeCount)
  { std::string attribute;
    std::string attributeValue;
    is >> attribute;
    is >> attributeValue;
    set_attribute(attribute,attributeValue);
    --attributeCount;
  }
  is >> mTried;
  is >> mInModel;
  is >> mEntryBid;
}


//  Output    Output    Output    Output    Output    Output    Output    Output    Output    Output    Output    Output

void
FeatureABC::write_to (std::ostream& os)     const
{
  os << "FeatureABC " << mAttributes.size() << " ";
  if (!mAttributes.empty())
    for (Attributes::const_iterator pA = mAttributes.begin(); pA != mAttributes.end(); ++pA)
      os << " [" << pA->first << " (" << pA->second << ")] ";
  os << "  tried=" << mTried << "  inModel=" << mInModel << "  bid=" << mEntryBid;
}


void
FeatureABC::write_values_to(std::ostream& os) const
{
  std::copy(begin(), end(), std::ostream_iterator<Scalar>(os," "));
}


void
FeatureABC::print_to(std::ostream& os) const
{
  os << name() ;
  if (!mAttributes.empty())
    for (Attributes::const_iterator pA = mAttributes.begin(); pA != mAttributes.end(); ++pA)
      os << " [" << pA->first << " (" << pA->second << ")] ";
  if (! mTried)
    os << " (not tried).";
  else
  { os << " (tried; ";
    if (!mInModel)
      os << "not ";
    os << "used; bid " << mEntryBid << ").";
  }
  os << "  Values " << range();
}


