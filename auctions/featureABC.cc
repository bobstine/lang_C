#include "featureABC.h"

#include "range_stats.h"

#include <sstream>

// trim string
#include <boost/algorithm/string.hpp>  


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
  //  std::cout << "TESTING: Adding attribute " << name << " with value " << value << std::endl;
  mAttributes[name]=value;
}


void
FeatureABC::add_attributes_from_paired_list (std::string list)
{
  // std::cout << "TESTING: adding attributes from space-delimited paired list " << list << std::endl;
  boost::algorithm::trim(list);
  std::istringstream iss (list);
  iss >> std::ws;
  while (!iss.eof())
  { std::string name;
    std::string value;
    iss >> name >> value;
    set_attribute(name, value);
  }
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
  std::istringstream ss( attribute_str_value(attr) );
  int value;
  ss >> value;
  return value;
}
      


double
FeatureABC::attribute_dbl_value(std::string attr) const
{
  std::istringstream ss( attribute_str_value(attr) );
  double value;
  ss >> value;
  return value;
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
  { std::cout << "FETR: Read prefix did not match FeatureABC, got " << prefix << " instead.\n";
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
  std::copy(begin(), end(), std::ostream_iterator<double>(os," "));
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


