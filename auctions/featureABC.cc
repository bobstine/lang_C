#include "featureABC.h"

#include "range_stats.h"

#include <sstream>

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
  Attributes attr (attributes());
  return (attr.end() != attr.find(attrName));
}


void
FeatureABC::add_attribute(std::string name, std::string value)
{
  mAttributes[name].insert(value);
}


void
FeatureABC::add_attributes_from_paired_list (std::string list)
{
  std::istringstream iss (list);
  iss >> std::ws;
  while (!iss.eof())
  { std::string name;
    std::string value;
    iss >> name >> value;
    add_attribute(name, value);
  }
}


std::set<std::string>
FeatureABC::attribute_str_value(std::string attr) const
{
  for(AttrIter it=mAttributes.begin(); it != mAttributes.end(); ++it)
    if (it->first == attr)
      return it->second;
  std::set<std::string> s;
  return s;
}
      

std::set<int>
FeatureABC::attribute_int_value(std::string attr) const
{
  std::set<int> result;
  std::set<std::string> strs (attribute_str_value(attr));
  for (std::set<std::string>::const_iterator it=strs.begin(); it!=strs.end(); ++it)
  { std::istringstream ss(*it);
    int value;
    ss >> value;
    result.insert(value);
  }
  return result;
}
      


std::set<double>
FeatureABC::attribute_dbl_value(std::string attr) const
{
  std::set<double> result;
  std::set<std::string> strs (attribute_str_value(attr));
  for (std::set<std::string>::const_iterator it=strs.begin(); it!=strs.end(); ++it)
  { std::istringstream ss(*it);
    double value;
    ss >> value;
    result.insert(value);
  }
  return result;
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
    mAttributes[attribute].insert(attributeValue);
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
    for (AttrIter pA = mAttributes.begin(); pA != mAttributes.end(); ++pA)
      os << " [" << pA->first << " (" << pA->second << ")] ";
  os << "  tried=" << mTried << "  inModel=" << mInModel << "  bid=" << mEntryBid << std::endl;
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
    for (AttrIter pA = mAttributes.begin(); pA != mAttributes.end(); ++pA)
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


