// $Id: featureABC.cc,v 3.1 2008/01/30 03:57:01 bob Exp $

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
FeatureABC::has_attribute(std::string attr)           const
{
  return (mAttributes.end() != mAttributes.find(attr));
}


void
FeatureABC::add_attribute(std::string name, std::string value)
{
  mAttributes[name] = value;
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


std::string
FeatureABC::attribute_str_value(std::string attr) const
{
  AttrIter iter (mAttributes.find(attr));
  if (iter != mAttributes.end())
    return iter->second;
  else
    return "";
}
      

int
FeatureABC::attribute_int_value(std::string attr) const
{
  AttrIter iter (mAttributes.find(attr));
  if (iter != mAttributes.end())
  { std::istringstream ss(iter->second);
    int i;
    ss >> i;
    return i;
  }
  else
    return 0;
}
      

double
FeatureABC::attribute_dbl_value(std::string attr) const
{
  AttrIter iter (mAttributes.find(attr));
  if (iter != mAttributes.end())
  { std::istringstream ss(iter->second);
    double d;
    ss >> d;
    return d;
  }
  else
    return 0.0;
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
    mAttributes[attribute] =  attributeValue;
    --attributeCount;
  }
  is >> mTried;
  is >> mInModel;
  is >> mEntryPValue;
}


//  Output    Output    Output    Output    Output    Output    Output    Output    Output    Output    Output    Output

void
FeatureABC::write_to (std::ostream& os)     const
{
  os << "FeatureABC " << mAttributes.size() << " ";
  if (!mAttributes.empty())
    os << mAttributes; // defined in featureABC.h
  os << mTried << " " << mInModel << " " << mEntryPValue << std::endl;
}


void
FeatureABC::print_to(std::ostream& os) const
{
  os << name() ;
  if (!mAttributes.empty())
    os << mAttributes;
  if (! mTried)
    os << " (not tried).";
  else
  { os << " (tried; ";
    if (!mInModel)
      os << "not ";
    os << "used; p-value " << mEntryPValue << ").";
  }
  os << "  Values " << range();
}


