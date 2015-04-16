#include "featureABC.h"

#include "debug.h"
#include "read_utils.h"
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
  os << name() << "  FeatureABC [attr: " << mAttributes << "]  tried=" << mTried << ", inModel=" << mInModel << ",  bid=" << mEntryBid;
}


void
FeatureABC::write_values_to(std::ostream& os) const
{
  std::copy(begin(), end(), std::ostream_iterator<Scalar>(os," "));
}


void
FeatureABC::print_to(std::ostream& os) const
{
  os << name() << " [attr: " << mAttributes << "] ";
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


