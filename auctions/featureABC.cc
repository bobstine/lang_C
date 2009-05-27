// $Id: featureABC.cc,v 3.1 2008/01/30 03:57:01 bob Exp $

#include "featureABC.h"

#include "range_stats.h"


FeatureABC::Arguments
FeatureABC::join_arguments(FeatureABC::Arguments const& a1, FeatureABC::Arguments const& a2) const
{
  Arguments args = a1;
  for (Arguments::const_iterator it = a2.begin(); it != a2.end(); ++it)
    args[it->first] += it->second;
  return args;
}


bool
FeatureABC::has_attribute(std::string const& a)       const
{
  return (mAttributes.end() != std::find(mAttributes.begin(), mAttributes.end(), a));
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
    is >> attribute;
    mAttributes.insert(attribute);
    --attributeCount;
  }
  is >> mTried;
  is >> mInModel;
  is >> mEntryPValue;
}

void
FeatureABC::write_to (std::ostream& os)     const
{
  os << "FeatureABC " << mAttributes.size() << " ";
  if (!mAttributes.empty())
    std::copy(mAttributes.begin(), mAttributes.end(), std::ostream_iterator<std::string>(os," "));
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


