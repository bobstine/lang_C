// $Id: featureABC.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

#include "featureABC.h"

#include "range_stats.h"

bool
FeatureABC::operator<  (FeatureABC const* f) const
{
  bool raw  = (operator_name()=="");
  bool fRaw = (f->operator_name()=="");
  if (raw && fRaw) 
    if (name() < f->name())
      return true;
    else
      return false;
  else                            // at least one compound feature
    if (raw && (not fRaw))       // put simple one first
      return true;
    else if (fRaw && (not raw))
      return false;
    else // both compound
      if (operator_name() == f->operator_name())
	return (name() < f->name());
      else
	return (operator_name() < f->operator_name());
}

FeatureABC::Arguments
FeatureABC::join_arguments(FeatureABC::Arguments const& a1, FeatureABC::Arguments const& a2) const
{
  Arguments args = a1;
  for (Arguments::const_iterator it = a2.begin(); it != a2.end(); ++it)
    args[it->first] += it->second;
  return args;
}

double
FeatureABC::average() const
{
  return range_stats::average(range());
}

double
FeatureABC::std_dev() const
{
  return range_stats::standard_deviation(range(), average(), range_ops::size(range()));
}

bool
FeatureABC::is_dummy() const
{
  return false;
}
