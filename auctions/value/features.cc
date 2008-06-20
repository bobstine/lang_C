// $Id: features.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

#include "features.h"

#include "range_stats.h"
#include "function_utils.h"

#include <sstream>

////  Operator traits  ///

std::string
operator_traits< std::multiplies<double> >::name()   { return "product"; }
char
operator_traits< std::multiplies<double> >::symbol() { return '*'; } 

std::string
operator_traits< Function_Utils::Power >::name()   { return "power"; }
char
operator_traits< Function_Utils::Power >::symbol()   { return '^'; }

std::string
operator_traits< std::plus<double> >::name()   { return "sum"; }
char
operator_traits< std::plus<double> >::symbol() { return '+'; }


//  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  



//  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature

std::string
InteractionFeature::name() const
{
  Arguments args (arguments());
  std::string name ("");
  for (Arguments::const_iterator it=args.begin(); it != args.end(); ++it)
  { name += it->first->name();
    if (it->second > 1)
    { std::ostringstream ss;
      ss << it->second;
      name += "^" + ss.str();
    }
    name += "*";
  }
  name[name.size()-1]=' ';
  return name;
}
    
FeatureABC::Arguments
InteractionFeature::arguments()    const
{
  Arguments argMap;
  bool raw1 (mFeature1->operator_name()=="");
  bool raw2 (mFeature2->operator_name()=="");
  if (raw1)
    argMap[mFeature1] = 1;
  else
    argMap = mFeature1->arguments();
  if (raw2)
    argMap[mFeature2] += 1;
  else
    argMap = join_arguments(argMap, mFeature2->arguments());
  return argMap;
}


void
InteractionFeature::write_to (std::ostream& os) const
{
  os << "BinaryFeature " << operator_traits< std::multiplies<double> >::symbol() << std::endl;
  mFeature1->write_to(os); 
  mFeature2->write_to(os); 
}

FeatureABC*
InteractionFeature::clone()         const
{
  return new InteractionFeature(mFeature1, mFeature2);
}
