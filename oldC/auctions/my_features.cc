// $Id: my_features.cc,v 1.8 2004/08/25 22:14:40 bob Exp $

#include "my_features.h"

#include "range_stats.h"

#include <sstream>


//  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  

void
ColumnFeature::write_to(std::ostream& os) const
{
  os << class_name() << " " << name() << std::endl;
  FeatureABC::write_to(os);
}


//  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature  InteractionFeature

void
InteractionFeature::make_name()
{
  Arguments args (arguments());
  for (Arguments::const_iterator it=args.begin(); it != args.end(); ++it)
  { mName += it->first->name();
    if (it->second > 1)
    { std::ostringstream ss;
      ss << it->second;
      mName += "^" + ss.str();
    }
    mName += "*";
  }
  mName[mName.size()-1]=' ';
}
    
FeatureABC::Arguments
InteractionFeature::arguments()    const
{
  Arguments argMap;
  bool raw1 (mFeature1->class_name() == "ColumnFeature");
  bool raw2 (mFeature2->class_name() == "ColumnFeature");
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
  os << class_name() << std::endl;
  mFeature1->write_to(os);
  mFeature2->write_to(os);
  FeatureABC::write_to(os);
}

//  LinearCombinationFeature  LinearCombinationFeature  LinearCombinationFeature  LinearCombinationFeature  

bool
LinearCombinationFeature::valid_args()    const 
{
  if (mBeta.size() == (1+mFeatures.size()))
    return true;
  else
  { std::cout << "\nFETR: *** Error *** Linear combination dimension error.\n";
    return false;
  }
}

void
LinearCombinationFeature::make_name()
{
  std::ostringstream oss ("");
  oss << "xb_" << mBeta.size()-1;
  mName = oss.str();
}


std::string
LinearCombinationFeature::long_name() const
{
  std::ostringstream oss ("");
  oss << "(" << mBeta[0];
  for (unsigned int j=1; j<mBeta.size(); ++j)
    oss << "+" << mBeta[j] << "*" << mFeatures[j-1]->name();
  return oss.str() + ")";
}


void
LinearCombinationFeature::fill_column()
{
  range_ops::fill(mColumn->range(),mBeta[0]);
  for (unsigned int j=1; j<mBeta.size(); ++j)
    range_ops::transform(mFeatures[j-1]->range(), mColumn->range(), begin(mColumn->range()), Function_Utils::AXPY(mBeta[j]));
  mColumn->update(); 
}

void
LinearCombinationFeature::write_to (std::ostream& os) const
{
  os << class_name() << " " << mBeta.size();
  for (unsigned int j=0; j<mBeta.size(); ++j)
    os << " " << mBeta[j];
  os << std::endl;
  for (unsigned int j=0; j<mBeta.size()-1; ++j)
    mFeatures[j]->write_to(os);
  FeatureABC::write_to(os);
}
