// $Id: my_features.cc,v 3.3 2008/01/30 03:57:01 bob Exp $

#include "my_features.h"

#include "range_stats.h"
#include "gsl_utils.h"

#include <sstream>
#include <set>

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
  { mName += it->first;
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
    argMap[mFeature1->name()] = 1;
  else
    argMap = mFeature1->arguments();
  if (raw2)
    argMap[mFeature2->name()] += 1;  // need += in case of square
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

// Unary feature

Features::FeatureVector
powers_of_column_feature (Column const& col, std::vector<int> const& powers)
{
  Features::FeatureVector fv;
  ColumnFeature * base = new ColumnFeature(col);   // need a better memory model to avoid this???
  for (size_t i=0; i<powers.size(); ++i)
  { switch( powers[i] )
    {
    case 2:
      fv.push_back(make_unary_feature(Function_Utils::CenteredSquare(col.average()), base));
      break;
    case 3:
      fv.push_back(make_unary_feature(Function_Utils::CenteredCube(col.average()), base));
      break;
    case 4:
      fv.push_back(make_unary_feature(Function_Utils::CenteredQuad(col.average()), base));
      break;
    case 5:
      fv.push_back(make_unary_feature(Function_Utils::CenteredQuint(col.average()), base));
      break;
    default:
      std::cout << "FETR: Error. Requested power outside of supported range.\n";
    }
  }
  return fv;    
}



//  LinearCombinationFeature  LinearCombinationFeature  LinearCombinationFeature  LinearCombinationFeature  

bool
LinearCombinationFeature::valid_args()    const 
{
  if (mBeta.size() == (1+mFeatures.size()))
    return true;
  else
    { std::cout << "\nFETR: *** Error *** " << mBeta.size() << " != 1 + " <<  mFeatures.size()
		<< " *** Linear combination dimension error.\n";
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
  range_ops::fill(mColumn.writable_range(),mBeta[0]);
  for (unsigned int j=1; j<mBeta.size(); ++j)
    range_ops::transform(mFeatures[j-1]->range(), mColumn.range(), mColumn.begin(), Function_Utils::AXPY(mBeta[j]));
  mColumn.update(); 
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


