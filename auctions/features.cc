#include "features.h"

#include "range_stats.h"

#include <sstream>
#include <set>



Feature::Feature()
{
  Column c;
  mFP = new ColumnFeature(c);
}

Feature::Feature(Column const &c)
{
  mFP = new ColumnFeature(c);
}  

Feature::Feature(Feature const& f, size_t lag, size_t blockSize)
{
  mFP = new LagFeature(f,lag,blockSize);
}
	
Feature::Feature(Feature const& f1, Feature const& f2)
{
  mFP = new InteractionFeature(f1,f2);
}

Feature::Feature(int n,  std::vector<double> b, std::vector<Feature> const& fv)
{
  mFP = new LinearCombinationFeature(n, b, fv);
}



Feature&
Feature::operator=(Feature const& f)
{
  if( (--mFP->mRefCount <= 0) && (mFP != f.mFP) ) delete mFP;
  mFP = f.mFP;
  mFP->mRefCount++;
  return *this;
}

//  utilities     utilities     utilities     utilities     utilities     utilities     utilities     utilities


FeatureVector
features_with_name(std::string name, FeatureVector const& fv)
{
  FeatureVector result;
  for (FeatureVector::const_iterator it=fv.begin(); it != fv.end(); ++it)
  { std::string fname ((*it)->name());
    if (std::string::npos != fname.find(name))
      result.push_back(*it);
  }
  return result;
}

  
Feature
make_indexed_feature(Feature const& f, IntegerColumn const& i)
{
  debugging::debug("IDXF",3) << "Creating indexed feature from '" << f->name() << " with " << f->size()
			     << " elements and " << i->size() << " indices from " << i->name() << ".\n";
  assert(i->size() == f->size());
  int n (i->size());
  std::vector<double> x (n);             // copy feature data into vector for indexing
  { FeatureABC::Iterator mSrc (f->begin());
    for (int i=0; i<n; ++i)
      x[i] = *mSrc++;
  }
  std::string name (f->name()+"["+i->name()+"]");
  Column dest(name.c_str(), n);
  double *pDest (dest->begin());
  int *pIndex (i->begin());
  for (int i=0; i<n; ++i)
    *pDest++ = x[*pIndex++];
  dest.init_properties();
  return Feature(dest);
}


//  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  

void
ColumnFeature::write_to(std::ostream& os) const
{
  os << class_name() << " " << name() << std::endl;
  FeatureABC::write_to(os);
}


//  Lag Feature     Lag Feature     Lag Feature     Lag Feature     Lag Feature     Lag Feature     Lag Feature

void
LagFeature::write_to(std::ostream& os) const
{
  os << class_name() << " " << name() << std::endl;
  FeatureABC::write_to(os); 
}


//  InteractionFeature     InteractionFeature     InteractionFeature     InteractionFeature


void
InteractionFeature::center_features()
{
  if (!mFeature1->is_dummy() && !mFeature1->is_constant())
    mCtr1 = mFeature1->average();
  if (!mFeature2->is_dummy() && !mFeature2->is_constant())
    mCtr2 = mFeature2->average();
}

void
InteractionFeature::collect_attributes()
{
  typedef typename std::set<std::string> StringSet;
  // very early, just to capture simple categorical terms
  // need to change handling since can have multiple categorial vars, each with own category
  // parent/category must be kept as a pair.
  if (mFeature1->has_attribute("parent"))
  { StringSet attrs (mFeature1->attribute_str_value("parent"));
    for(StringSet::const_iterator it=attrs.begin(); it != attrs.end(); ++it)
      add_attribute("parent",*it);
    add_attribute("category", *(mFeature1->attribute_str_value("category").begin()));
  }
  if (mFeature2->has_attribute("parent"))
  { StringSet attrs (mFeature2->attribute_str_value("parent"));
    for(StringSet::const_iterator it=attrs.begin(); it != attrs.end(); ++it)
      add_attribute("parent",*it);
    add_attribute("category", *(mFeature2->attribute_str_value("category").begin()));
  }
}


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
    


void
InteractionFeature::write_to (std::ostream& os) const
{
  os << class_name() << std::endl;
  mFeature1->write_to(os);
  mFeature2->write_to(os);
  FeatureABC::write_to(os);
}

// Unary feature

std::vector<Feature>
powers_of_column (Column const& col, std::vector<int> const& powers)
{
  std::vector<Feature> fv;
  Feature  base(col);
  for (size_t i=0; i<powers.size(); ++i)
  { switch( powers[i] )
    {
    case 2:
      fv.push_back(Feature(Function_Utils::CenteredSquare(col->average()), base));
      break;
    case 3:
      fv.push_back(Feature(Function_Utils::CenteredCube(col->average()), base));
      break;
    case 4:
      fv.push_back(Feature(Function_Utils::CenteredQuad(col->average()), base));
      break;
    case 5:
      fv.push_back(Feature(Function_Utils::CenteredQuint(col->average()), base));
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
  range_ops::fill(mColumn->writable_range(),mBeta[0]);
  for (unsigned int j=1; j<mBeta.size(); ++j)
    range_ops::transform(mFeatures[j-1]->range(), mColumn->range(), mColumn->begin(), Function_Utils::AXPY(mBeta[j]));
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


//     Feature Source     Feature Source     Feature Source     Feature Source     Feature Source     Feature Source     Feature Source

void
FeatureSource::initialize (std::vector<Column> cols)
{ 
  StringSet streams;                // set to track unique names
  // mStreams.push_back("MAIN");       
  // streams.insert("MAIN");
  for (std::vector<Column>::const_iterator it = cols.begin(); it != cols.end(); ++it)
  { Feature f(*it);                 // convert column to feature 
    StringSet streamSet (f->attribute_str_value("stream"));
    if (streamSet.empty())          // assign to default stream
    { f->add_attribute("stream", "MAIN");
      streamSet.insert("MAIN");
    }
    for(StringSet::const_iterator it=streamSet.begin(); it!=streamSet.end(); ++it)
      if(streams.find(*it) == streams.end())
      { streams.insert(*it);
	mStreams.push_back(*it);    // add new stream name to ordered list of names
      }
    mFeatures.push_back(f);
  }
}

void
FeatureSource::print_summary (std::ostream& os)     const
{ os << "FeatureSource (skip=" << mSkip << ") has " << number_of_features() << " features in " << number_of_streams() << " streams as follows.\n";
  for(StringVector::const_iterator it=mStreams.begin(); it != mStreams.end(); ++it)
    os << "      " << *it << " -- " << features_with_attribute("stream",*it).size() << std::endl;
}




namespace {
  bool found_string(std::string val, std::set<std::string> const& s)
  { for (std::set<std::string>::const_iterator it=s.begin(); it!=s.end(); ++it)
      if (val == *it)
	return true;
    return false;
  }
}


std::vector< Feature >
FeatureSource::features_with_attribute (std::string attr) const
{
  std::vector< Feature > fv;

  for(FeatureVector::const_iterator f = mFeatures.begin(); f != mFeatures.end(); ++f)
    if ( (*f)->has_attribute(attr) )
      fv.push_back(*f);
  return fv;
}


std::vector< Feature >
FeatureSource::features_with_attribute (std::string attr, std::string value) const
{
  std::vector< Feature > fv;

  for(FeatureVector::const_iterator f = mFeatures.begin(); f != mFeatures.end(); ++f)
    if ( (*f)->has_attribute(attr) && (found_string(value, (*f)->attribute_str_value(attr))) )
      fv.push_back(*f);
  return fv;
}


std::vector< Feature >
FeatureSource::features_with_attributes (std::set<std::string> const& attrs) const
{
  std::set< Feature > features;

  for(FeatureVector::const_iterator f = mFeatures.begin(); f != mFeatures.end(); ++f)
    for(StringSet::const_iterator s = attrs.begin(); s != attrs.end(); ++s)
      if ( (*f)->has_attribute(*s) )
      {	features.insert(*f);
	break;
      }
  FeatureVector fv;
  for(std::set<Feature>::const_iterator sv=features.begin(); sv!=features.end(); ++sv)
    fv.push_back(*sv);
  return fv;
}


