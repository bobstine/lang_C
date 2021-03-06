#include "features.Template.h"
#include "column.Template.h"
#include "string_trim.h"

#include "range_stats.h"

#include <sstream>
#include <set>

const std::string tag = "FETR: ";
  

Feature::Feature()
{
  Column<Scalar> c;
  mFP = new ColumnFeature(c);
}

Feature::Feature(Column<Scalar> const &c)
{
  mFP = new ColumnFeature(c);
}  

Feature::Feature(Feature f, size_t lag, size_t blockSize)
{
  mFP = new LagFeature(f,lag,blockSize);
}
	
Feature::Feature(Feature f1, Feature f2)
{
  mFP = new InteractionFeature(f1,f2);
}


Feature::Feature(int n, std::string name,  std::vector<Scalar> b, std::vector<Feature> const& fv)
{
  mFP = new LinearCombinationFeature(n, name, b, fv);
}

Feature::Feature(int n,  std::vector<Scalar> b, std::vector<Feature> const& fv)
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


void
Feature::write_to (std::ostream& os) const
{
  mFP->write_to(os);
  FeatureABC::DependenceMap m = mFP->dependence_map();
  if (!m.empty())
  { os << std::endl << "                 Dependence map is   { ";
    std::for_each(m.begin(), m.end(),
		  [&os] (FeatureABC::DependenceMap::value_type const& p)
		  { os << " (" << p.first->name();
		    if(p.second>1)
		      os << "^" << p.second;
		    os << ")"; }
		  );
    os << " } ";
  }
}


//  utilities     utilities     utilities     utilities     utilities     utilities     utilities     utilities

std::vector<Feature>
features_with_name(std::string name, std::vector<Feature> const& fv)
{
  std::vector<Feature> result;
  for (auto f : fv)
  { std::string fname (f->name());
    if (std::string::npos != fname.find(name))
      result.push_back(f);
  }
  return result;
}

std::vector<Feature>
features_with_attribute(std::string attribute, std::string value, std::vector<Feature> const& fv)
{
  std::vector<Feature> result;
  for(auto f : fv)
  { std::string v = f->attribute_str_value(attribute);
    if(v == value)
      result.push_back(f);
  }
  return result;
}

  
Feature
make_indexed_feature(Feature f, IntegerColumn const& i)
{
  debugging::debug("IDXF",3) << "Creating indexed feature from '" << f->name() << " with " << f->size()
			     << " elements and " << i->size() << " indices from " << i->name() << ".\n";
  assert(i->size() == f->size());
  int n (i->size());
  std::vector<SCALAR> x (n);             // copy feature data into vector for indexing
  { FeatureABC::Iterator mSrc (f->begin());
    for (int i=0; i<n; ++i)
      x[i] = *mSrc++;
  }
  std::string name (f->name()+"["+i->name()+"]");
  Column<SCALAR> dest(name.c_str(), n);
  SCALAR *pDest (dest->begin());
  int *pIndex (i->begin());
  for (int i=0; i<n; ++i)
    *pDest++ = x[*pIndex++];
  dest.init_properties();
  return Feature(dest);
}


//  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  ColumnFeature  

std::string
ColumnFeature::class_name()     const
{ return "ColumnFeature"; }

std::string
ColumnFeature::name()           const
{ return mColumn->name(); }

std::string
ColumnFeature::operator_name()  const
{ return ""; }

FeatureABC::DependenceMap
ColumnFeature::dependence_map() const
{ return DependenceMap(); }

int
ColumnFeature::degree()         const
{ return 1; }

FeatureABC::Arguments
ColumnFeature::arguments()      const
{
  Arguments a;
  a[name()] = 1;
  return a;
}
  
Column<ColumnFeature::Scalar>
ColumnFeature::column()         const
{
  return mColumn;
}

FeatureABC::Iterator
ColumnFeature::begin()          const
{
  return make_anonymous_iterator(mColumn->begin());
}

FeatureABC::Iterator
ColumnFeature::end()            const
{
  return make_anonymous_iterator(mColumn->end());
}

FeatureABC::Range
ColumnFeature::range()          const
{
  return make_anonymous_range(mColumn->range());
}

bool
ColumnFeature::is_dummy()       const
{
  return mColumn->is_dummy();
}

bool
ColumnFeature::is_constant()    const
{
  return (1 == mColumn->num_unique());
}

ColumnFeature::Scalar
ColumnFeature::average()        const
{
  return (Scalar) mColumn->average();
}

ColumnFeature::Scalar
ColumnFeature::center()         const
{
  return (Scalar) mColumn->average();
}

ColumnFeature::Scalar
ColumnFeature::scale()          const
{
  return (Scalar) mColumn->scale();
}  // defaults to range/6

void
ColumnFeature::write_to(std::ostream& os) const
{
  os << class_name() << " " << name() << std::endl;
  FeatureABC::write_to(os);
}


//  Lag Feature     Lag Feature     Lag Feature     Lag Feature     Lag Feature     Lag Feature     Lag Feature


std::string
LagFeature::class_name()     const
{
  return "LagFeature";
}

std::string
LagFeature::name()           const
{
  return "Lag(" + mFeature->name() + ",t-" + mLagStr +")";
}

std::string
LagFeature::operator_name()  const
{
  return "[-" + mLagStr + "]";
}

int
LagFeature::degree()         const
{
  return mFeature->degree();
}

FeatureABC::Arguments
LagFeature::arguments()      const
{
  return mFeature->arguments();
}

FeatureABC::DependenceMap
LagFeature::dependence_map() const
{
  return DependenceMap();
}
  
int
LagFeature::lag()            const
{ return mLag; }

FeatureABC::Iterator
LagFeature::begin()          const
{
  return make_anonymous_iterator(make_lag_iterator(mFeature->begin(), mLag));
}

FeatureABC::Iterator
LagFeature::end()            const
{
  return make_anonymous_iterator(make_lag_iterator(mFeature->end()  , mLag));
}

FeatureABC::Range
LagFeature::range()                      const
{
  return make_anonymous_range(   make_lag_iterator(mFeature->begin(), mLag),
				 make_lag_iterator(mFeature->end()  , mLag));
}

bool
LagFeature::is_dummy()       const
{
  return mFeature->is_dummy();
}

bool
LagFeature::is_constant()    const
{
  return mFeature->is_constant();
}

LagFeature::Scalar
LagFeature::average()        const
{ return mFeature->average(); }

LagFeature::Scalar
LagFeature::center()         const
{ return mFeature->average(); }

LagFeature::Scalar
LagFeature::scale()          const
{ return mFeature->scale(); }

void
LagFeature::write_to(std::ostream& os) const
{
  os << class_name() << " " << name() << std::endl;
  FeatureABC::write_to(os); 
}


//  InteractionFeature     InteractionFeature     InteractionFeature     InteractionFeature


std::string
InteractionFeature::class_name()     const { return "InteractionFeature"; }

std::string
InteractionFeature::name()           const { return mName; }

std::string
InteractionFeature::operator_name()  const { return "*"; }

int
InteractionFeature::degree()         const { return mFeature1->degree() + mFeature2->degree(); }

FeatureABC::Arguments
InteractionFeature::arguments()      const { return join_arguments(mFeature1->arguments(), mFeature2->arguments()); }

FeatureABC::DependenceMap
InteractionFeature::dependence_map() const { return mDependenceMap; }

FeatureABC::Iterator
InteractionFeature::begin()          const
{
  return make_anonymous_iterator(
				 make_binary_iterator(
						      Function_Utils::CenteredMultiply(mCtr1,mCtr2),
						      mFeature1->begin(),
						      mFeature2->begin()));
}

FeatureABC::Iterator
InteractionFeature::end()            const
{
  return make_anonymous_iterator(
				 make_binary_iterator(
						      Function_Utils::CenteredMultiply(mCtr1,mCtr2),
						      mFeature1->end(),
						      mFeature2->end()));
}

FeatureABC::Range
InteractionFeature::range()          const
{
  return make_anonymous_range(
			      make_binary_range(
						Function_Utils::CenteredMultiply(mCtr1,mCtr2),
						// [&](Scalar x1, Scalar x2)->Scalar { return (x1-mCtr1)*(x2-mCtr2); },   // ??? how to make lambdas work
						mFeature1->range(),
						mFeature2->range()));
}

InteractionFeature::Scalar
InteractionFeature::average()        const { return (Scalar) Ranges::average(range(), (Scalar)size()); }

InteractionFeature::Scalar
InteractionFeature::center()         const { return (Scalar) mFeature1->center()*mFeature2->center(); }

InteractionFeature::Scalar
InteractionFeature::scale()          const { return (Scalar) mFeature1->scale()*mFeature2->scale(); }

bool
InteractionFeature::is_dummy()       const { return (mFeature1->is_dummy() && mFeature2->is_dummy()) ; }

bool
InteractionFeature::is_constant()    const { return (mFeature1->is_constant() && mFeature2->is_constant()); }

void
InteractionFeature::make_dependence_map()
{ // join dep maps of input features; init from first
  mDependenceMap = mFeature1->dependence_map();
  if (mDependenceMap.empty())  
    mDependenceMap[mFeature1] = 1;
  // add items from second
  DependenceMap two = mFeature2->dependence_map();
  if (two.empty())
    mDependenceMap[mFeature2] += 1;
  else
    std::for_each(two.begin(), two.end(),
		  [&] (DependenceMap::value_type const& p) { mDependenceMap[p.first]+=p.second; }
		  );
}

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
  if ( (!mFeature1->has_attribute("parent")) && (!mFeature2->has_attribute("parent")))
    return;
  else
  { std::string parentStr   (mFeature1->attribute_str_value("parent") + mFeature2->attribute_str_value("parent"));
    std::string categoryStr (mFeature1->attribute_str_value("category") + mFeature2->attribute_str_value("category"));
    if (parentStr.size()>0)
      set_attribute("parent", parentStr);
    if (categoryStr.size() > 0)
      set_attribute("category", categoryStr);
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
  os << " Dependence map is   { ";
  std::for_each(mDependenceMap.begin(), mDependenceMap.end(),
		[&os] (DependenceMap::value_type const& p) { os << " (" << p.first << "^" << p.second << ")"; }
		);
  os << std::endl;
  mFeature1->write_to(os);
  mFeature2->write_to(os);
  FeatureABC::write_to(os);
}

//  LinearCombinationFeature  LinearCombinationFeature  LinearCombinationFeature  LinearCombinationFeature

std::string
LinearCombinationFeature::class_name()     const { return "LinearCombinationFeature"; }

std::string
LinearCombinationFeature::name()           const { return mName; }

int
LinearCombinationFeature::degree()         const { return (int) mFeatures.size(); }

FeatureABC::Arguments
LinearCombinationFeature::arguments()      const { Arguments a; a[name()]=1; return a;}

FeatureABC::DependenceMap
LinearCombinationFeature::dependence_map() const { return DependenceMap(); }

FeatureABC::Iterator
LinearCombinationFeature::begin()          const { return make_anonymous_iterator(mColumn->begin()); }

FeatureABC::Iterator
LinearCombinationFeature::end()            const { return make_anonymous_iterator(mColumn->end()); }

FeatureABC::Range
LinearCombinationFeature::range()          const { return make_anonymous_range(mColumn->range()); }

LinearCombinationFeature::Scalar
LinearCombinationFeature::average()        const { return (Scalar) mColumn->average(); }

LinearCombinationFeature::Scalar
LinearCombinationFeature::center()         const { return (Scalar) mColumn->average(); }

LinearCombinationFeature::Scalar
LinearCombinationFeature::scale()          const { return (Scalar) mColumn->scale(); }

bool
LinearCombinationFeature::is_dummy()       const { return mColumn->is_dummy(); }

bool
LinearCombinationFeature::is_constant()    const { return (1 == mColumn->num_unique()); }


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

std::string
LinearCombinationFeature::make_name(std::vector<Scalar> b) const
{
  return(std::string("LC_") + std::to_string(b.size()-1));
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


std::vector<Feature>
powers_of_column (Column<SCALAR> const& col, std::vector<int> const& powers)
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

//     Feature Source     Feature Source     Feature Source     Feature Source     Feature Source     Feature Source     Feature Source

void
FeatureSource::initialize (std::vector<Column<SCALAR>> cols)
{ 
  StringSet streams;                      // track unique stream names
  for (std::vector<Column<SCALAR>>::const_iterator it = cols.begin(); it != cols.end(); ++it)
  { Feature f(*it);                       // convert column to feature 
    if (!f->has_attribute("stream"))      // assign to default stream
      f->set_attribute("stream", "MAIN");
    std::string streamName (f->attribute_str_value("stream"));
    if(streams.find(streamName) == streams.end())
    { streams.insert(streamName);
      mStreams.push_back(streamName);     // add new stream name to ordered list of names
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



std::vector< Feature >
FeatureSource::features_with_attribute (std::string attr) const
{
  std::vector< Feature > fv;

  for(auto f : mFeatures)
    if ( f->has_attribute(attr) )
      fv.push_back(f);
  return fv;
}


std::vector< Feature >
FeatureSource::features_with_attribute (std::string attr, std::string value) const
{
  std::vector< Feature > fv;

  for(auto f : mFeatures)
    if ( f->has_attribute(attr) && (f->attribute_str_value(attr) == value)  )
      fv.push_back(f);
  return fv;
}


std::vector< Feature >
FeatureSource::features_with_attributes (std::set<std::string> const& attrs) const
{
  std::set< Feature > features;

  for(auto f : mFeatures)
    for(std::string attr : attrs)
      if ( f->has_attribute(attr) )
      {	features.insert(f);
	break;
      }
  std::vector<Feature> fv;
  for(auto f : features)
    fv.push_back(f);
  return fv;
}


