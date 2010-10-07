#include "adapter.h"
#include "debug.h"
#include <assert.h>

namespace {
  
  bool indicators_from_same_parent(Feature const& f1, Feature const& f2)
  {
    return f1->has_attribute("category")
      && f2->has_attribute("category")
      && (f1->attribute_str_value("parent")==f2->attribute_str_value("parent"));
  }
  
  bool found_feature_name_in_vector (std::string const& name, std::vector<Feature> const& vec, std::string const& vecName)
  {
    // std::cout << "Looking for " << name << " in " ;
    // for(int i=0; i < (int) vec.size(); ++i)  std::cout << vec[i]->name() << ", "; std::cout << std::endl;
    if (name.size() == 0)
      return false;
    for (std::vector<Feature>::const_iterator it = vec.begin();it != vec.end(); ++it)
    { if (name == (*it)->name())
      { debugging::debug("FETR", 4) << "Found feature " << name << " in " << vecName << std::endl;
	return true; 
      }
    }
    return false;
  }
}


//  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream

template<class Model>
bool
FitStream<Model>::empty()  const
{
  if (mModel.q()==0) return true;
  return(mLastQ == mModel.q());
}

template<class Model>
bool
FitStream<Model>::current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const&)
{
  assert (used.size() > 0);                                                  // otherwise the stream should be empty
  std::string lastVarName = used.back()->name();                             // check name of last used feature for name signature
  bool foundSig = (std::string::npos != lastVarName.find(mSignature));       // ::npos means not found
  if (foundSig) 
  { debugging::debug("FSTR",4) << "Fit stream already used; not okay.\n";
    mLastQ = mModel.q();                                             // signals empty
    return false;
  }
  else
  { debugging::debug("FSTR",4) << "Fit stream current feature is okay.\n";
    return true;
  }
}
      
template<class Model>
std::vector<Feature>FitStream<Model>::pop()
{
  std::vector<int> powers;
  mFit = Column(feature_name().c_str(), mSkip + mModel.n_estimation_cases());  // grab current fit
  double *b (mFit->begin());
  for(int i=0; i<mSkip; ++i)      *b++ = 0;
  mModel.fill_with_fit(mFit->begin() + mSkip);
  mFit->update();
  for (int j = 2; j <= mPower; ++j)
    powers.push_back(j);
  debugging::debug("FSTR",4) << "Fit stream constructs powers 2-" << mPower <<" of " << mFit->name() << std::endl;
  mLastQ = mModel.q();                                                         // will be empty until next is added
  return powers_of_column_feature(mFit,powers);
}

template<class Model>
std::string
FitStream<Model>::feature_name () const
{
  if(empty())
    return std::string("");
  else
  { std::ostringstream oss;
    oss << mModel.q();
    return mSignature + oss.str();
  }
}



///////////////    Iteraction Streams    Iteraction Streams    Iteraction Streams    Iteraction Streams    Iteractgion Streams
//
//         f1    f2   f3   f4
//    f1    *     *    *    *
//    f2          *    *    *
//    f3               *    *
//    f4                    *
//                               mPos2 says which column; mPos1 tracks row, moving down from first row


template<class Source>
bool
InteractionStream<Source>::empty() const
{
  // debugging::debug("INST",4) << "Interaction stream " << mName << " @ " << mPos1 << "," << mPos2 << " has " << mSource.size() << " in source." << std::endl;
  return (mPos2 >= (int) mSource.size());
}


template<class Source>
void
InteractionStream<Source>::build_current_feature_name()
{
  debugging::debug("INST",4) << "Interaction stream " << mName << " @ " << mPos1 << "," << mPos2 << " building feature name, source size " << mSource.size() << std::endl;
  if (empty())
    mCurrentFeatureName = "";
  else
    mCurrentFeatureName = Feature(mSource[mPos1], mSource[mPos2])->name();
}


template<class Source>
void
InteractionStream<Source>::increment_position()
{
  if (mPos1 == mPos2-mDiag)                                                   // move to next column
  { ++mPos2;
    while ((mPos2 < (int) mSource.size()) && mSource[mPos2]->is_constant())  // skip constant columns
      ++mPos2;
    debugging::debug("INST",4) << "Interaction stream " << mName << " moving to column " << mPos2 << std::endl;
    mPos1 = 0;
  }
  else
    ++mPos1;
  build_current_feature_name();   // rebuild name
}


template<class Source>
bool
InteractionStream<Source>::current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const& skipped) const
{
  if (
      ((mPos1 == mPos2) && mSource[mPos1]->is_dummy()) ||              // dont square dummy
      indicators_from_same_parent(mSource[mPos1], mSource[mPos2]) ||   // disjoint by definition
      (mSource[mPos1]->is_constant())                                  // no interactions with constant (mPos2 handled in increment)
      )
  { debugging::debug("INST",4) << "Interaction stream " << mName << " avoiding dummy variable." << std::endl;
    return false;
  }
  std::string name (feature_name());
  if (found_feature_name_in_vector(name, used, "model features") || found_feature_name_in_vector(name,skipped, "skipped features"))
  { debugging::debug("INST",4) << "Interaction stream " << mName << " avoiding redundant variable." << std::endl;
    return false;
  }
  return true;
}

template<class Source>
int   
InteractionStream<Source>::number_remaining() const 
{
  int d (mSource.size()); 
  int m (mPos2+1); 
  return (d * (d+1))/2 - (m*(1+m))/2 + mPos1;
}

template<class Source>
typename std::vector<Feature>
InteractionStream<Source>::pop()
{
  Feature  x1 (mSource[mPos1]);  
  Feature  x2 (mSource[mPos2]);
  increment_position();
  std::vector<Feature> result;
  result.push_back(Feature(x1,x2));
  return result;
}


//  Cross-product stream    Cross-product stream    Cross-product stream    Cross-product stream    Cross-product stream

template<class Source1, class Source2>
  void
  CrossProductStream<Source1, Source2>::print_to(std::ostream& os)  const
{
  os << "CPST: " << name() << ": " ;
  if(empty())
    os << " is empty.";
  else
    os << mSlowPos << " @ " << mPos[mSlowPos];
}


template<class Source1, class Source2>
  void
  CrossProductStream<Source1, Source2>::update_position_vector()    const         // update mutable sources
{
  while (mPos.size() < mSlowSource.size())
    mPos.push_back(0);
}


template<class Source1, class Source2>
int
  CrossProductStream<Source1, Source2>::number_remaining()          const
{
  update_position_vector();
  int num (0);
  for(std::vector<int>::const_iterator it=mPos.begin(); it != mPos.end(); ++it)
    num += mFastSource.size() - *it;
  return num;
}

template<class Source1, class Source2>
bool
CrossProductStream<Source1, Source2>::empty()                       const
{
  return number_remaining() == 0;
}

  
template<class Source1, class Source2>
void
CrossProductStream<Source1, Source2>::build_current_feature_name() 
{
  if (empty())
    mCurrentFeatureName = "";
  else
    mCurrentFeatureName = Feature(mSlowSource[mSlowPos], mFastSource[mPos[mSlowPos]])->name();
}


template<class Source1, class Source2>
void
CrossProductStream<Source1, Source2>::increment_position()
{
  ++mPos[mSlowPos];  
  mSlowPos = 0;                                 // now go back to check that list has not grown
  update_position_vector();
  for(std::vector<int>::const_iterator it=mPos.begin(); it!=mPos.end(); ++it)
  { if (*it < (int) mFastSource.size())
      break;
    else
      ++mSlowPos;
  }
  debugging::debug("CPST",4) << "Increment position to " << mSlowPos << " x " << mPos[mSlowPos] << std::endl;
  build_current_feature_name();
}



template<class Source1, class Source2>
bool
CrossProductStream<Source1, Source2>::current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const&)
{
  if (mSlowPos >= (int) mSlowSource.size())   // happens when increment_position but none left
    return false;
  if (mSlowSource[mSlowPos]->is_constant() || (mFastSource[mPos[mSlowPos]]->is_constant()) )
    return false;
  if (mCurrentFeatureName=="")               // check that we have a name since streams may have grown
    build_current_feature_name();
  if (found_feature_name_in_vector(mCurrentFeatureName, used, "model features"))  // try those that have been skipped before again
    return false;
  return true;
}


template<class Source1, class Source2>
typename std::vector<Feature>
CrossProductStream<Source1, Source2>::pop()
{
  debugging::debug("CPST",3) << name() << " slow[" << mSlowPos << "/" << mSlowSource.size()
			     << "] x fast[" << mPos[mSlowPos] << "/" << mFastSource.size() << "]: "
			     << mSlowSource[0]->name() << " x " << mFastSource[0]->name() << std::endl;
  Feature  xs (mSlowSource[mSlowPos]);
  Feature  xf (mFastSource[mPos[mSlowPos]]);
  increment_position();
  std::vector<Feature> result;
  result.push_back(Feature(xs,xf));
  return(result);
}



///  PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream

template<class Source>
std::string
PolynomialStream<Source>::feature_name() const                            
{
  if (empty())
    return std::string("");
  else
    return mSource[mPos]->name();
}


template<class Source>
typename std::vector<Feature>
PolynomialStream<Source>::pop()
{
  Feature x  (mSource[mPos]);
  debugging::debug("PLYS",4) << "Stream " << name() << " making polynomial subspace from feature " <<  x->name() << std::endl;
  increment_position();
  std::vector<Feature> result;
  if (!x->is_used_in_model())    // include X if not in model
    result.push_back(x);
  result.push_back(Feature(Function_Utils::Square(), x));
  if(mDegree>2) 
    result.push_back(Feature(Function_Utils::Cube(), x));
  for (int j=4; j<=mDegree; ++j)
    result.push_back(Feature(Function_Utils::Power(j), x));
  return result;
}


template<class Source>
bool
PolynomialStream<Source>::current_feature_is_okay(std::vector<Feature> const&, std::vector<Feature> const&)
{ 
  Feature  feature (mSource[mPos]);
  std::string name (feature->name());
  debugging::debug("PLYS",4) << " Polynomial stream is considering variable '" << name << "'\n";
  // avoid calibration variables, powers, interactions
  if (feature->degree()>1)                              return false;
  if (name.size() >= 4 && "cube" == name.substr(0,4))   return false;
  if (name.size() >= 6 && "square" == name.substr(0,6)) return false;
  if (std::string::npos != name.find("Y_hat_") )        return false;
  return ( ! (feature->is_dummy() || (feature->is_constant()) ) );
}




//  NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams      NeighborhoodStreams  

template<class Source>
std::string 
NeighborhoodStream<Source>::feature_name() const 
{ 
  if(empty())
    return ("");
  else
    return mSource[mPos]->name() + "[" + mIndexColumn->name() + "]";
}

template<class Source>
std::vector<Feature>
NeighborhoodStream<Source>::pop()                
{ 
  Feature x  (mSource[mPos]);
  debugging::debug("NBDS",4) << "Stream " << name() << " making indexed feature from " <<  x->name() << std::endl;
  increment_position();
  FeatureVector result;
  Feature ix (make_indexed_feature(x,mIndexColumn));
  result.push_back(ix);
  return result; 
}

template<class Source>
void
NeighborhoodStream<Source>::print_to(std::ostream& os)          const
{
  os << "Neighborhood feature stream " << name();
  if (empty())
    os << " is empty.";
  else
    os << " has " << number_remaining() << " features.";
}

template<class Source>
bool
NeighborhoodStream<Source>::current_feature_is_okay(FeatureVector const&, FeatureVector const&)   const
{
  Feature  feature (mSource[mPos]);
  std::string name (feature->name());
  debugging::debug("NBDS",4) << " Neighborhood stream is considering variable '" << name << "'\n";
  if (name.size() >= 4 && "cube" == name.substr(0,4))                  // avoid calibration variables, powers
    return false;
  if (name.size() >= 6 && "square" == name.substr(0,6))
    return false;
  if (std::string::npos == name.find(mSignature))                      // must include the signature
    return false;
  if (std::string::npos != name.find(mIndexColumn->name()))            // avoid already-indexed variables
    return false;
  if (std::string::npos != name.find("Y_hat_") )
    return false;
  return ( (!feature->is_constant()) && (!feature->is_dummy()) );
}



///   SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream

template<class Source, class Pred, class Trans>
  std::vector<Feature>
  SubspaceStream<Source, Pred, Trans>::pop()
{
  assert (mBundle.size() > 0);
  debugging::debug("SUBS",4) << mName << " popping bundle with " << mBundle.size() << " elements.\n";
  //  show names
  //  for (FeatureVector::const_iterator it = mBundle.begin(); it != mBundle.end(); ++it)   std::cout << (*it)->name() << " ";
  //  std::cout << std::endl;

  //  Dump the bundle to a file to see what's going on: stream to file and then transpose
  /*  
      std::string file ("/Users/bob/C/auctions/data/anes/bundle.txt");
      std::ofstream bs (file.c_str());
      if (bs)
      { debugging::debug("SUBS", 3) << "Writing bundle to file " << file << std::endl;
      for (unsigned int j=0; j<mBundle.size(); ++j)
      { mBundle[j]->write_values_to(bs);
      bs << std::endl;
      }
      }
      else
      debugging::debug("SUBS", 3) << " Non-fatal error; could not open file " << file << " to dump feature bundle.\n ";
  */      
  std::vector<Feature> result (mTransformation(mBundle));
  debugging::debug("SUBS",4) << mName << " transformation completed.\n";  
  mBundle.clear();
  return result;
}

  
template<class Source, class Pred, class Trans>
  void
  SubspaceStream<Source, Pred, Trans>::increment_position()
{
  while (((int)mBundle.size()<mBundleSize) && (mPos<(int)mSource.size()))
  { if(mPredicate(mSource[mPos]))
      mBundle.push_back(mSource[mPos]);
    ++mPos;
  }
}


