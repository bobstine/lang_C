#include "adapter.h"
#include "debug.h"


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
  if (mLastQ == mModel.q())
    return true;
  mLastQ = mModel.q();
  return false;
}

template<class Model>
bool
FitStream<Model>::current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const&)
{
   std::string lastVarName = used.back()->name();                             // check name of last used feature for name signature
   mIncreaseDegree = (std::string::npos != lastVarName.find(mSignature));     // ::npos means not found
   if (mIncreaseDegree && (std::string::npos != lastVarName.find("fifth")))   // already topped out at fifth degree
     return false;
   else
     return true;
}
      
template<class Model>
std::vector<Feature>FitStream<Model>::pop()
{
  std::vector<int> powers;
  if (!mIncreaseDegree)                                                  // first attempt to calibrate
  { ++mCount;
    mFit = Column(feature_name().c_str(), mSkip + mModel.fit_length());                    // grab current fit
    double *b (mFit->begin());
    for(int i=0; i<mSkip; ++i)      *b++ = 0;
    mModel.fill_with_fit(mFit->begin() + mSkip);
    mFit->update();
    powers.push_back(2);
    powers.push_back(3);                                                 // square and cubic for first attempt
  }                                                        
  else                                                                   // use higher powers to improve
  { powers.push_back(4);
    powers.push_back(5);
  }
  return powers_of_column_feature(mFit,powers);
}

template<class Model>
std::string
FitStream<Model>::feature_name () const
{
  std::ostringstream oss;
  oss << mCount;
  return mSignature + oss.str();
}



///////////////    Iteraction Streams    Iteraction Streams    Iteraction Streams    Iteraction Streams    Iteractgion Streams


template<class Source>
void
InteractionStream<Source>::build_current_feature_name()
{
  if (empty())
    mCurrentFeatureName = "";
  else
    mCurrentFeatureName = Feature(mSource[mPos1], mSource[mPos2])->name();
}

template<class Source>
bool
InteractionStream<Source>::empty() const
{
  return (mPos2 >= (int) mSource.size());
}


template<class Source>
void
InteractionStream<Source>::increment_position()
{
  if (0 == mPos1)  // move to next column; traverses 'upper half' a column at a time, from diagonal 'up'
  { ++mPos2;
    while ((mPos2 < (int)mSource.size())
	   && mSource[mPos2]->is_constant())            // skip constant column
      ++mPos2;
    mPos1 = (mUseSquares) ? mPos2 : mPos2-1;
  }
  else
    --mPos1;
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
    return false;
  std::string name (feature_name());
  if (found_feature_name_in_vector(name, used, "model features") || found_feature_name_in_vector(name,skipped, "skipped features"))
    return false;
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




///  Feature-product stream  Feature-product stream  Feature-product stream  Feature-product stream  Feature-product stream

template<class Source>
bool
FeatureProductStream<Source>::empty() const
{
  return (mSource.empty() || (mPos < 0));
}

template<class Source>
void
FeatureProductStream<Source>::build_current_feature_name()
{
  if (empty())
    mCurrentFeatureName = "";
  else
    mCurrentFeatureName = Feature(mFeature, mSource[mPos])->name();  // Feature(a,b) builds interaction
}


template<class Source>
void
FeatureProductStream<Source>::increment_position()
{
  --mPos;
  build_current_feature_name();
}


template<class Source>
bool
FeatureProductStream<Source>::current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const&)
{
  if ( mSource[mPos]->is_constant() ||                                            //  equiv to the internal feature
       indicators_from_same_parent(mFeature, mSource[mPos]) ||                    //  save the effort 
       found_feature_name_in_vector(mCurrentFeatureName, used, "model features")  //  skip if has been used already
       )
    return false;
  return true;
}


template<class Source>
typename std::vector<Feature>
FeatureProductStream<Source>::pop()
{
  Feature  xd (mSource[mPos]);  // pop must increment counter *after* reading off top
  debugging::debug("FPST",2) << name() << " stream making product of "
			     << mFeature->name() << " x Source[" << mPos << "] (" << xd->name() << ").\n";
  increment_position();
  std::vector<Feature> result;
  result.push_back(Feature(mFeature,xd));
  
  return(result);
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
  debugging::debug("CPST",4) << "Source sizes " << mSlowSource.size() << "," << mFastSource.size() << " give position " << mPos << " and " << num << " remaining.\n";
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
  return ( ! (feature->is_dummy() || (feature->is_constant()) ) );
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


