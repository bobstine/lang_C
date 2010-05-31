#include "debug.h"

#include "adapter.h"

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
      { debugging::debug(0) << "FETR: Found feature " << name << " in feature vector " << vecName << std::endl;
	return true;
      }
    }
    return false;
  }
}


//  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream

template<class Model>
bool
FitStream<Model>::empty()
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
std::vector<Feature>
FitStream<Model>::pop()
{
  std::vector<int> powers;
  if (!mIncreaseDegree)                                              // first attempt to calibrate
  { ++mCount;
    mFit = Column(feature_name().c_str(), mModel.fit_length());      // grab current fit
    mModel.fill_with_fit(mFit->begin());
    mFit->update();
    powers.push_back(2);
    powers.push_back(3);                                             // square and cubic for first attempt
  }                                                        
  else                                                               // use higher powers to improve
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
  debugging::debug("FPST",0) << name() << " stream making product of "
			     << mFeature->name() << " x Source[" << mPos << "] (" << xd->name() << ").\n";
  increment_position();
  std::vector<Feature> result;
  result.push_back(Feature(mFeature,xd));
  
  return(result);
}



//  Cross-product stream  Cross-product stream  Cross-product stream  Cross-product stream  Cross-product stream

template<class Source1, class Source2>
bool
CrossProductStream<Source1, Source2>::empty() const
{
  return ((int)mFixedSource.size() <= mFixedPos) || (mDynSource.size() == 0);
}

template<class Source1, class Source2>
void
CrossProductStream<Source1, Source2>::build_current_feature_name()
{
  if (empty())
    mCurrentFeatureName = "";
  else
    mCurrentFeatureName = Feature(mFixedSource[mFixedPos], mDynSource[mDynPos])->name();
}


template<class Source1, class Source2>
void
CrossProductStream<Source1, Source2>::increment_position()
{
  if (mFixedPos < (int) mFixedSource.size()-1)   // move on fixed first since will cover this more completely
    ++ mFixedPos;
  else if (mDynPos < (int)(mDynSource.size()-1))
    ++ mDynPos;
  else
  { mFixedPos = 0;  // start over
    mDynPos = 0;
  }
  build_current_feature_name();
}



template<class Source1, class Source2>
bool
CrossProductStream<Source1, Source2>::current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const&)
{
  if (mFixedSource[mFixedPos]->is_constant() || (mDynSource[mDynPos]->is_constant()) )
    return false;
  if (mCurrentFeatureName=="")           // check that we have a name since streams may have grown
    build_current_feature_name();
  if (found_feature_name_in_vector(mCurrentFeatureName, used, "model features"))  // try those that have been skipped before again
    return false;
  return true;
}


template<class Source1, class Source2>
typename std::vector<Feature>
CrossProductStream<Source1, Source2>::pop()
{
  debugging::debug("CPST",0) << name() << " stream making cross-product of fixed["<< mFixedPos << "] x dyn[" << mDynPos << "].\n";
  Feature  xf (mFixedSource[mFixedPos]);
  Feature  xd (mDynSource[mDynPos]);
  increment_position();
  std::vector<Feature> result;
  result.push_back(Feature(xf,xd));
  return(result);
}



///  PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   


template<class Source>
bool 
PolynomialStream<Source>::has_feature(std::vector<Feature> const&, std::vector<Feature> const&)
{ 
  int remaining (number_remaining());
  if (0 == remaining)
    return false;
  if (feature_meets_conditions(mSource[mPos]))
    return true;
  else if (remaining>1)
    increment_position();
  return (feature_meets_conditions(mSource[mPos])); }


template<class Source>
std::string
PolynomialStream<Source>::feature_name() const                            
{
  return mSource[mPos]->name();
}

template<class Source>
void
PolynomialStream<Source>::increment_position()
{
  int k (mSource.size());
  if (mPos < k-1) ++mPos;
  while ( (! feature_meets_conditions(mSource[mPos]))
          && (mPos<k-1) )
    ++mPos;
}


template<class Source>
typename std::vector<Feature>
PolynomialStream<Source>::pop()
{
  Feature x  (mSource[mPos]);
  debugging::debug(0) << "PLYS: " << name() << " stream making polynomial subspace from feature " <<  x->name() << std::endl;
  ++mPos; // do not revisit this one
  std::vector<Feature> result;
  result.push_back(Feature(Function_Utils::Square(), x));
  if(mDegree>2) 
    result.push_back(Feature(Function_Utils::Cube(), x));
  for (int j=4; j<=mDegree; ++j)
    result.push_back(Feature(Function_Utils::Power(j), x));
  return result;
}

template<class Source>
bool 
PolynomialStream<Source>::feature_meets_conditions(Feature const& feature) const
{ 
  // dont need this anymore???  ColumnFeature const* cp (dynamic_cast<ColumnFeature const*>(feature));
  return ( (!feature->is_dummy()) && (!feature->is_constant()) );
}


///  BundleStream   BundleStream   BundleStream   BundleStream   BundleStream   BundleStream   BundleStream   


template<class Source, class Pred, class Trans>
bool
BundleStream<Source, Pred, Trans>::has_feature(std::vector<Feature> const&, std::vector<Feature> const&)
{
  if (mPopped)
  { mPopped = false;
    mBundle.clear();
  }
  // add as much to the bundle as possible from the current source
  while (((int)mBundle.size()<mBundleSize) && (mPos<(int)mSource.size()) && mPredicate(mSource[mPos]))
  { mBundle.push_back(mSource[mPos]);
    ++mPos;
  }
  return ((int)mBundle.size() == mBundleSize);
}
 
template <class Method>
std::vector<Feature> 
SubspaceBasis<Method>::operator()(std::vector<Feature> const& fv) const
{
  gsl_matrix *mat (ConvertFeaturesIntoMatrix(fv));
  // form the decomposition of the corr matrix 
  std::pair<gsl_vector*, std::vector<gsl_vector*> > basis = mMethod(mat);
  int numPC ((int)basis.second.size());
  if (numPC > 0)
    std::cout << "SUBB: Leading eigenvalues are " << &gsl_vector_const_subvector(basis.first,0,numPC).vector << std::endl;
  else
    std::cout << "SUBB: *** Error ***    Subspace eigenvector decomposition returns no basis element.\n";
  // free space allocated by conversion
  gsl_vector_free(basis.first);
  gsl_matrix_free(mat);
  // convert the resulting vector of gsl vectors into features
  std::vector<Feature> result;
  /* gsl_features did not respect memory... need to replace this with column features
     for (int j=0; j<numPC; ++j)
    result.push_back(new gslVectorFeature("Basis", basis.second[j]));
  */
  return result;
}

