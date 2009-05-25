// $Id: feature_streams.Template.h,v 1.14 2008/02/22 19:39:47 bob Exp $

#include "debug.h"

#include "adapter.h"

namespace {
  bool found_feature_name_in_vector (std::string const& name, Features::FeatureVector const& vec)
  {
    // std::cout << "Looking for " << name << " in " ;
    // for(int i=0; i < (int) vec.size(); ++i)  std::cout << vec[i]->name() << ", "; std::cout << std::endl;
    if (name.size() == 0)
      return false;
    for (Features::FeatureVector::const_iterator it = vec.begin();it != vec.end(); ++it)
    { if (name == (*it)->name())
      { debugging::debug(0) << "FETR: Found feature " << name << " already in model.\n";
	return true;
      }
    }
    return false;
  }
}


/////////////////  Finite Streams

template<class Source>
bool
FiniteStream<Source>::is_empty() const
{
  return (mPosition >= (int) mSource.size());
}

template<class Source>
void
FiniteStream<Source>::increment_position()
{
  ++mPosition;
}

template<class Source>
bool
FiniteStream<Source>::current_feature_is_okay(Features::FeatureVector const&, Features::FeatureVector const&) const
{
  return !(
	   mSource[mPosition]->was_tried_in_model() ||
	   mSource[mPosition]->is_constant()
	   );
}

template<class Source>
std::string
FiniteStream<Source>::feature_name() const                            
{
  return mSource[mPosition]->name();
}


template<class Source>
typename Features::FeatureVector
FiniteStream<Source>::pop()                            
{
  Features::FeatureVector result;
  result.push_back(mSource[mPosition]); 
  ++mPosition; 
  return result;
}



//  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream  FitStream

template<class Model>
bool
FitStream<Model>::current_feature_is_okay(Features::FeatureVector const& used, Features::FeatureVector const&)
{
  if(!used.empty())  // check name of last used feature for our signature
  { mEmpty = false;
    std::string lastVarName = used.back()->name();
    mIncreaseDegree = (std::string::npos != lastVarName.find(mSignature));  // npos means not found
    if (mIncreaseDegree && (std::string::npos != lastVarName.find("fifth")))
    { mEmpty = true;
      return false;
    }
  }
  return true;
}
      
template<class Model>
Features::FeatureVector
FitStream<Model>::pop()
{ ++mCount;
  Column fit(feature_name().c_str(), mModel.fit_length());
  mModel.fill_with_fit(fit->begin());
  fit->update();
  std::vector<int> powers;
  powers.push_back(2); powers.push_back(3);
  if (mIncreaseDegree)
    for(size_t i=0; i<powers.size(); ++i) powers[i]+=2;
  return powers_of_column_feature(fit,powers);
}

template<class Model>
std::string
FitStream<Model>::feature_name () const
{
  std::ostringstream oss;
  oss << mCount;
  return mSignature + oss.str();
}



  ///////////////    Iteraction Streams    Iteraction Streams    Iteraction Streams    Iteraction Streams    Iteraction Streams


template<class Source>
void
InteractionStream<Source>::build_current_feature_name()
{
  if (is_empty())
    mCurrentFeatureName = "";
  else
    mCurrentFeatureName = InteractionFeature(mSource[mPos1], mSource[mPos2]).name();
}

template<class Source>
bool
InteractionStream<Source>::is_empty() const
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
InteractionStream<Source>::current_feature_is_okay(Features::FeatureVector const& used, Features::FeatureVector const& skipped) const
{
  if (
      ((mPos1 == mPos2) && mSource[mPos1]->is_dummy()) ||    // dont square dummy
      (mSource[mPos1]->is_constant())                        // no interactions with constant (mPos2 handled in increment)
      )
    return false;
  std::string name (feature_name());
  if (found_feature_name_in_vector(name, used) || found_feature_name_in_vector(name,skipped))
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
typename Features::FeatureVector
InteractionStream<Source>::pop()
{
  FeatureABC* x1 (mSource[mPos1]);
  FeatureABC* x2 (mSource[mPos2]);
  increment_position();
  Features::FeatureVector result;
  result.push_back(new InteractionFeature(x1,x2));
  return result;
}



///  Cross-product stream  Cross-product stream  Cross-product stream  Cross-product stream  Cross-product stream

template<class Source1, class Source2>
bool
CrossProductStream<Source1, Source2>::is_empty() const
{
  return ((int)mFixedSource.size() <= mFixedPos) || (mDynSource.size() == 0);
}

template<class Source1, class Source2>
void
CrossProductStream<Source1, Source2>::build_current_feature_name()
{
  if (is_empty())
    mCurrentFeatureName = "";
  else
    mCurrentFeatureName = InteractionFeature(mFixedSource[mFixedPos], mDynSource[mDynPos]).name();
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
CrossProductStream<Source1, Source2>::current_feature_is_okay(Features::FeatureVector const& used, Features::FeatureVector const&)
{
  if (mFixedSource[mFixedPos]->is_constant() || (mDynSource[mDynPos]->is_constant()) )
    return false;
  if (mCurrentFeatureName=="")           // check that we have a name since streams may have grown
    build_current_feature_name();
  if (found_feature_name_in_vector(mCurrentFeatureName, used))  // try those that have been skipped before again
    return false;
  return true;
}


template<class Source1, class Source2>
typename Features::FeatureVector
CrossProductStream<Source1, Source2>::pop()
{
  debugging::debug(0) << "CPST: " << name() << " stream making cross-product of fixed["<< mFixedPos << "] x dyn[" << mDynPos << "].\n";
  FeatureABC const* xf (mFixedSource[mFixedPos]);
  FeatureABC const* xd (mDynSource[mDynPos]);
  increment_position();
  Features::FeatureVector result;
  result.push_back(new InteractionFeature(xf,xd));
  return(result);
}



///  PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   PolynomialStream   


template<class Source>
bool 
PolynomialStream<Source>::has_feature(Features::FeatureVector const&, Features::FeatureVector const&)
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
typename Features::FeatureVector
PolynomialStream<Source>::pop()
{
  FeatureABC *x  (mSource[mPos]);
  debugging::debug(0) << "PLYS: " << name() << " stream making polynomial subspace from feature " <<  x->name() << std::endl;
  ++mPos; // do not revisit this one
  std::vector<FeatureABC*> result;
  result.push_back(make_unary_feature(Function_Utils::Square(), x));
  if(mDegree>2) 
    result.push_back(make_unary_feature(Function_Utils::Cube(), x));
  for (int j=4; j<=mDegree; ++j)
    result.push_back(make_unary_feature(Function_Utils::Power(j), x));
  return result;
}

template<class Source>
bool 
PolynomialStream<Source>::feature_meets_conditions(FeatureABC const* feature) const
{ 
  ColumnFeature const* cp (dynamic_cast<ColumnFeature const*>(feature));
  return (cp && (!feature->is_dummy()) && (!feature->is_constant()));
}


///  BundleStream   BundleStream   BundleStream   BundleStream   BundleStream   BundleStream   BundleStream   


template<class Source, class Pred, class Trans>
bool
BundleStream<Source, Pred, Trans>::has_feature(Features::FeatureVector const&, Features::FeatureVector const&)
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
Features::FeatureVector 
SubspaceBasis<Method>::operator()(Features::FeatureVector const& fv) const
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
  std::vector<FeatureABC *> result;
  /* gsl_features did not respect memory... need to replace this with column features
     for (int j=0; j<numPC; ++j)
    result.push_back(new gslVectorFeature("Basis", basis.second[j]));
  */
  return result;
}

