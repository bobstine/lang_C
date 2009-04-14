// $Id: feature_streams.Template.h,v 1.14 2008/02/22 19:39:47 bob Exp $

#include "debug.h"

#include "adapter.h"

namespace {
  bool found_feature_name (std::string const& name, Features::FeatureVector const& vec)
  {
    for (Features::FeatureVector::const_iterator it = vec.begin();it != vec.end(); ++it)
    { if (name == (*it)->name())
	return true;
    }
    return false;
  }
}


template<class Source>
bool
FiniteStream<Source>::has_feature (Features::FeatureVector const&, Features::FeatureVector const&)
{
  int n (mSource.size());
  if (mPosition >= n)
    return false;
  while(mSource[mPosition]->was_tried_in_model() || mSource[mPosition]->is_constant())
  { ++mPosition;
    if (mPosition == n) break;
  }
  return (mPosition < n);
}

template<class Source>
std::string
FiniteStream<Source>::feature_name()                            
{
  return mSource[mPosition]->name();
}


template<class Source>
typename Features::FeatureVector
FiniteStream<Source>::pop()                            
{
  Features::FeatureVector result (1);
  result[0] = mSource[mPosition]; 
  ++mPosition; 
  return result;
}


///////////////  Iteraction Streams

template<class Source>
bool
InteractionStream<Source>::has_feature(Features::FeatureVector const& used, Features::FeatureVector const& skipped)
{
  bool hasFeature = false;
  while ((mPos2 < (int)mSource.size()) && !hasFeature)
  { if ( ((mPos1 == mPos2) && mSource[mPos1]->is_dummy())
	 || (mSource[mPos1]->is_constant())
	 || (mSource[mPos2]->is_constant()) )
      increment_position();
    else
    { std::string name = feature_name();
      if (found_feature_name(name, used) || found_feature_name(name,skipped))
	increment_position();
      else
        hasFeature = true;
    }
  }
  return hasFeature;
}


template<class Source>
std::string
InteractionStream<Source>::feature_name() const
{
  return mSource[mPos1]->name()+"*"+mSource[mPos2]->name();
}


template<class Source>
void
InteractionStream<Source>::increment_position()
{
  // move to next column; traverses 'upper half' a column at a time, from diagonal 'up'
  if (0 == mPos1)
  { ++mPos2;
    mPos1 = mPos2;
  }
  else
    --mPos1;
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
    std::cout << "SINT: " << name() << " stream making interaction from #"<< mPos1 << " x #" << mPos2 << ".\n";
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
CrossProductStream<Source1, Source2>::has_feature(Features::FeatureVector const& used, Features::FeatureVector const& skipped)
{
  bool hasFeature = false;
  while (((mFixedSource.size()-mFixedPos)*(mDynSource.size()) > 0) && !hasFeature)
  { if (mFixedSource[mFixedPos]->is_constant() )
      increment_position();
    else
    { std::string name = feature_name();
      if (found_feature_name(name, used) || found_feature_name(name,skipped))
	increment_position();
      else
        hasFeature = true;
    }
  }
  return hasFeature;
}


template<class Source1, class Source2>
void
CrossProductStream<Source1, Source2>::increment_position()
{
  if (mFixedPos < (int) mFixedSource.size()-1)   // move on fixed first since will cover this more completely
    ++ mFixedPos;
  else if (mDynPos < (int)(mDynSource.size()-1))
    ++mDynPos;
  else
  { mFixedPos = 0;  // start over
    mDynPos = 0;
  }
}


template<class Source1, class Source2>
std::string
CrossProductStream<Source1,Source2>::feature_name() const
{
  return mFixedSource[mFixedPos]->name()+"*"+mDynSource[mDynPos]->name();
}


template<class Source1, class Source2>
typename Features::FeatureVector
CrossProductStream<Source1, Source2>::pop()
{
  debugging::debug(0) << "SCPS: " << name() << " stream making cross-product of fixed["<< mFixedPos << "] x dyn[" << mDynPos << "].\n";
  while ( (mFixedSource[mFixedPos]->is_constant()) ||
          (mDynSource[mDynPos]->is_constant()) )
  {
    increment_position();
  }
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
PolynomialStream<Source>::feature_name()                            
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
  for (int j=0; j<numPC; ++j)
    result.push_back(new gslVectorFeature("Basis", basis.second[j]));
  return result;
}

