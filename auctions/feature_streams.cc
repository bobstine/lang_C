/*
 *  feature_stream.cc
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "feature_streams.h"

// principal components
#include "gsl_eigen.h"
// copy into gsl matrix
#include "gsl_utils.h"

bool 
FeatureAcceptancePredicate::operator()(Feature const& f) const
{
  return !(f->is_constant() || f->is_dummy() || (f->name() == "Basis"));
}



//  Finite Stream     Finite Stream     Finite Stream     Finite Stream     Finite Stream     Finite Stream

/*
  The initial boolean in each pair asks whether this variable has been tried
  since the last time that the model changed (indicated by 'mark_position')
*/

void
FiniteStream::insert_features(FeatureVector const& f)
{
  for(FeatureVector::const_iterator it=f.begin(); it != f.end(); ++it)
    if (! (*it)->is_constant() )
      mFeatures.push_back( std::make_pair(false,*it) );
}


int
FiniteStream::number_remaining() const
{
  return mFeatures.size();
}


std::vector<Feature>
FiniteStream::pop()                            
{
  FeatureVector result;
  result.push_back(mFeatures.front().second); 
  increment_position();
  return result;
}

void
FiniteStream::mark_position()
{
  for(QueueType::iterator it=mFeatures.begin(); it != mFeatures.end(); ++it)
    it->first = false;            // all get an untried stamp
}

bool
FiniteStream::empty()  const
{
  return ( (mFeatures.size()==0) || mFeatures.front().first );
}


void
FiniteStream::increment_position()
{
  Feature f (mFeatures.front().second);
  mFeatures.pop_front();
  if (f->is_used_in_model())      // dump f from queue
    return;
  else                            // return with boolean indicator to show tried since last added
    mFeatures.push_back(std::make_pair(true,f));   
}


bool
FiniteStream::current_feature_is_okay(std::vector<Feature> const&, std::vector<Feature> const&) const
{
  return !mFeatures.front().second->is_used_in_model();
}


std::string
FiniteStream::feature_name() const                            
{
  if(empty())
    return std::string("");
  else
    return mFeatures.front().second->name();
}


void
FiniteStream::print_to(std::ostream& os)          const
{
  os << "FiniteStream " << mName;
  if (empty())
    os << " is empty.";
  else
    os << " @ " << feature_name() << " with queue size " << mFeatures.size() << " ";
}

void
FiniteStream::print_features_to (std::ostream& os) const
{
  print_to(os);
  os << std::endl;
  for(QueueType::const_iterator it=mFeatures.begin(); it != mFeatures.end(); ++it)
    os << "     " << it->first << "  " << it->second << std::endl;
}



///  Feature-product stream  Feature-product stream  Feature-product stream  Feature-product stream  Feature-product stream

void
FeatureProductStream::initialize_queue(FeatureVector const& s)
{
  for (FeatureVector::const_iterator is = s.begin(); is != s.end(); ++is)
    mQueue.push(*is);
}

void
FeatureProductStream::build_current_feature_name()
{
  if (empty())
    mCurrentFeatureName = "";
  else
    mCurrentFeatureName = Feature(mFeature, mQueue.top())->name();  // Feature(a,b) builds interaction
}


void
FeatureProductStream::increment_position()
{
  mQueue.pop();
  build_current_feature_name();
}


bool
FeatureProductStream::current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const&)
{
  if ( mQueue.top()->is_constant() ||                                             //  equiv to the internal feature
       indicators_from_same_parent(mFeature, mQueue.top()) ||                     //  save the effort 
       found_feature_name_in_vector(mCurrentFeatureName, used, "model features")  //  skip if has been used already
       )
    return false;
  return true;
}


FeatureVector
FeatureProductStream::pop()
{
  Feature  xd (mQueue.top());
  debugging::debug("FPST",3) << name() << " stream making product of "
			     << mFeature->name() << " x Queue[" << mQueue.size() << "] (" << xd->name() << ").\n";
  increment_position();
  std::vector<Feature> result;
  result.push_back(Feature(mFeature,xd));
  
  return(result);
}



//  LagStreams      LagStreams      LagStreams      LagStreams      LagStreams      LagStreams      LagStreams  

std::string 
LagStream::feature_name() const 
{ 
  if(empty())
    return ("");
  else
  { std::ostringstream ss; 
    ss << mLag; 
    return mFeature->name()+"[t-"+ss.str()+"]";
  }
}

std::vector<Feature>
LagStream::pop()                
{ 
  increment_position(); 
  FeatureVector(fv); 
  fv.push_back(Feature(mFeature,mLag,mBlockSize)); 
  return fv; 
}

void
LagStream::print_to(std::ostream& os)          const
{
  os << "Lag feature stream " << name();
  if (empty())
    os << " is empty.";
  else
    os << " at position " << mLag << " with " << mCyclesLeft << " cycles left.";
}
    

int
LagStream::number_remaining()                  const
{
  return  mMaxLag - mLag + mCyclesLeft * mMaxLag;
}


bool
LagStream::empty()  const
{
  return (mCyclesLeft==0) && (mLag > mMaxLag);
}

bool
LagStream::current_feature_is_okay(FeatureVector const&, FeatureVector const&)   const
{
  return (!mFeature->is_constant());   // need a better check here for whether lags are in model already
}


void
LagStream::increment_position()
{
  ++mLag;
  if (mLag > mMaxLag && mCyclesLeft>0)
  { --mCyclesLeft;
    mLag = 1;
  }
}
