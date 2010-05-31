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

int
FiniteStream::number_remaining() const
{
  int diff (mMarkedPosition - mPosition);
  if (diff > 0)
    return diff;
  else
    return mFeatures.size() + diff;  // handles 0, returning size if at current position
}


std::vector<Feature>
FiniteStream::pop()                            
{
  std::vector<Feature> result;
  result.push_back(mFeatures[mPosition]); 
  increment_position();
  return result;
}


void
FiniteStream::mark_position()
{
  mMarkedPosition = mPosition;
}



bool
FiniteStream::empty()  const
{
  return mIsEmpty;
}


void
FiniteStream::increment_position()
{
  ++mPosition;
  if (mPosition > (int) mFeatures.size())    // treat as cyclic
    mPosition = 0;
  mIsEmpty = (mPosition == mMarkedPosition); // empty if new position matches marked position
}


bool
FiniteStream::current_feature_is_okay(std::vector<Feature> const&, std::vector<Feature> const&) const
{
  //  std::cout << "FINITE STREAM at " << mPosition << "(out of " << mSource.size() << ") with " << mCyclesLeft << " cycles left."
  //	    << " top feature is " << mSource[mPosition]->name() <<  std::endl;
  return !(
	   mFeatures[mPosition]->is_used_in_model() ||
	   mFeatures[mPosition]->is_constant()
	   );
}


std::string
FiniteStream::feature_name() const                            
{
  return mFeatures[mPosition]->name();
}


//  LagStreams      LagStreams      LagStreams      LagStreams      LagStreams      LagStreams      LagStreams  

std::string
LagStream::name()         const 
{ 
  return mName; 
}
  
std::string 
LagStream::feature_name() const 
{ 
  std::ostringstream ss; 
  ss << mLag; 
  return mFeature->name()+"[t-"+ss.str()+"]"; 
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
  os << "Lag feature stream " << name() << " at position " << mLag << " with " << mCyclesLeft << " cycles left.\n";
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
