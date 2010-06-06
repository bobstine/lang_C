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
FiniteStream::mark_position()            // mark prior location
{
  mIsEmpty = false;
  if (mPosition)
    mMarkedPosition = mPosition - 1;
  else
    mMarkedPosition = mFeatures.size() - 1;
  std::cout << "TESTING:  " << mName << " marking position " << mMarkedPosition << std::endl;
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
  if (mPosition >= (int) mFeatures.size())   // treat as cyclic
    mPosition = 0;
  mIsEmpty = (mPosition == mMarkedPosition); // empty if new position matches marked position
}


bool
FiniteStream::current_feature_is_okay(std::vector<Feature> const&, std::vector<Feature> const&) const
{
  bool okay (!(mFeatures[mPosition]->is_used_in_model() || mFeatures[mPosition]->is_constant()));
  /*
  std::cout << "FiniteStream " << mName << " at " << mPosition << " (out of " << mFeatures.size() 
  	    << ").  Top feature " << mFeatures[mPosition]->name();
  if (okay)
    std::cout << " is okay\n";
  else
    std::cout << " is not okay.\n";
  */
  return okay;
}


std::string
FiniteStream::feature_name() const                            
{
  return mFeatures[mPosition]->name();
}


void
FiniteStream::print_to(std::ostream& os)          const
{
  os << "FiniteStream " << mName << " at position " << mPosition << " with mark at " << mMarkedPosition;
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
