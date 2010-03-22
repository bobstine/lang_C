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
