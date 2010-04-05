/*
 *  expert.cc
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "experts.h"


std::string
ExpertABC::role_string () const
{
  switch(mRole) {
  case source   : { return "source"; }
  case parasite : { return "parasite"; }
  case calibrate: { return "calibrate"; }
  case custom   : { return "custom"; }
  }
  return "not found";
}


std::vector< std::pair<std::string, FeatureABC::Iterator> >
ExpertABC::convert_to_model_iterators(FeatureVector const& fv) const
{ 
  NamedIteratorVector result;
  for(FeatureVector::const_iterator it=fv.begin(); it!=fv.end(); ++it)
    result.push_back(std::make_pair((*it)->name(), (*it)->begin() + mSkip));
  return result;
}

void
ExpertABC::print_to(std::ostream& os) const
{
  os << "Expert[" << role_string() << "," << mSkip <<"]: " << name() << " with alpha " << mAlpha; 
}


void 
ExpertABC::payoff (double w)    
{ 
  mBidHistory.append_bid_outcome( (w>0.0) );
  mAlpha += w; 
  if (mAlpha<-0.000001)
    std::cout << "XPRT:  *** Error. ***   Expert " << name() << " has negative alpha=" << mAlpha << std::endl;
}




Expert&
Expert::operator=(Expert const& e)
{
  if( mpExpert // watch for an empty expert
      && (--mpExpert->mRefCount <= 0) && (mpExpert != e.mpExpert) ) delete mpExpert;
  mpExpert = e.mpExpert;
  mpExpert->mRefCount++;
  return *this;
}
