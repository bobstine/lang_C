/*
 *  feature_stream.cc
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "feature_streams.h"


bool 
FeatureAcceptancePredicate::operator()(Feature const& f) const
{
  return !(f->is_constant() || f->is_dummy() || (f->name() == "Basis"));
}



//  Finite Stream     Finite Stream     Finite Stream     Finite Stream     Finite Stream     Finite Stream

void
FiniteStream::insert_features(FeatureVector const& f)
{
  for(FeatureVector::const_iterator it=f.begin(); it != f.end(); ++it)
    if (! (*it)->is_constant() )
      mFeatures.push_back( *it );
}

void
FiniteStream::build_next_feature()
{
  std::cout << "Building, with " << mFeatures.size() << " features and head of size " << mHead.size() << std::endl;
  // dump any features that are already in the model
  while(!mFeatures.empty() && mFeatures.front()->is_used_in_model())
    mFeatures.pop_front();
  // insert first feature into mHead
  if (mFeatures.empty())
    return;
  mHead.push_back(mFeatures.front());
  // put the current feature on the back to try again later
  mFeatures.pop_front();
  mFeatures.push_back(mHead[0]);
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
    os << " " << *it << std::endl;
}



///  Feature-product stream  Feature-product stream  Feature-product stream  Feature-product stream  Feature-product stream

void
FeatureProductStream::initialize_queue(FeatureVector const& s)
{
  for (FeatureVector::const_iterator is = s.begin(); is != s.end(); ++is)
    mQueue.push(*is);
}


bool
FeatureProductStream::current_feature_is_okay(std::vector<Feature> const& used, std::vector<Feature> const&)
{
  return  !( (mHead.size() == 0)                 ||
	     mHead[0]->is_constant()           ||                                    //  equiv to the internal feature
	     indicators_from_same_parent(mFeature, mHead[0]) ||                     //  save the effort 
	     found_feature_name_in_vector(feature_name(), used, "model features")   //  skip if has been used already
	     );
}


void
FeatureProductStream::build_next_feature()
{
  Feature  xd (mQueue.top());
  debugging::debug("FPST",3) << name() << " stream making product of "
			     << mFeature->name() << " x Queue[" << mQueue.size() << "] (" << xd->name() << ").\n";
  // increment position
  mQueue.pop();
  // build feature vector
  mHead.push_back(Feature(mFeature,xd));
}



//  LagStreams      LagStreams      LagStreams      LagStreams      LagStreams      LagStreams      LagStreams  

void
LagStream::build_next_feature()
{
  ++mLag;
  if (mLag > mMaxLag && mCyclesLeft>0)
  { --mCyclesLeft;
    mLag = 1;
  }
  mHead.push_back(Feature(mFeature,mLag,mBlockSize));
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
    


