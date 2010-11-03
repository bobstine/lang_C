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



//  BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream     BaseStream

bool
BaseStream::indicators_from_same_parent(Feature const& f1, Feature const& f2) const
{
  return f1->has_attribute("category")
    && f2->has_attribute("category")
    && (f1->attribute_str_value("parent")==f2->attribute_str_value("parent"));
}

std::string
BaseStream::last_name_in_list (FeatureList const& features) const
{
  std::string name ("");
  for (FeatureIterator it = features.begin(); it != features.end(r); ++it)
    name = (*it)->name();
  return name;
}



//  Finite Stream     Finite Stream     Finite Stream     Finite Stream     Finite Stream     Finite Stream

void
FiniteStream::insert_features(FeatureVector const& f)
{
  for(FeatureVector::const_iterator it=f.begin(); it != f.end(); ++it)
    if (! (*it)->is_constant() )
    { std::cout << "TEST: inserting feature " << *it << std::endl;
      mFeatures.push_back( *it );
    }
}

void
FiniteStream::build_next_feature()
{
  // dump any features that are already in the model
  while(!mFeatures.empty() && mFeatures.front()->is_used_in_model())
    mFeatures.pop_front();
  // put front into head
  if(!mFeatures.empty())
  { set_head(mFeatures.front());
    // put the current feature on the back to try again later
    mFeatures.push_back(mFeatures.front());
    mFeatures.pop_front();
  }  
}

void
FiniteStream::print_features_to (std::ostream& os) const
{
  BaseStream::print_to(os);
  os << std::endl;
  for(std::deque<Feature>::const_iterator it=mFeatures.begin(); it != mFeatures.end(); ++it)
    os << " " << *it << std::endl;
}



//  LagStreams      LagStreams      LagStreams      LagStreams      LagStreams      LagStreams      LagStreams  

void
LagStream::build_next_feature()
{
  ++mLag;
  if (mLag > mMaxLag)
  { if (mCyclesLeft>0)  // go around again
    { --mCyclesLeft;
      mLag = 1;
    }
    else return;        // cannot generate more
  }
  Feature lag(mFeature,mLag,mBlockSize);
  set_head(lag);
}



///  Feature-product stream  Feature-product stream  Feature-product stream  Feature-product stream  Feature-product stream

void
FeatureProductStream::initialize_queue(FeatureList const& s)
{
  for (FeatureList::const_iterator is = s.begin(); is != s.end(); ++is)
    if (! (*is)->is_constant() )
      mQueue.push(*is);
}


void
FeatureProductStream::build_next_feature()
{
  while ( !mQueue.empty() && indicators_from_same_parent(mFeature, mQueue.top()) )
    mQueue.pop();
  while ( !mQueue.empty() )
  { Feature candidate(mFeature,mQueue.top());
    if (found_name_among_features(candidate->name(), mAccepted, "model features"))   //  skip if has been used already
      mQueue.pop();
    else
    { debugging::debug("FPST",3) << name() << " constructing " << candidate << std::endl;
      set_head(candidate);
      mQueue.pop();
      return;
    }
  }
}


