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

bool
BaseStream::found_name_in_feature_vector (std::string const& name, FeatureVector const& vec, std::string const& vecName) const
{
  // std::cout << "Looking for " << name << " in " ;
  // for(int i=0; i < (int) vec.size(); ++i)  std::cout << vec[i]->name() << ", "; std::cout << std::endl;
  if (name.size() == 0)
    return false;
  for (std::vector<Feature>::const_iterator it = vec.begin();it != vec.end(); ++it)
  { if (name == (*it)->name())
    { debugging::debug("FETR", 4) << "Found feature " << name << " in " << vecName << std::endl;
      return true; 
    }
  }
  return false;
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
FiniteStream::build_next_feature(FeatureVector const&, FeatureVector const&)
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
LagStream::build_next_feature(FeatureVector const& accepted, FeatureVector const&)
{
  while (true)
  { ++mLag;
    if (mLag > mMaxLag)
    { if (mCyclesLeft>0)  // go around again
      { --mCyclesLeft;
	mLag = 1;
      }
      else return;        // cannot generate more
    }
    Feature lag(mFeature,mLag,mBlockSize);
    if (!found_name_in_feature_vector(lag->name(), accepted, "model features"))
    { set_head(lag);
      return;
    }
  }
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
	     mHead[0]->is_constant() //          ||                                    //  equiv to the internal feature
	     //	     indicators_from_same_parent(mFeature, mHead[0]) ||                     //  save the effort 
	     // found_name_in_feature_vector(feature_name(), used, "model features")   //  skip if has been used already
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


