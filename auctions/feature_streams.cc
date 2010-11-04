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
  for (FeatureIterator it = features.begin(); it != features.end(); ++it)
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


bool
FeatureProductStream::can_build_more_features(FeatureList const& accepted, FeatureList const&)
{
  if (number_remaining() == 0)
    return false;
  while ( !mQueue.empty() && indicators_from_same_parent(mFeature, mQueue.top()) )
    mQueue.pop();
  while ( !mQueue.empty() )
  { Feature candidate(mFeature,mQueue.top());
    if (found_name_among_features(candidate->name(), accepted, "model features"))   //  skip if has been used already
      mQueue.pop();
    else
      return true;
  }
  return false;
}

void
FeatureProductStream::build_next_feature()
{
  Feature candidate(mFeature,mQueue.top());
  debugging::debug("FPST",3) << name() << " constructing " << candidate << std::endl;
  set_head(candidate);
  mQueue.pop();
}



//   PolynomialStream     PolynomialStream     PolynomialStream     PolynomialStream     PolynomialStream     PolynomialStream     PolynomialStream     

  
bool
PolynomialStream::can_build_more_features(FeatureList const& accepts, FeatureList const& rejects)
{
  FeatureIterator theEnd ((mID==acceptedStreamID)?accepts.end():rejects.end());
  while (mIterator != theEnd)
  { std::string fname ((*mIterator)->name());
    debugging::debug("PLYS",4) << " Polynomial stream is considering variable '" << fname << "' \n";
    if ( ((*mIterator)->degree()>1)                           ||       // avoid calibration variables, powers, interactions
	 (fname.size() >= 4 && "cube" == fname.substr(0,4))   ||   
	 (fname.size() >= 6 && "square" == fname.substr(0,6)) ||
	 (std::string::npos != fname.find("Y_hat_") )         ||
	 ((*mIterator)->is_dummy())                           ||
	 ((*mIterator)->is_constant())      )               
    {
      ++mIterator;
    }
  }
  return mIterator != theEnd;
}

void
PolynomialStream::build_next_feature()
{
  debugging::debug("PLYS",4) << "Stream " << name() << " making polynomial subspace from feature " <<  (*mIterator)->name() << std::endl;
  FeatureVector powers;
  if (!(*mIterator)->is_used_in_model())    // include X if not in model
    powers.push_back(*mIterator);
  powers.push_back(Feature(Function_Utils::Square(), *mIterator));
  if(mDegree>2) 
    powers.push_back(Feature(Function_Utils::Cube(), *mIterator));
  for (int j=4; j<=mDegree; ++j)
    powers.push_back(Feature(Function_Utils::Power(j), *mIterator));
  set_head(powers);
  ++mIterator;
}



//  Cross-product stream    Cross-product stream    Cross-product stream    Cross-product stream    Cross-product stream

bool
CrossProductStream::can_build_more_features(FeatureList const& accepted, FeatureList const& rejected) const
{
  if (accepted.size() == rejected.size())
    return false;
  else
    return true;
}



void
CrossProductStream::update_iterators ()
{

}



void
CrossProductStream::build_next_feature()
{
  /*  debugging::debug("CPST",3) << name() << std::endl;
  while (false)
  { Feature  xs (*mSlowIterator);
    Feature  xf (*mFastIterators[0]);
    Feature candidate(xs,xf);
    if ( (!candidate->is_constant()) &&
	 (!found_name_among_features(feature_name(), "model features")) )
    { set_head(candidate);
      return;
    }
  }
  */
}


