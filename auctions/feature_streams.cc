/*
 *  feature_stream.cc
 *  auctions
 *
 *  Created by Robert Stine on 1/18/08.
 *  Copyright 2008. All rights reserved.
 *
 */

#include "feature_streams.h"


//     Predicates

bool
SkipIfDerived::operator()(Feature const& f) const
{
  std::string fname (f->name());
  return   (f->is_constant())  || (f->is_dummy())           ||
    (f->degree() > 1)                                       ||     // composition
    (fname.size() >= 4 && "cube" == fname.substr(0,4))      ||     // avoid powers
    (fname.size() >= 6 && "square" == fname.substr(0,6))    ||
    (f->has_attribute("neighborhood"))                      ||     // already-indexed variable
    (std::string::npos != fname.find("Y_hat_"))                    // calibration variable
    ;
}


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


//  LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator

LagIterator&
LagIterator::operator++()
{
  ++mLag; --mRemaining;
  if ((mLag > mMaxLag) && (mRemaining>0))
    mLag = 1;  // go around again
  return *this;
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


