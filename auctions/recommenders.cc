// $Id: recommenders.cc,v 3.5 2008/01/18 03:59:19 bob Exp $

#include "recommenders.h"


////  RecommenderABC  ////

void
RecommenderABC::chosen_feature (ConstFeaturePtr f)
{
  if (f == mFeature)
    std::cout << "\nRCMD: *** Error *** Chosen feature was mine, but was not accepted.\n";
}

bool
RecommenderABC::has_a_feature()
{
  if (mFeature == 0) // try to make one
  { // std::cout << "RCMD: has_a_feature calls make_feature because recommender " << name() << " lacks a feature.\n";
    return make_feature();
  }
  else return true;
}

int
RecommenderABC::number_successes() const
{
  int count (0);
  for (unsigned int i=0; i<mHistory.size(); ++i)
    if (mHistory[i].first) ++count;
  return count;
}

void
RecommenderABC::print_to (std::ostream& os) const
{
  os << "Recommender " << mName
     << "["
     << mNumSinceUsed << " since used; " << number_recommended() << " rcmd; " << number_remaining() << " remain"
     << "]:";
}

void
RecommenderABC::update_history(FeatureABC const* pf, bool b)
{
  if(pf == mFeature)
  {
    if (b)
      mNumSinceUsed=0;
    else
      ++mNumSinceUsed;
    mHistory.push_back(std::make_pair(b,0.0));   // do not reveal p-value
    increment_position();
    make_feature();
  }
  else
  {
    std::cout << "RCMD: *** Error *** Update confused; received " << pf->name() 
              << " but expected " << mFeature->name() << std::endl;
  }
}
