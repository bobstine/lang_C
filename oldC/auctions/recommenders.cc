// $Id: recommenders.cc,v 1.12 2004/04/23 19:30:41 bob Exp $

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
    make_feature();
  return (mFeature != 0);
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
RecommenderABC::update_history(bool b, double p)
{
  if (b)
    mNumSinceUsed=0;
  else
    ++mNumSinceUsed;
  mHistory.push_back(std::make_pair(b,p));
  mFeature->set_model_results(b, p);
  increment_position();
  make_feature();
}
