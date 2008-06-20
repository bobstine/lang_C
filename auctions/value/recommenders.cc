// $Id: recommenders.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

#include "recommenders.h"


////  RecommenderABC  ////


void
RecommenderABC::chosen_feature(Feature const& f)
{
  if (f == mFeature)
    std::cout << "RCMD: ERROR --- Chosen feature match.\n";
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
  mFeature.set_model_results(b, p);
  increment_position();
  make_feature();
}
