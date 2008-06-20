// $Id: recommenders.Template.h,v 3.0 2004/11/19 18:58:36 foster Exp $


///  Sequence recommender  ////

template<class Source>
void
SequenceRecommender<Source>::print_to(std::ostream& os) const
{
  RecommenderABC::print_to(os);
  os << " sequence @ " << mPosition;
}

template<class Source>
void
SequenceRecommender<Source>::make_feature ()
{
  typename Source::Vector fv = mSource.features();
  mFeature = Feature();
  int n (fv.size());
  if (mPosition < n)
  {
    while(fv[mPosition].was_tried_in_model())
      increment_position();
    std::cout << "RCMD: Sequence recommender at position " << mPosition << std::endl;
    if (mPosition < n)
    { mFeature = fv[mPosition];
      std::cout << "RCMD: Make feature addr " << &mFeature << " for feature " << mFeature << std::endl;
    }
  }
}


///  Permutation recommender  ////

template<class Source>
void
PermutationRecommender<Source>::print_to(std::ostream& os) const
{
  RecommenderABC::print_to(os);
  os << " permutation @ " << mPosition << " -> " << mPermutation[mPosition];
}


template<class Source>
void
PermutationRecommender<Source>::chosen_feature (Feature const& f)
{
  RecommenderABC::chosen_feature(f);
  if (f == mFeature)
  { increment_position();
    make_feature();
  }
}

template<class Source>
void
PermutationRecommender<Source>::make_feature ()
{
  typename Source::Vector fv = mSource.features();
  int n (fv.size());
  mFeature = Feature();
  if (mPosition < (int) mPermutation.size() && mPermutation[mPosition] < (int) fv.size())
  {
    while(fv[mPermutation[mPosition]].was_tried_in_model())
      increment_position();
    if (mPosition < n)
      mFeature = fv[mPermutation[mPosition]];
  }
}

template<class Source>
void
PermutationRecommender<Source>::make_permutation ()
{
  int n (mPermutation.size());
  for (int i=0; i<n; ++i)    mPermutation[i] = i;
  std::random_shuffle(mPermutation.begin(), mPermutation.end());
}

///  Interaction recommender  ////


template<class Source>
void
InteractionRecommender<Source>::increment_position()
{
  typename Source::Vector fv = mSource.features();
  if (mPos1 == 0) // move to next column
  { ++mPos2;
    mPos1 = mPos2;
  }
  else
    --mPos1;
}


template<class Source>
void
InteractionRecommender<Source>::make_feature()
{
  std::cout << "ICRM: Making feature at ("<< mPos1 << "," << mPos2 << ")\n";
  typename Source::Vector fv = mSource.features();
  int n (fv.size());
  mFeature = Feature();
  if (mPos2 < n)
  {
    Feature x1 (fv[mPos1]);
    Feature x2 (fv[mPos2]);
    if ((mPos1 == mPos2) && x1.is_dummy())
    { std::cout << "ICRM: Diagonal dummy at position " << mPos1 << std::endl;
      increment_position();
      x1 = fv[mPos1];
      x2 = fv[mPos2];
    }
    if (mPos2 == n) return;
    if ((mPos1 == mPos2) && x1.is_dummy())  // handle starting position
    { std::cout << "ICRM: Diagonal dummy at position " << mPos1 << std::endl;
      increment_position();
      x1 = fv[mPos1];
      x2 = fv[mPos2];
    }
    mFeature = make_interaction_feature(x1, x2);
  }
}
