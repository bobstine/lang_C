// $Id: recommenders.Template.h,v 1.19 2004/05/26 02:46:57 bob Exp $

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
  int n (mSource.size());
  if (mPosition >= n)
    mFeature = 0;
  else
  {
    while((mPosition < n) && mSource[mPosition]->was_tried_in_model())
      increment_position();
    mFeature = (mPosition < n) ? mSource[mPosition] : 0;
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
PermutationRecommender<Source>::make_feature ()
{
  int n (mSource.size());
  if (mPosition >= n || mPermutation[mPosition] >= (int) mSource.size())
    mFeature = 0;
  else
  {
    while((mPosition < n) && mSource[mPermutation[mPosition]]->was_tried_in_model())
      increment_position();
    mFeature = (mPosition < n) ? mSource[mPermutation[mPosition]] : 0;
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
  std::cout << "IRCM: Making interaction from #"<< mPos1 << " x #" << mPos2 << ".\n";
  int n (mSource.size());
  mFeature = 0;
  if (mPos2 < n)
  {
    FeatureABC const* x1 (mSource[mPos1]);
    FeatureABC const* x2 (mSource[mPos2]);
    if ((mPos1 == mPos2) && x1->is_dummy())
    { std::cout << "IRCM: Diagonal dummy at position " << mPos1 << std::endl;
      increment_position();
      x1 = mSource[mPos1];
      x2 = mSource[mPos2];
    }
    if (mPos2 == n) return;
    if ((mPos1 == mPos2) && x1->is_dummy())  // handle starting position
    { std::cout << "IRCM: Diagonal dummy at position " << mPos1 << std::endl;
      increment_position();
      x1 = mSource[mPos1];
      x2 = mSource[mPos2];
    }
    InteractionFeature *pf = mFactory->make_interaction_feature_ptr(x1, x2);
    mFeature = pf;
  }
}

///  Cross-product recommender  ////

template<class Source1, class Source2>
void
CrossProductRecommender<Source1, Source2>::increment_position()
{
  if (mFixedPos < (int)(mFixedSource.size()-1))
    ++mFixedPos;
  else
  { mFixedPos = 0;
    ++mDynPos;
  }
}


template<class Source1, class Source2>
void
CrossProductRecommender<Source1, Source2>::make_feature()
{
  std::cout << "PRCM: Making cross-product from fixed["<< mFixedPos << "] x dyn[" << mDynPos << "]\n";
  mFeature = 0;
  if (mDynPos < mDynSource.size())
  {
    FeatureABC const* xf (mFixedSource[mFixedPos]);
    FeatureABC const* xd (mDynSource[mDynPos]);
    InteractionFeature *pf = mFactory->make_interaction_feature_ptr(xf, xd);
    mFeature = pf;
  }
}


// XBRecommender  XBRecommender  XBRecommender  XBRecommender  XBRecommender  XBRecommender  

template<class Source, class Model>
void
XBRecommender<Source, Model>::make_feature ()
{
  int q (mModel.number_of_predictors());
  if ((q > 1) && (q > mModelSize))
  { std::ostringstream oss;
    oss << q;
    std::string name ("XB_" + oss.str());
    // ought to use this rather than recompute xb again
    // ColumnFeature* cf (mFF->make_column_feature_ptr(name, mModel.xb()));
    LinearCombinationFeature* xb (mFF->make_linear_combination_feature_ptr(mModel.beta(), mSource.features()));
    mFeature = mFF->make_unary_feature_ptr(Function_Utils::Square(), xb);
    mModelSize = q;
    std::cout << "MRCM: Made xb^2 for q=" << q << std::endl;
  }
  else
    mFeature = 0;
}

//  ModelFitRecommender  ModelFitRecommender  ModelFitRecommender  ModelFitRecommender  ModelFitRecommender

template<class Source, class Model>
void
ModelFitRecommender<Source, Model>::make_feature ()
{
  int q (mModel.number_of_predictors());
  if ((q > 1) && (q > mModelSize))
  { std::ostringstream oss;
    oss << q;
    std::string name ("Fit_" + oss.str());
    // ought to use this rather than recompute xb again
    // ColumnFeature* cf (mFF->make_column_feature_ptr(name, mModel.xb()));
    // mFeature = mFF->make_interaction_feature_ptr(cf,cf);
    LinearCombinationFeature* xb (mFF->make_linear_combination_feature_ptr(mModel.beta(), mSource.features()));
    mFeature = mFF->make_unary_feature_ptr(Function_Utils::LogisticPos(), xb);
    mModelSize = q;
    std::cout << "MRCM: Made fit^2 with q=" << q << std::endl;
  }
  else
    mFeature = 0;
}
