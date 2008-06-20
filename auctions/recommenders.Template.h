// $Id: recommenders.Template.h,v 3.11 2008/01/18 03:59:19 bob Exp $

///  Sequence recommender  ////

template<class Source>
void
SequenceRecommender<Source>::print_to(std::ostream& os) const
{
  RecommenderABC::print_to(os);
  os << " sequence @ " << mPosition;
}

template<class Source>
bool
SequenceRecommender<Source>::make_feature ()
{
  std::cout << "RCMS: " << name() << " making sequence feature at current position " << mPosition << ".\n";
  int n (mSource.size());
  mFeature = 0;
  if (mPosition < n)
  { while((mPosition < n) && mSource[mPosition]->was_tried_in_model())
      increment_position();
    mFeature = (mPosition < n) ? mSource[mPosition] : 0;
  }
  return (mFeature != 0);
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
bool
PermutationRecommender<Source>::make_feature ()
{
  std::cout << "RCMP: " << name() << " making permutation feature at current position " << mPosition << ".\n"; 
  mFeature = 0;
  int n (mSource.size());
  if (mPosition < n && mPermutation[mPosition] < (int) mSource.size())
  {
    while((mPosition < n) && mSource[mPermutation[mPosition]]->was_tried_in_model())
      increment_position();
    mFeature = (mPosition < n) ? mSource[mPermutation[mPosition]] : 0;
  }
  return (mFeature != 0);
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
bool
InteractionRecommender<Source>::make_feature()
{
  int n (mSource.size());
  // std::cout << " interaction rcmd " << name() << " making feature from source of size " << n << std::endl;
  mFeature = 0;
  if (mPos2 < n)
  {
    FeatureABC const* x1 (mSource[mPos1]);
    FeatureABC const* x2 (mSource[mPos2]);
    if ((mPos1 == mPos2) && x1->is_dummy())
    { std::cout << "RCMI: Diagonal dummy at position " << mPos1 << std::endl;
      increment_position();
      x1 = mSource[mPos1];
      x2 = mSource[mPos2];
    }
    if (mPos2 == n) return (mFeature != 0);
    if ((mPos1 == mPos2) && x1->is_dummy())  // handle starting position
    { std::cout << "RCMI: Diagonal dummy at position " << mPos1 << std::endl;
      increment_position();
      x1 = mSource[mPos1];
      x2 = mSource[mPos2];
    }
    std::cout << "RCMI: " << name() << " making interaction from #"<< mPos1 << " x #" << mPos2 << ".\n";
    InteractionFeature *pf = mFactory->make_interaction_feature_ptr(x1, x2);
    mFeature = pf;
  }
  return (mFeature != 0);
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
bool
CrossProductRecommender<Source1, Source2>::make_feature()
{
  mFeature = 0;
  if (mDynPos < mDynSource.size())
  {
    std::cout << "RCMC: " << name() << " making cross-product from fixed["<< mFixedPos << "] x dyn[" << mDynPos << "].\n";
    FeatureABC const* xf (mFixedSource[mFixedPos]);
    FeatureABC const* xd (mDynSource[mDynPos]);
    InteractionFeature *pf = mFactory->make_interaction_feature_ptr(xf, xd);
    mFeature = pf;
  }
  return (mFeature != 0);
}


// XBRecommender  XBRecommender  XBRecommender  XBRecommender  XBRecommender  XBRecommender  

template<class Source, class Model>
bool
XBRecommender<Source, Model>::make_feature ()
{
  int q (mModel.number_of_predictors());
  std::cout << "RCMM: " << name() << " making xb^2 for q=" << q << std::endl;
  mFeature = 0;
  if ((q > 1) && (q > mModelSize))
  { std::ostringstream oss;
    oss << q;
    std::string name ("XB_" + oss.str());
    // ought to use this rather than recompute xb again
    // ColumnFeature* cf (mFF->make_column_feature_ptr(name, mModel.xb()));
    LinearCombinationFeature* xb (mFF->make_linear_combination_feature_ptr(mModel.beta(), mSource.features()));
    mFeature = mFF->make_unary_feature_ptr(Function_Utils::Square(), xb);
    mModelSize = q;
  }
  return (mFeature !=0);
}

//  ModelFitRecommender  ModelFitRecommender  ModelFitRecommender  ModelFitRecommender  ModelFitRecommender

template<class Source, class Model>
bool
ModelFitRecommender<Source, Model>::make_feature ()
{
  mFeature = 0;
  int q (mModel.number_of_predictors());
  std::cout << "RCMM: " << name() << " making model fit features with q=" << q  << std::endl;;
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
  }
  return (mFeature != 0);
}



//  GarbageRecommender  GarbageRecommender  GarbageRecommender  GarbageRecommender  GarbageRecommender  

template <class Model>
int   
GarbageRecommender<Model>::number_remaining()  const
{ 
  int result = 0;
  int num_used = mModel.number_of_predictors();
  int num_tried = mModel.number_of_features();
  if ((num_used > 1) && (num_tried > 2 * num_used + 1.3 * mLast))  result = 1;
  return result;
}

template <class Model>
void  
GarbageRecommender<Model>::print_to(std::ostream& os) const  
{ 
  RecommenderABC::print_to(os); 
  os << "FitTrash: "
    << mModel.features().size() << " " 
    << mModel.number_of_predictors() << std::endl;
}


//  FitTrashRecommender  FitTrashRecommender  FitTrashRecommender  FitTrashRecommender  FitTrashRecommender

/*
 template<class Model>
std::string
FitTrashRecommender<Model>::make_feature ()
{
  std::stringstream s;
  int q (mModel.number_of_predictors());
  if(number_remaining() > 0)
  { std::ostringstream oss;
    oss << q;
    std::string name ("FitTrash_" + oss.str());
    // ought to use this rather than recompute xb again
    // ColumnFeature* cf (mFF->make_column_feature_ptr(name, mModel.xb()));
    // mFeature = mFF->make_interaction_feature_ptr(cf,cf);

    std::cout << "TRASH: sizes: beta = " << mModel.model().beta().size() 
 	      << ", model_features = " << mModel.model_features().size() 
 	      << ", number_of_features = " << mModel.number_of_features() 
 	      << ", predictors = " << mModel.number_of_predictors() 
 	      << ", features.size = " << mModel.features().size() 
 	      << std::endl; 


    std::vector<double> myBeta = mModel.model().beta();
    if(myBeta.size() != mModel.model_features().size()+1)
      myBeta.pop_back();  
    if(myBeta.size() != mModel.model_features().size()+1)
      myBeta.pop_back();  
    
    //    std::cout << "TRASH:*lcf 1: " << myBeta.size() << " x " << mModel.model_features().size() << std::endl;
    LinearCombinationFeature* xb (mFF->make_linear_combination_feature_ptr(myBeta,
									   mModel.model_features()));
    FeaturePtr yhat = mFF->make_unary_feature_ptr(Function_Utils::LogisticPos(), xb);
    int n = end(yhat->range())- begin(yhat->range());
    double Sy = range_ops::accumulate(yhat->range(),0.0);
    double Tyy = range_ops::inner_product(yhat->range(),yhat->range(),  0.0);
    double Syy = Tyy - Sy * Sy / n;
    
    mModelSize = q;

    // * * * * * ** * ** * * * *
    // CAUTION: MEN AT WORK
    // * * * * * ** * ** * * * *

    std::vector<double> coefficients(mModel.features().size()+1);

    int num_Xs = int(mModel.features().size());
    for(int i = 0; i < num_Xs;++i)
      { // compute beta-hat = (x'x)^-1 x'y
	ConstFeaturePtr mX = mModel.features()[i];
	double Txy = range_ops::inner_product(mX->range(),yhat->range(),0.0);
	double Txx = range_ops::inner_product(mX->range(),mX->range(),  0.0);
	double Sx = range_ops::accumulate(mX->range()  ,0.0);
	double Sxx = Txx - Sx * Sx / n;
	double Sxy = Txy - Sx * Sy / n;

	double y_hat_slope = (Sxy - Sx * Sy / n) / Sxx;
	if((Txx != 0) && Sxx != 0)
	  coefficients[i+1] = y_hat_slope;
      }
    //    std::cout << "TRASH: lcf 2: " << coefficients.size() << " x " << num_Xs << std::endl;

    LinearCombinationFeature* result (mFF->make_linear_combination_feature_ptr(coefficients,
									       mModel.features()));

    double r_sq = 0;
    {
      double Txy = range_ops::inner_product(result->range(),yhat->range(),0.0);
      double Txx = range_ops::inner_product(result->range(),result->range(),  0.0);
      double Sx = range_ops::accumulate(result->range()  ,0.0);
      double Sxy = Txy - Sx * Sy / n;  // centered version
      double Sxx = Txx - Sx * Sx / n;  // centered version
      r_sq = Sxy * Sxy / (Sxx * Syy);
    }
    //    std::cout << "TRASH: generated " << num_Xs << " go generate r-sq of " << r_sq
    //	      << " for " << result->name() << std::endl;
    mFeature = result;

    // * * * * * ** * ** * * * *
    // CAUTION: END (men at work)
    // * * * * * ** * ** * * * *

    mLast = num_Xs;
  }
  else
    mFeature = 0;
  return s.str();
}
*/
