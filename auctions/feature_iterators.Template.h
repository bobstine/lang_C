#ifndef _FEATURE_ITERATORS_TEMPLATE_H_
#define _FEATURE_ITERATORS_TEMPLATE_H_

#include "feature_iterators.h"
#include "feature_predicates.Template.h"

#include "adapter.h"
#include "debug.h"

#include <assert.h>

//     BeamIterator     BeamIterator     BeamIterator     BeamIterator     BeamIterator     BeamIterator

template<class A>
void
BeamIterator<A>::print_to(std::ostream& os) const
{ os << "BeamIterator with " << mBeamFeatures.size() << " beams of sizes: ";
  for(size_t i=0; i<mBeamFeatures.size(); ++i)
    os << "(" << mBeamNames[i] << "," << mBeamFeatures[i].size() << ") ";
}	

template<class A>
void
BeamIterator<A>::init()
{ debugging::debug("BEAM",3) << "Initializing BeamIterator to watch " << mBeamFeatures.size() << " streams:";
  for(std::string n : mBeamNames) std::cout << " " << n;
  std::cout << std::endl;
  for(int i=0; i<number_of_beams(); ++i)
    for(int j=0; j<=i; ++j)
      mBeamFeaturesUsed[std::make_pair(i,j)] = std::make_pair(0,0);
}

template<class A>
std::vector<int>
BeamIterator<A>::current_beam_indices (int beam, int position) const
{
  assert(position < (int)mBeamFeatures[beam].size());
  std::vector<int> result;
  for(int i=position; i<(int)mBeamFeatures[beam].size(); ++i)
    result.push_back(mBeamFeatures[beam][i].first);
  return result;
}
      
template<class A>
bool
BeamIterator<A>::update_adds_to_beams()
{ debugging::debug("BEAM",3) << "BeamIterator::update_adds_to_beams, mLastQ=" << mLastQ << "; model now has " << mAuction.number_of_model_features() << " features." << std::endl;
  bool result = false;
  if (mLastQ < mAuction.number_of_model_features())  // need to add features
  { FeatureVector modelFeatures = mAuction.model_features();
    debugging::debug("BEAM",3) << "Update beam with vector of " << modelFeatures.size() << " model features.\n";
    for (int i=mLastQ; i<mAuction.number_of_model_features(); ++i)
    { Feature f = modelFeatures[i];
      debugging::debug("BEAM",4) << "Update examining feature " << f->name() << " from stream " << f->attribute_str_value("stream") << std::endl;
      for(int j=0; j<(int)mBeamNames.size(); ++j)
      { if(mBeamNames[j] == f->attribute_str_value("stream"))      // add this feature to named beam   ... ??? add a 'beam' attribute in the future rather than use stream name
	{ result=true;
	  mBeamFeatures[j].push_back(std::make_pair(i,f));
	}
      }
    }
  }
  mLastQ = mAuction.number_of_model_features();
  return result;
}

template<class A>
void
BeamIterator<A>::find_best_beam()
{ debugging::debug("BEAM",4) << "BeamIterator::find_best_beam\n";
  int bestSum = 0;
  for(int i=0; i<number_of_beams(); ++i)
    for (int j=0; j<=i; ++j)
    { std::pair<int,int> lastCount = mBeamFeaturesUsed[std::make_pair(i,j)];
      int sum = (int)(mBeamFeatures[i].size()+mBeamFeatures[j].size())-(lastCount.first + lastCount.second);
      if (bestSum < sum)
      { bestSum = sum;
	mBestBeam = std::make_pair(i,j);
      }
    }
}

template<class A>
bool
BeamIterator<A>::best_beam_is_okay() const
{ std::pair<int,int> lastCount = mBeamFeaturesUsed.find(mBestBeam)->second;
  return   (
	    ((lastCount.first +mGap) < (int)mBeamFeatures[mBestBeam.first].size()) ||
	    ((lastCount.second+mGap) < (int)mBeamFeatures[mBestBeam.second].size())
	    );
}



//  CyclicIterator     CyclicIterator     CyclicIterator     CyclicIterator     CyclicIterator     CyclicIterator

template<class Collection, class Pred>
  void
  CyclicIterator<Collection, Pred>::initialize()
{
  while(mSkipFeature(*mIter))
  { --mSize;
    ++mIter;
    if (mIter == mSource.end())
    { std::cerr << "FITR: *** ERROR *** Source for cyclic iterator is empty.\n";
      mIter = mSource.begin();
    }
  }
}

template<class Collection, class Pred>
  CyclicIterator<Collection, Pred>&
  CyclicIterator<Collection, Pred>::operator++()
{
  int size = (int) mSource.size();
  int count = (int) std::count_if(mSource.begin(), mSource.end(), mSkipFeature);
  mSize = size - count;
  if (mSize == 0)
    std::cerr << "FITR: Warning. Cyclic iterator has no more elements to increment\n";
  else
  { ++mIter;
    if(mIter == mSource.end())
      mIter = mSource.begin();
    while(mSkipFeature(*mIter))
    { ++mIter;
      if (mIter == mSource.end())
	mIter = mSource.begin();
    }
  }
  return *this;
}


//  ModelIterator     ModelIterator     ModelIterator     ModelIterator     ModelIterator     ModelIterator     ModelIterator     

template< class Model >
bool
ModelIterator<Model>::points_to_valid_data()  const
{
  if(mModel.q() <= (mLastQ + mSeparation))
    return false;
  // need to check name of last accepted; dont have one if its us
  std::string lastName (mModel.predictor_names().back());
  if (std::string::npos != lastName.find("Y_hat"))                          // npos means not found; != npos means found
  { debugging::debug("FTRS", 3) << "Found fit variable in most recent model; cannot build feature.\n";
    return false;
  }
  return true;
}



///////////////    Iteraction Streams    Iteraction Streams    Iteraction Streams    Iteraction Streams    Iteractgion Streams
//
//         f1    f2   f3   f4
//    f1    *     *    *    *
//    f2          *    *    *
//    f3               *    *
//    f4                    *
//                               mPos2 says which column; mPos1 tracks row, moving down from first row


template<class Source, class Pred>
  void
  InteractionIterator<Source,Pred>::initialize()
{
  if(!mIncludeDiagonal) ++mpColFeature;
  while(skip_current_pair() && mRemain)
    inc_pointers();
}


template<class Source, class Pred>
  int
  InteractionIterator<Source,Pred>::initial_count(int k)         const
{
  return (k*k-k)/2 + (mIncludeDiagonal?k:0);
}


template<class Source, class Pred>
  Feature
  InteractionIterator<Source,Pred>::operator*()   const
{
  assert(mpColFeature != mSource.end());
  assert(mRemain >= 0);
  return Feature(*mpDiagFeature, *mpColFeature);
}


template<class Source, class Pred>
  void
  InteractionIterator<Source,Pred>::inc_pointers()
{
  // knocks number remaining down by 1; only uses count as set initially based on diagonal of array
  //  std::cout << "                enter inc-pointers: " << (*mpDiagFeature)->name() << "  " << (*mpColFeature)->name()
  //            << " with " << mRemain << " remaining " <<std::endl;
  if(!--mRemain) return;
  ++mpColFeature; 
  if (mpColFeature == mSource.end())      // move diagonal "row" pointer
  { ++mpDiagFeature;
    mpColFeature = mpDiagFeature;
    if (!mIncludeDiagonal)
      ++mpColFeature;
  }
}



template<class Source, class Pred>
 bool
 InteractionIterator<Source,Pred>::skip_current_pair() const
{
  // skip constants
  if((*mpColFeature)->is_constant() || (*mpDiagFeature)->is_constant())
    return true;
  // dont square diagonal
  if((mpDiagFeature == mpColFeature) && (*mpDiagFeature)->is_dummy())
    return true;
  // apply external criterion
  if(mSkipPred(*mpDiagFeature, *mpColFeature))
  { debugging::debug("INTS",4) << " skipping pair: " << (*mpDiagFeature)->name() << " & " << (*mpColFeature)->name() << std::endl;
    return true;
  }
  return false;
}


template<class Source, class Pred>
  InteractionIterator<Source,Pred>&
  InteractionIterator<Source,Pred>::operator++()
{
  assert (mRemain > 0);        // starting condition
  inc_pointers();
  while(skip_current_pair() && mRemain)
   inc_pointers();
  return *this;
}


template<class Source, class Pred>
  void
  InteractionIterator<Source,Pred>::print_to(std::ostream &os)   const
{
  os << "InteractionIterator [" << mRemain << "] ";
  if(points_to_valid_data()) os << " @ " << (*mpDiagFeature)->name() << " x "<< (*mpColFeature)->name();
}



//   CrossProductIterator     CrossProductIterator     CrossProductIterator     CrossProductIterator     CrossProductIterator

template<class Pred>
bool
CrossProductIterator<Pred>::points_to_valid_data()             const
{
  update_index_vector();
  return (mSlowIndex < mSlowSource.size()) && (mFastIndices[mSlowIndex] < mFastSource.size());
}

template<class Pred>
int
CrossProductIterator<Pred>::number_remaining () const
{
  if (! points_to_valid_data() ) return 0;
  int n (0);
  for (unsigned iSlow=0; iSlow < mSlowSource.size(); ++iSlow)
    n += (int)mFastSource.size() - mFastIndices[iSlow];
  debugging::debug("CPIT",4) << " CP of vectors of sizes " << mSlowSource.size() << " x " << mFastSource.size()
			     << " has " << n << " remaining features." << std::endl;
  return n;
}

template<class Pred>
Feature
CrossProductIterator<Pred>::operator*()         const
{
  assert(points_to_valid_data());
  return Feature(mSlowSource[mSlowIndex], mFastSource[mFastIndices[mSlowIndex]]);
}


template<class Pred>
CrossProductIterator<Pred>&
CrossProductIterator<Pred>::operator++()
{
  // mark that last was used since return directly on stack
  ++mFastIndices[mSlowIndex];
  update_index_vector();                                                                                                                                  
  // find first not used
  mSlowIndex=0;
  bool keepSearching (true);
  while(keepSearching)
  { while(mSlowIndex < mSlowSource.size())
    { if (mFastIndices[mSlowIndex] < mFastSource.size())
      { keepSearching = false;
	break;
      }
      else
	++mSlowIndex;
    }
    if (mSlowIndex < mFastIndices.size())
    { keepSearching = mSkipPred(mSlowSource[mSlowIndex], mFastSource[mFastIndices[mSlowIndex]]);
      if (keepSearching)
      { debugging::debug("CPIT",3) << "Skipping cross product of " << mSlowSource[mSlowIndex]->name() << " x "
				   << mFastSource[mFastIndices[mSlowIndex]] << std::endl;
	++mFastIndices[mSlowIndex];
      }
      else
	debugging::debug("CPIT",3) << "Allowing cross product of " << mSlowSource[mSlowIndex]->name() << " x "
				   << mFastSource[mFastIndices[mSlowIndex]] << std::endl;
      debugging::debug("CPIT",4) << "Increment position to " << mSlowIndex << " x " << mFastIndices[mSlowIndex] << std::endl;
    }
    else
    { --mSlowIndex;              // keep position in valid range in case fast list expands
      keepSearching = false;     // none left to search
      debugging::debug("CPIT",4) << "Increment results in empty iterator.\n";
    }
  }
  return *this;
}


template<class Pred>
void
CrossProductIterator<Pred>::update_index_vector()    const
{
  while (mFastIndices.size() < mSlowSource.size())
    mFastIndices.push_back(0);
}

template<class Pred>
void
CrossProductIterator<Pred>::print_to (std::ostream& os) const
{ os << "CrossProductIterator with indices ";
  print_indices(os);
  if(points_to_valid_data())
    os << "@ " << mSlowSource[mSlowIndex]->name() << " x " << mFastSource[mFastIndices[mSlowIndex]]->name() << std::endl;
  else
    os << "is empty.\n";
}

template<class Pred>
void
CrossProductIterator<Pred>::print_indices(std::ostream& os)  const
{
  os << mSlowIndex << " { ";
  for (unsigned i=0; i<mFastIndices.size(); ++i)
    os << mFastIndices[i] << " ";
  os << "}";
}

#endif
