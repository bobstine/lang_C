#include "adapter.h"
#include "debug.h"

#include <assert.h>


//  FeatureStream     FeatureStream     FeatureStream     FeatureStream     FeatureStream     FeatureStream

template<class Iterator, class Trans>
  bool
  FeatureStream<Iterator,Trans>::has_feature ()
{
  if (!mHead.empty())
    return true;
  else if (mIterator.valid())
  { mHead = mTransform(*mIterator);
    ++mIterator;
  }
  else
    debugging::debug("RGST",3) << "has_feature finds that stream '" << name() <<"' cannot build more features.\n";
  return (!mHead.empty());                                      // may not have been able to build one 
}
  

//  CyclicIterator     CyclicIterator     CyclicIterator     CyclicIterator     CyclicIterator     CyclicIterator     


template<class Collection, class Pred>
  CyclicIterator<Collection, Pred>&
  CyclicIterator<Collection, Pred>::operator++()
{
  ++mIter;
  if(mIter == mSource.end())
    mIter = mSource.begin();
  while(mSkipFeature(*mIter) && (mSize > 0))
  { --mSize;
    ++mIter;
    if (mIter == mSource.end())
      mIter = mSource.begin();
  }
  return *this;
}


//  ModelIterator     ModelIterator     ModelIterator     ModelIterator     ModelIterator     ModelIterator     ModelIterator     

template< class Model >
bool
ModelIterator<Model>::valid()  const
{
  if(mLastQ == mModel.q())
    return false;
  // need to check name of last accepted; dont have one if its us
  std::string lastName (mModel.accepted_features().back()->name());
  if (std::string::npos != lastName.find("Y_hat"))                          // npos means not found; != npos means found
  { debugging::debug("FITS", 3) << "Found fit variable in most recent model; cannot build feature.\n";
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
  InteractionIterator<Source,Pred>::inc_pointers()
{
  ++mpColFeature; --mRemain;
  if (mpColFeature == mSource.end())   // move diagonal "row" pointer
  { ++mpDiagFeature;
    mpColFeature = mpDiagFeature;
    if (!mIncludeDiagonal)
      ++mpColFeature;
    else if ((*mpDiagFeature)->is_dummy())  // dont square a dummy
    { ++mpColFeature;
      -- mRemain;
    }
  }
}


template<class Source, class Pred>
  InteractionIterator<Source,Pred>&
  InteractionIterator<Source,Pred>::operator++()
{
  if(mRemain == 0) return *this;
  // skip constants
  while ((*mpDiagFeature)->is_constant())
    inc_pointers();
  // need different parents
  while(mRemain > 0 && mSkipPred(*mpDiagFeature, *mpColFeature))
  { debugging::debug("INTS",4) << " skipping pair: " << (*mpDiagFeature)->name() << " & " << (*mpColFeature)->name() << std::endl;
    inc_pointers();
  }
  return *this;
}





