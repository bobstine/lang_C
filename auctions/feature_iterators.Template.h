#include "adapter.h"
#include "debug.h"

#include <assert.h>


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
  std::string lastName (mModel.predictor_names().back());
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
  if(valid()) os << " @ " << (*mpDiagFeature)->name() << " x "<< (*mpColFeature)->name();
}



