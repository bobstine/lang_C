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
  else if (can_build_more_features())
    build_next_feature();                                   // advance position using current state
  else
    debugging::debug("RGST",3) << "FeatureStream '" << name() <<"' cannot build more features.\n";
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
ModelIterator<Model>::empty()  const
{
  if(mLastQ == mModel.q() || mModel.q()==0)
    return true;
  // need to check name of last accepted; dont have one if its us
  std::string lastName (mModel.accepted_features().back()->name());
  if (std::string::npos != lastName.find("Y_hat"))                          // npos means not found; != npos means found
  { debugging::debug("FITS", 3) << "Found fit variable in most recent model; cannot build feature.\n";
    return true;
  }
  return false;
}



///////////////    Iteraction Streams    Iteraction Streams    Iteraction Streams    Iteraction Streams    Iteractgion Streams
//
//         f1    f2   f3   f4
//    f1    *     *    *    *
//    f2          *    *    *
//    f3               *    *
//    f4                    *
//                               mPos2 says which column; mPos1 tracks row, moving down from first row


template<class Source>
void
InteractionStream<Source>::inc_pointers()
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


template<class Source>
bool
InteractionStream<Source>::find_next_position()
{
  inc_pointers();
  if (mRemain == 0) return false;
  // skip constants
  while ((*mpDiagFeature)->is_constant())
    inc_pointers();
  // need different parents
  while(mRemain > 0 && indicators_from_same_parent(*mpDiagFeature, *mpColFeature))
  { debugging::debug("INTS",4) << name() << " encountered features with common parent: " << (*mpDiagFeature)->name() << " & " << (*mpColFeature)->name() << std::endl;
    inc_pointers();
  }
  if (mRemain == 0)
  { debugging::debug("INTS",4) << name() << " has run out of features.\n";
    return false;
  }
  return true;
}




//   SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream     SubspaceStream


//  show names
  //  for (FeatureVector::const_iterator it = mBundle.begin(); it != mBundle.end(); ++it)   std::cout << (*it)->name() << " "; std::cout << std::endl;

  //  Dump the bundle to a file to see what's going on: stream to file and then transpose
  /*  
      std::string file ("/Users/bob/C/auctions/data/anes/bundle.txt");
      std::ofstream bs (file.c_str());
      if (bs)
      { debugging::debug("SUBS", 3) << "Writing bundle to file " << file << std::endl;
        for (unsigned int j=0; j<mBundle.size(); ++j)
        { mBundle[j]->write_values_to(bs);
          bs << std::endl;  }}
      else  debugging::debug("SUBS", 3) << " Non-fatal error; could not open file " << file << " to dump feature bundle.\n ";
  */


template< class Pred, class Trans>
  bool
  SubspaceStream<Pred, Trans>::can_build_more_features(FeatureList const& accepts, FeatureList const& rejects) 
{
  FeatureIterator theEnd ((mID==acceptedStreamID)?accepts.end():rejects.end());
  while ((mIterator != theEnd) && ( (int)mBundle.size() < mBundleSize))
  { if(mPredicate(*mIterator))
      mBundle.push_back(*mIterator);
    ++mIterator;
  }
  return mBundleSize == mBundle.size();
}


template<class Pred, class Trans>
  void
  SubspaceStream<Pred, Trans>::build_next_feature()
{
  debugging::debug("SUBS",4) << mName << " building bundle with " << mBundle.size() << " elements.\n";
   // apply transformation to the block of variables
  set_head(mTransformation(mBundle));
  mBundle.clear();
  debugging::debug("SUBS",4) << mName << " transformation completed.\n";  
}

  



