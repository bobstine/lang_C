#ifndef _FEATURE_ITERATORS_H_
#define _FEATURE_ITERATORS_H_

#include "features.h"

// for finite streams
#include <queue>
#include <iostream>



class BidOrder
{
public:
  bool operator()(Feature const& a, Feature const& b) const { return (a->entry_bid() < b->entry_bid()); }
};



//     QueueIterator     QueueIterator     QueueIterator     QueueIterator     QueueIterator     QueueIterator     QueueIterator     

class FeatureQueue;

class RefCountedQueue
{
public:
  typedef std::priority_queue<Feature, FeatureVector, BidOrder> Queue;
  Queue   mQueue;
  int     mRefCount;
  
  ~RefCountedQueue() {  }
  
  template<class Collection>
  RefCountedQueue(Collection const& c): mQueue(), mRefCount(1)
    { for (typename Collection::const_iterator it=c.begin(); it!=c.end(); ++it) if (! (*it)->is_constant() ) mQueue.push(*it); }
};


template< class Collection, class SkipPredicate >
class QueueIterator
{
  RefCountedQueue *mpQueue;
  SkipPredicate    mSkipPred;
public:
  ~QueueIterator() { if(--mpQueue->mRefCount == 0) delete mpQueue; }
  
  QueueIterator(Collection const& c, SkipPredicate p) : mpQueue(new RefCountedQueue(c)), mSkipPred(p) { }
  QueueIterator(QueueIterator const& queue)    : mpQueue(queue.mpQueue), mSkipPred(queue.mSkipPred) { ++mpQueue->mRefCount; }

  int    number_remaining()             const { return mpQueue->mQueue.size(); }
  bool   valid()                        const { return !mpQueue->mQueue.empty(); }
  
  QueueIterator&   operator++()               { assert(valid()); mpQueue->mQueue.pop();
                                                while( (!mpQueue->mQueue.empty()) && mSkipPred(mpQueue->mQueue.top()) ) mpQueue->mQueue.pop();
						return *this; }

  Feature          operator*()          const { assert(valid()); return mpQueue->mQueue.top(); }
    
  void print_to(std::ostream& os)       const { os << "QueueIterator: " << number_remaining() << " features remain; queue ref count " << mpQueue->mRefCount; }

  //  RefCountedQueue::Queue *operator->()  const { return &mpQueue->mQueue; }  // others dont need access to underlying queue
};

template<class Collection, class Pred>
std::ostream&
operator<< (std::ostream& os, QueueIterator<Collection,Pred> const& queue) { queue.print_to(os); return os; }



//     DynamicIterator     DynamicIterator     DynamicIterator     DynamicIterator     DynamicIterator     DynamicIterator     DynamicIterator     
  
template<class Collection, class SkipPredicate>                                  //     waits for source to grow
class DynamicIterator
{
  Collection const& mSource;         // someone else maintains; must be random accessible
  unsigned int      mPosition;
  SkipPredicate     mSkipFeature;
  
public:
  DynamicIterator(Collection const& source, SkipPredicate pred)
    : mSource(source), mPosition(0), mSkipFeature(pred) {  }

  int   number_remaining()              const { return mSource.size() - mPosition; }
  bool  valid()                         const { return mPosition < mSource.size(); }

  DynamicIterator& operator++()               { assert(valid()); ++mPosition; while( (mPosition < mSource.size()) && mSkipFeature(mSource[mPosition]))  ++mPosition;  return *this;}

  Feature          operator*()          const { assert(valid()); return mSource[mPosition]; }

  void  print_to(std::ostream& os)      const { os << "DynamicIterator [" << mPosition << "/" << mSource.size() << "] "; }
};

template <class Collection, class Pred>
std::ostream&
operator<< (std::ostream& os, DynamicIterator<Collection,Pred> const& it) { it.print_to(os); return os; }



//     CyclicIterator     CyclicIterator     CyclicIterator     CyclicIterator     CyclicIterator     CyclicIterator

template<class Collection, class SkipPredicate>                                   // CyclicIterator    repeats over and over though collection
class CyclicIterator
{
  typedef typename Collection::const_iterator Iterator;

  Collection const& mSource;         // someone else maintains
  Iterator          mIter;
  int               mSize;
  SkipPredicate     mSkipFeature;
  
public:
  CyclicIterator(Collection const& source, SkipPredicate pred)
    : mSource(source), mIter(source.begin()), mSize(source.size()), mSkipFeature(pred) { }
  
  int   number_remaining()              const { return mSize; }             // number not used in model
  bool  valid()                         const { return !mSource.empty() && (mSize > 0); }

  CyclicIterator& operator++();
  Feature         operator*()           const { return *mIter; }

  void  print_to(std::ostream& os)      const { os << "CyclicIterator @ "; if (valid()) os << *mIter << " ";  else os << " empty "; }
};

template <class Collection, class Pred>
std::ostream&
operator<< (std::ostream& os, CyclicIterator<Collection,Pred> const& it) { it.print_to(os); return os; }



//     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator

class LagIterator
{
  const Feature   mFeature;       // construct lags of this feature
  const int       mBlockSize;     // blocking factor used if longitudinal
  int             mRemaining;
  int             mLag;           // current lag
  int             mMaxLag;        // cycle through the lags
  
public:  
  LagIterator(Feature const& f, int maxLag, int cycles, int blockSize)
    :  mFeature(f), mBlockSize(blockSize), mRemaining(maxLag*cycles), mLag(1), mMaxLag(maxLag) {  }
  
  int   number_remaining()         const   { return  mRemaining; }
  bool  valid()                    const   { return  mRemaining > 0; }

  LagIterator&  operator++();
  Feature       operator*()        const   { return  Feature(mFeature,mLag,mBlockSize); }

  void  print_to(std::ostream& os) const { os << "LagIterator @ " << mLag << "/" << mMaxLag << " with " << mRemaining << " left. "; }
};

inline
std::ostream&
operator<< (std::ostream& os, LagIterator const& it) { it.print_to(os); return os; }



//     ModelIterator     ModelIterator     ModelIterator     ModelIterator     ModelIterator     ModelIterator     ModelIterator     

template< class Model >
class ModelIterator
{
  Model const& mModel;    // maintained by someone else
  int          mLastQ;
public:
  ModelIterator(Model const& m): mModel(m), mLastQ(0) {}

  bool   valid()                   const;
  ModelIterator&  operator++()           { return *this; }
  Model const&    operator*()            { mLastQ = mModel.q(); return mModel; }
  void  print_to(std::ostream& os) const { os << "ModelIterator, last q=" << mLastQ << "; model @ " << mModel.q() << " "; }
};
 
template <class Model>
std::ostream&
operator<< (std::ostream& os, ModelIterator<Model> const& it) { it.print_to(os); return os; }



//     BundleIterator     BundleIterator     BundleIterator     BundleIterator     BundleIterator     BundleIterator     BundleIterator

template< class Collection, class SkipPred >   // must be random access collection
class BundleIterator
{
  Collection const&  mSource;      // maintained by someone else
  unsigned int       mBundleSize;
  SkipPred           mSkipPred;
  unsigned int       mLoIndex, mHiIndex;
public:
  BundleIterator(Collection const& source, int bundleSize, SkipPred pred) : mSource(source), mBundleSize(bundleSize), mSkipPred(pred), mLoIndex(0), mHiIndex(0) { }

  bool     valid()              const    { return (mSource.size()-mLoIndex) > mBundleSize; } 

  BundleIterator& operator++()           { return *this; }

  FeatureVector   operator*()   
    { FeatureVector fv;
      while(fv.size() < mBundleSize && mLoIndex < mSource.size())
      { // std::cout << " bundle checking feature " << mSource[mLoIndex]->name() << std::endl;
	if (!mSkipPred(mSource[mLoIndex]))
	  fv.push_back(mSource[mLoIndex]);
	++mLoIndex; 
      }
      return fv;
    }
      
  void print_to(std::ostream& os) const { os << "BundleIterator @ [" << mLoIndex << "," << mHiIndex << "] " ; }
};

template <class Collection, class Pred>
std::ostream&
operator<< (std::ostream& os, BundleIterator<Collection,Pred> const& it) { it.print_to(os); return os; }



//    InteractionIterator      InteractionIterator      InteractionIterator      InteractionIterator      InteractionIterator      InteractionIterator      

template<class Collection, class SkipPred>                         // static collection
class InteractionIterator
{
private:
  typedef typename Collection::const_iterator Iter;

  Collection const& mSource;
  bool              mIncludeDiagonal;
  SkipPred          mSkipPred;
  Iter              mpDiagFeature, mpColFeature;
  int               mRemain;
  
public:
  
  InteractionIterator(Collection const& src, bool useSquares, SkipPred pred)
    : mSource(src), mIncludeDiagonal(useSquares), mSkipPred(pred),
      mpDiagFeature(src.begin()), mpColFeature(src.begin()), mRemain(initial_count(src.size())) { if(!mIncludeDiagonal) ++mpColFeature;  }
  
  int   number_remaining()           const { return mRemain; }
  bool  valid ()                     const { return mRemain > 0; }
  InteractionIterator& operator++();  
  Feature              operator*()   const { assert(mpColFeature != mSource.end()); return Feature(*mpDiagFeature, *mpColFeature); }
  
  void  print_to(std::ostream &os)   const { os << "InteractionIterator [" << mRemain << "] ";
                                             if(valid()) os << " @ " << (*mpDiagFeature)->name() << " x "<< (*mpColFeature)->name(); }
private:
  int   initial_count(int k)         const { return (k*k-k)/2 + (mIncludeDiagonal?k:0); }
  void  inc_pointers();
};


template <class Collection, class Pred>
std::ostream&
operator<< (std::ostream& os, InteractionIterator<Collection,Pred> const& it) { it.print_to(os); return os; }



//     CrossProductIterator     CrossProductIterator     CrossProductIterator     CrossProductIterator     CrossProductIterator     CrossProductIterator     

/*
  Combines two dynamically growing sources using iterators without references to the
  collections themselves. In order to go forward, use reverse iterators in backwards order. Yuk.
*/
  

template <class Collection>
class CrossProductIterator
{
  typedef typename Collection::const_reverse_iterator Iterator;
  
  unsigned int            mSlowPosition;
  Iterator                mSlowIterator;    // position in slow list 
  const Iterator          mSlowEnd;         // end of slow list
  Iterator                mFastIterator;    // use to tack on 
  std::vector<Iterator>   mFastIterators;   // may get zeros tacked onto end; get one for each slow iterator
  
public:
    
  CrossProductIterator(Collection const& slow, Collection const& fast)
    : mSlowIterator(slow.rend()), mFastIterators() { update_iterator_vector(); }
  
  int   number_remaining()          const { std::cout << "WARNING: call to number remaining in dynamic stream.\n"; return 0; }   //  for interface only

  bool  valid()                           { update_iterator_vector(); return (mSlowIterator != mSlowSource.end()) && (mFastIterators[mSlowPosition] != mFastSource.end()); }

  CrossProductIterator& operator++()
    {
      ++mFastIterators[mSlowPosition];
      update_iterator_vector();                                                                                                      
      for(typename std::vector<Iterator>::const_iterator it=mFastIterators.begin(); it!=mFastIterators.end(); ++it)
      { if (*it != mFastSource.end())                         // find first available cross product 
	  break;                                  
	else                                                                                                                         
	{ ++mSlowPosition; ++mSlowIterator; }
      }
      return *this;
    }
  
  Feature               operator*()/*const*/     { assert(valid()); return Feature(*mSlowIterator,*mFastIterators[mSlowPosition]); }

  void               print_to(std::ostream& os)  { os << "CrossProduct iter "; if (valid()) os << "@ " << (*mSlowIterator)->name()
							   << " x " << (*mFastIterators[mSlowPosition])->name(); else os << "empty "; }
private:
  void  update_iterator_vector() 
  { while (mFastIterators.size() < mSlowSource.size())
    { mSlowPosition = 0; mSlowIterator = mSlowSource.begin();
      mFastIterators.push_back(mFastSource.begin());
      std::cout << "\nadded fast iterator; slow size " << mSlowSource.size() << "-" << mFastIterators.size() << " fast " << mFastSource.size() <<"\n";
    }
  }
};


template <class Collection>
std::ostream&
operator<< (std::ostream& os, CrossProductIterator<Collection>& it) { it.print_to(os); return os; }


#include "feature_iterators.Template.h"

#endif