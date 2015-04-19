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

  int    number_remaining()             const { return (int) mpQueue->mQueue.size(); }
  bool   points_to_valid_data()         const { return !mpQueue->mQueue.empty(); }
  
  QueueIterator&   operator++()               { assert(points_to_valid_data()); mpQueue->mQueue.pop();
                                                while( (!mpQueue->mQueue.empty()) && mSkipPred(mpQueue->mQueue.top()) ) mpQueue->mQueue.pop();
						return *this; }

  Feature          operator*()          const { assert(points_to_valid_data()); return mpQueue->mQueue.top(); }
    
  void print_to(std::ostream& os)       const { os << "QueueIterator: " << number_remaining() << " features remain; queue ref count " << mpQueue->mRefCount; }
  
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
  bool              mNeedToCheck;
  SkipPredicate     mSkipFeature;
  
public:
  DynamicIterator(Collection const& source, SkipPredicate pred)
    : mSource(source), mPosition(0), mNeedToCheck(true), mSkipFeature(pred) { advance_position(); }

  int   number_remaining()              const { return (int)mSource.size() - (int)mPosition; }
  bool  points_to_valid_data()                { if(mNeedToCheck) advance_position();  return mPosition < mSource.size(); }

  DynamicIterator& operator++()               { assert(mPosition < mSource.size()); ++mPosition; advance_position(); return *this;}
  
  Feature          operator*()          const { assert(mPosition < mSource.size()); return mSource[mPosition]; }

  void  print_to(std::ostream& os)      const { os << "DynamicIterator [" << mPosition << "/" << mSource.size() << "] "; }
  
 private:
  void advance_position()                     { while((mPosition < mSource.size()) && mSkipFeature(mSource[mPosition]))  ++mPosition; mNeedToCheck = (mPosition == mSource.size()); }
};

template <class Collection, class Pred>
std::ostream&
operator<< (std::ostream& os, DynamicIterator<Collection,Pred> const& it) { it.print_to(os); return os; }



//     CyclicIterator     CyclicIterator     CyclicIterator     CyclicIterator     CyclicIterator     CyclicIterator

template<class Collection, class SkipPredicate>                                   // CyclicIterator    repeats over and over though collection
class CyclicIterator
{
  typedef typename Collection::const_iterator Iterator;

  Collection     mSource;
  SkipPredicate  mSkipFeature;
  size_t         mPosition;
  
public:
  CyclicIterator(Collection source, SkipPredicate pred)
    : mSource(source), mSkipFeature(pred), mPosition(0) { initialize(); }
  
  int   number_remaining()              const { return (int) mSource.size(); }
  bool  points_to_valid_data()          const { return !mSource.empty(); }

  CyclicIterator& operator++();
  Feature         operator*()           const { return mSource[mPosition]; }

  void  print_to(std::ostream& os)      const { os << "CyclicIterator @ "; if (points_to_valid_data()) os << mSource[mPosition] << " ";  else os << " empty "; }

 private:
  void initialize();
};

template <class Collection, class Pred>
std::ostream&
operator<< (std::ostream& os, CyclicIterator<Collection,Pred> const& it) { it.print_to(os); return os; }



//     EigenwordIterator     EigenwordIterator     EigenwordIterator     EigenwordIterator     EigenwordIterator

//     Never used as other iterators in this collection since it *builds* the features on the
//     fly rather than indexing into an external collection as expected in auction.

/*
#include "simple_vocabulary.h"
#include "simple_eigenword_dictionary.h"

class EigenwordIterator
{
  std::string                      mName;
  std::vector<std::string>         mTokens;
  Text::SimpleEigenwordDictionary  mDictionary;
  size_t                           mEigenDim;
  std::vector<bool>                mUsedDim;
  size_t                           mPosition;
  size_t                           mNumberRemaining;
  
public:
 EigenwordIterator(std::string name, std::vector<std::string> tokens, size_t dim, Text::SimpleEigenwordDictionary const& dict)
   : mName(name),
    mTokens(tokens), mDictionary(dict), mEigenDim(dim),
    mUsedDim(std::vector<bool>(dim,false)), mPosition(0), mNumberRemaining(dim) { }
  
  int   number_remaining()              const { return (int)mNumberRemaining; }
  bool  points_to_valid_data()          const { return (mNumberRemaining > 0); }

  EigenwordIterator& operator++()            ;
  Feature            operator*()        const;

  void  print_to(std::ostream& os)      const;
};


inline
std::ostream&
operator<< (std::ostream& os, EigenwordIterator const& it) { it.print_to(os); return os; }
*/
  

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
  bool  points_to_valid_data()     const   { return  mRemaining > 0; }

  LagIterator&  operator++();
  Feature       operator*()        const   { return  Feature(mFeature,mLag,mBlockSize); }

  void  print_to(std::ostream& os) const { os << "LagIterator @ " << mLag << "/" << mMaxLag << " with " << mRemaining << " left. "; }
};

inline
std::ostream&
operator<< (std::ostream& os, LagIterator const& it) { it.print_to(os); return os; }


//     ModelIterator    ModelIterator     ModelIterator     ModelIterator     ModelIterator     ModelIterator     ModelIterator     

template< class Model >
class ModelIterator
{
  Model const& mModel;       // maintained by someone else
  int          mLastQ;       // q of last model built predictors for
  int          mSeparation;  // gap between models that are points_to_valid_data
 public:
 ModelIterator(Model const& m, int gap): mModel(m), mLastQ(0), mSeparation(gap) {}
  
  bool            points_to_valid_data()     const ;
  int             number_remaining ()        const { return 1; }
  ModelIterator&  operator++()                     { return *this; }
  Model const*    operator*()                      { mLastQ = mModel.q(); return &mModel; }
  void            print_to(std::ostream& os) const { os << "ModelIterator, last q=" << mLastQ << "; model @ " << mModel.q() << " with separation " << mSeparation; }
};
 
template <class Model>
std::ostream&
operator<< (std::ostream& os, ModelIterator<Model> const& it) { it.print_to(os); return os; }


//     BeamIterator     BeamIterator     BeamIterator     BeamIterator     BeamIterator     BeamIterator     BeamIterator     BeamIterator

template< class Auction >
class BeamIterator
{  
 public:
  typedef std::pair<std::vector<int>, std::vector<int>> IndexPair;               // which model features form the beams
  typedef std::pair<int, Feature>                       IndexedFeature;          // where does feature appear in model
  typedef std::vector<std::vector<IndexedFeature>>      IndexedFeatureVector;    // features in a beam
  typedef std::pair<std::string,FeaturePredicate>       NamedFeaturePredicate;   // assign feature with index to beam

 private:
  typedef std::vector<std::string     >   NameVector;
  typedef std::vector<FeaturePredicate>   PredicateVector;
  typedef std::vector<int>                IntVector;
  typedef std::pair<int,int>              IntPair;
  typedef std::map<IntPair,IntPair>       IntPairMap;
  
  Auction    const&         mAuction;    
  NameVector                mBeamNames;
  int                       mGap;               // gap between beams from same stream
  int                       mLastQ;
  IntPair                   mBestBeam;
  IndexedFeatureVector      mBeamFeatures;
  IntPairMap                mBeamFeaturesUsed;

 public:
 BeamIterator(Auction const& auc, std::vector<std::string> beamNames, int gap)
   : mAuction(auc), mBeamNames(beamNames), mGap(gap), mLastQ(mAuction.number_of_model_features()), mBestBeam({0,0}), mBeamFeatures(beamNames.size())
  { init();  find_best_beam(); }													       

  int             number_of_beams()          const { return (int) mBeamFeatures.size(); }
  bool            points_to_valid_data()           { if(update_adds_to_beams()) find_best_beam(); return best_beam_is_okay(); }
  int             number_remaining ()      const   { return (int) mBeamNames.size();} //  Need to decide something sensible here??? (must maintain const)
  BeamIterator&   operator++()                     { find_best_beam(); return *this; }
  IndexPair       operator*()
  { std::pair<int,int> base = mBeamFeaturesUsed[mBestBeam];
    return std::make_pair(
			  current_beam_indices(mBestBeam.first, base.first),
			  current_beam_indices(mBestBeam.second, base.second)
			  );
  }
  void            print_to(std::ostream& os) const;
  
 private:
  void       init();
  IntVector  current_beam_indices (int beam, int position) const;
  bool       update_adds_to_beams();
  void       find_best_beam();
  bool       best_beam_is_okay() const;
};

template <class A>
inline
std::ostream&
operator<<(std::ostream& os, BeamIterator<A> bi)
{
  bi.print_to(os);
  return(os);
}

//     BundleIterator     BundleIterator     BundleIterator     BundleIterator     BundleIterator     BundleIterator     BundleIterator

template< class Collection, class SkipPred >   // must be random access collection
class BundleIterator
{
  Collection       mSource;
  unsigned int     mBundleSize;
  SkipPred         mSkipPred;
  unsigned int     mLoIndex, mHiIndex;
public:
  BundleIterator(Collection const& source, int bundleSize, SkipPred pred) : mSource(source), mBundleSize(bundleSize), mSkipPred(pred), mLoIndex(0), mHiIndex(0) { }

  bool points_to_valid_data()   const    { return mBundleSize <= (mSource.size()-mLoIndex); } 
  int  number_remaining ()      const    { if (points_to_valid_data()) return 1; else return 0; }
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
  inline
  std::ostream&
  operator<< (std::ostream& os, BundleIterator<Collection,Pred> const& it) { it.print_to(os); return os; }



//    InteractionIterator      InteractionIterator      InteractionIterator      InteractionIterator      InteractionIterator      InteractionIterator      

template<class Collection, class SkipPred>                         // static collection
class InteractionIterator
{
private:
  typedef typename Collection::const_iterator Iter;

  Collection  mSource;
  bool        mIncludeDiagonal;
  SkipPred    mSkipPred;
  Iter        mpDiagFeature, mpColFeature;
  int         mRemain;
  
public:
  
  InteractionIterator(Collection const& src, bool useSquares, SkipPred pred)
    : mSource(src), mIncludeDiagonal(useSquares), mSkipPred(pred),
    mpDiagFeature(mSource.begin()), mpColFeature(mSource.begin()), mRemain(initial_count((int)src.size())) { initialize(); }
  
  int   number_remaining()           const { return mRemain; }
  bool  points_to_valid_data ()      const { return mRemain > 0; }

  InteractionIterator& operator++();  
  Feature              operator*()   const;  

  void  print_to(std::ostream &os)   const;

 private:
  bool  skip_current_pair()          const;
  int   initial_count(int k)         const;
  void  initialize();
  void  inc_pointers();
};


template <class Collection, class Pred>
  inline
  std::ostream&
  operator<< (std::ostream& os, InteractionIterator<Collection,Pred> const& it) { it.print_to(os); return os; }



//     CrossProductIterator     CrossProductIterator     CrossProductIterator     CrossProductIterator     CrossProductIterator     CrossProductIterator     

/*  Allows combination of two dynamically growing vectors. Since
    accessed through a persistent const reference,

         * you must guarantee *

    that the sources remain "alive" for the duration of the
    application.
                                                                                                                
    Suppose the fast source has 4 elements.  Then                                                               
                                                                                                                
    Position vector {4,2,0} indicates that                                                                      
            var 0 of the slow source has been crossed with 0,1,2,3 of fast (done with var 0 for now)            
            var 1                    has been crossed with 0,1     of fast, next with third in fast             
            var 2                    has not been crossed with any                                              
*/                                                                                                              
                                                                                                                
template<class SkipPred> 
class DynamicCrossProductIterator
{
  FeatureVector const&    mSlowSource;
  FeatureVector const&    mFastSource;
  SkipPred                mSkipPred;
  unsigned                mSlowIndex;
  mutable std::vector<unsigned> mFastIndices;

 public:
 DynamicCrossProductIterator(FeatureVector const& slow, FeatureVector const& fast, SkipPred p)
   : mSlowSource(slow), mFastSource(fast), mSkipPred(p), mSlowIndex(0), mFastIndices() { update_index_vector(); }
  
  int                           number_remaining ()    const;
  bool                          points_to_valid_data() const;
 
  Feature                       operator*()            const;
  DynamicCrossProductIterator& operator++();
  
  void print_to (std::ostream& os)                     const;
    
 private:
  void update_index_vector()                const;
  void print_indices(std::ostream& os)      const;
};

template<class Pred>
  inline
  std::ostream&
  operator<< (std::ostream& os, DynamicCrossProductIterator<Pred> const& it) { it.print_to(os); return os; }


// This version handles static sources, rather than dynamic as above. Repeats in cycles

template<class SkipPred> 
class CrossProductIterator
{
  FeatureVector               mSlowSource;
  FeatureVector               mFastSource;
  SkipPred                    mSkipPred;
  mutable   size_t            mSlowIndex;
  mutable std::vector<size_t> mFastIndices;

 public:
 CrossProductIterator(FeatureVector slow, FeatureVector fast, SkipPred p)
   : mSlowSource(slow), mFastSource(fast), mSkipPred(p), mSlowIndex(0), mFastIndices() { update_index_vector(); }
  
  int                   number_remaining ()    const;
  bool                  points_to_valid_data() const;

  Feature               operator*()            const;
  CrossProductIterator& operator++();
  
  void print_to (std::ostream& os)             const;
    
 private:
  void update_index_vector()                   const;
  void print_indices(std::ostream& os)         const;
};
                                                                                                                                                           

template<class Pred>
  inline
  std::ostream&
  operator<< (std::ostream& os, CrossProductIterator<Pred> const& it) { it.print_to(os); return os; }

#endif
