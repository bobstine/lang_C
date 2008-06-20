// $Id: recommenders.h,v 3.11 2008/01/21 23:27:26 bob Exp $

#ifndef _RECOMMENDERS_H_
#define _RECOMMENDERS_H_

/*
  Recommenders use a 'strategy' to recommend a feature for use in the
  auction.  They also keep track of their 'history' in the auction so
  that we can find out how well they have done in the past.

  Recommenders can draw data from a source of columns (such as read
  from a file) or from something that responds with a vector of
  features.  The base class makes no distinction of where a
  recommender gets its choices.
 
  Recommenders are also able to rebuild features from a factory.
  
    6 Apr 04 ... Separate making a feature from advancing position to handle empty initial source.
   16 Mar 04 ... Created.
*/

#include "range.h"
#include "range_ops.h"
#include "range_stats.h"

#include "function_iterators.h"
#include "anonymous_iterator.h"

#include "column.h"

#include <iostream>
#include <vector>
#include <utility>
#include <algorithm> // random_shuffle
#include <sstream>


class RecommenderABC
{
 public:
  typedef std::pair<bool, double> ModelResult;     // <was result used, p-value>
  typedef FeatureABC*             FeaturePtr;
  typedef FeatureABC const*       ConstFeaturePtr;

 protected:
  FeaturePtr               mFeature;

 private:
  std::string              mName;
  std::vector<ModelResult> mHistory;
  int                      mNumSinceUsed;
  
 public:
  virtual ~RecommenderABC() { }

  RecommenderABC()
    : mFeature(0), mName(""), mHistory(), mNumSinceUsed(0) { }
  RecommenderABC(std::string name)
    : mFeature(0), mName(name), mHistory(), mNumSinceUsed(0) { }
  
  std::string              name()                          const { return mName; }
  bool                     has_a_feature(); 
  FeaturePtr               feature()                       const { return mFeature; }
  int                      number_recommended()            const { return mHistory.size(); }
  int                      number_recommended_since_used() const { return mNumSinceUsed; }
  std::vector<ModelResult> history()                       const { return mHistory; }
  std::pair<int, int>      history_summary()               const { return std::make_pair(number_successes(), mHistory.size()); }

  void                     update_history (const FeatureABC*, bool b);               // input feature used for debugging to confirm that it was from this recommender
  virtual void             chosen_feature (ConstFeaturePtr f);                       // somebody else's feature appeared in auction; way for the rcmd to find out what auctioned
  virtual int              number_remaining()              const       = 0;          // pure virtual
  virtual void             print_to(std::ostream& os)      const;

 protected:
  virtual bool             make_feature()                              = 0;          // fails if cannot make feature
  virtual void             increment_position() { }
 private:
  int                      number_successes()              const;
};

inline
std::ostream&
operator<<(std::ostream& os, RecommenderABC const* r)
{
  r->print_to(os); return os;
}


//  SequenceRecommender  SequenceRecommender  SequenceRecommender  SequenceRecommender  SequenceRecommender  

template<class Source>
class SequenceRecommender : public RecommenderABC
{
   Source const& mSource;
   int           mPosition;

  public:
   virtual ~SequenceRecommender() {}

   SequenceRecommender(std::string name, Source const& src)
     : RecommenderABC(name), mSource(src), mPosition(0) { make_feature(); }
  
   int   number_remaining()                  const { return mSource.size() - mPosition; }
   void  print_to(std::ostream& os)          const;
  
 protected:
   void  increment_position()                       { ++mPosition; }
   bool  make_feature ();
};

template <class Source>
inline
SequenceRecommender<Source>*
make_sequence_recommender (std::string const& name, Source const& s)
{
  return new SequenceRecommender<Source>(name, s);
}

 
//  InteractionRecommender  InteractionRecommender  InteractionRecommender  InteractionRecommender  InteractionRecommender  

template<class Source>
class InteractionRecommender : public RecommenderABC
{
  Source const&   mSource;
  int             mPos1, mPos2;
  FeatureFactory* mFactory;
  
 public:
  virtual ~InteractionRecommender() {}

  InteractionRecommender(std::string name, FeatureFactory *ff, Source const& src)
    : RecommenderABC(name), mSource(src), mPos1(0), mPos2(), mFactory(ff) { make_feature(); }
  
  int  number_remaining()         const { int d (mSource.size()); int m (mPos2+1); return (d * (d+1))/2 - (m*(1+m))/2 + mPos1; }
  void print_to(std::ostream& os) const { RecommenderABC::print_to(os); os << " " << mPos1 << " x " << mPos2 << " "; }
  
 protected:
  bool make_feature();
  void increment_position();
};

template <class Source>
inline
InteractionRecommender<Source>*
make_interaction_recommender (std::string const& name, FeatureFactory *ff, Source const& s)
{
  return new InteractionRecommender<Source>(name, ff, s);
}



//  PermutationRecommender  PermutationRecommender  PermutationRecommender  PermutationRecommender  PermutationRecommender
 
template<class Source>
class PermutationRecommender : public RecommenderABC
{
   Source const&    mSource;
   int              mPosition;
   std::vector<int> mPermutation;
   
  public:
   virtual ~PermutationRecommender() {}

   PermutationRecommender(std::string name, Source const& src, std::vector<int> permutation)
     : RecommenderABC(name), mSource(src), mPosition(0), mPermutation(permutation) { make_feature(); }

   PermutationRecommender(std::string name, Source const& src)
     : RecommenderABC(name), mSource(src), mPosition(0), mPermutation(mSource.size()) { make_permutation(); make_feature(); }

   int   number_remaining()                 const  { return mSource.size() - mPosition; }
   void  print_to(std::ostream& os)         const; 
  
 protected:
  bool  make_feature ();
  void  increment_position()                        { ++ mPosition; }
  void  make_permutation ();
};

template <class Source>
inline
PermutationRecommender<Source>*
make_permutation_recommender (std::string const& name, Source const& s)
{
  return new PermutationRecommender<Source>(name, s);
}


template <class Source>
inline
PermutationRecommender<Source>*
make_permutation_recommender (std::string const& name, Source const& s, std::vector<int> permutation)
{
  return new PermutationRecommender<Source>(name, s, permutation);
}

 
//  CrossProductRecommender  CrossProductRecommender  CrossProductRecommender  CrossProductRecommender  

template<class Source1, class Source2>
class CrossProductRecommender : public RecommenderABC
{
  Source1 const&  mFixedSource;  // fixed number of features
  Source2 const&  mDynSource;    // this one iterates most often (odometer style)
  int             mFixedPos, mDynPos;
  FeatureFactory* mFactory;
  
 public:
  virtual ~CrossProductRecommender() {}

  CrossProductRecommender(std::string name, FeatureFactory *ff, Source1 const& fixedSrc, Source2 const& dynSrc)
    : RecommenderABC(name), mFixedSource(fixedSrc), mDynSource(dynSrc), mFixedPos(0), mDynPos(0), mFactory(ff) { make_feature(); }
  
  int  number_remaining()         const { return (mFixedSource.size()-mFixedPos)*(mDynSource.size()-mDynPos); }
  void print_to(std::ostream& os) const { RecommenderABC::print_to(os); os << " " << mFixedPos << " xx " << mDynPos << " "; }
  
 protected:
  bool make_feature();
  void increment_position();
};

template <class Source1, class Source2>
inline
CrossProductRecommender<Source1, Source2>*
make_cross_product_recommender (std::string const& name, FeatureFactory *ff, Source1 const& fixedSrc, Source2 const& dynSrc)
{
  return new CrossProductRecommender<Source1, Source2>(name, ff, fixedSrc, dynSrc);
}


//  ModelFitRecommender  ModelFitRecommender  ModelFitRecommender  ModelFitRecommender  ModelFitRecommender

template<class Source, class Model>
class ModelFitRecommender : public RecommenderABC
{
  int             mModelSize;
  Source          mSource;
  Model const&    mModel;
  FeatureFactory* mFF;

 public:
   ~ModelFitRecommender() {}

   ModelFitRecommender(std::string name, FeatureFactory *ff, Source const& src, Model const& model)
     : mModelSize(0), RecommenderABC(name), mSource(src), mModel(model), mFF(ff) { }

   void  chosen_feature(ConstFeaturePtr f)         { RecommenderABC::chosen_feature(f); make_feature(); }
   int   number_remaining()                 const  { if (mModel.number_of_predictors() > 1) return 1; else return 0; }
   void  print_to(std::ostream& os)         const  { RecommenderABC::print_to(os); os << " Fit at q=" << mModelSize; }
  
 protected:
   bool make_feature ();
};


//  GarbageRecommender  GarbageRecommender  GarbageRecommender  GarbageRecommender  GarbageRecommender

template<class Model>
class GarbageRecommender : public RecommenderABC                 // Tries polynomial of those that have not worked
{
  Model const&    mModel;
  int             mModelSize;
  int             mLast;
  FeatureFactory* mFF;
  
public:
    ~GarbageRecommender() {}
  
  GarbageRecommender(std::string name, FeatureFactory *ff, Model const& model)
    : RecommenderABC(name), mModel(model), mModelSize(0), mLast(0), mFF(ff) { }
  
  int   number_remaining() const;

  void  print_to(std::ostream& os) const ;
  
protected:
    bool make_feature ();
};

template<class Auction>
inline
GarbageRecommender<Auction>*
make_garbage_recommender (std::string const& name,
                          FeatureFactory *ff,
                          Auction const& model)
{
  return new GarbageRecommender<Auction>(name, ff, model);
}



//  FitTrashRecommender  FitTrashRecommender  FitTrashRecommender  FitTrashRecommender  FitTrashRecommender
// This recommender uses the current y-hat and all previous dud variables to generate a new
// combination that refits the current fit.  That this works is yet to be need.  Bob and Dean both
// have bets on both sides!

template<class Model>
class FitTrashRecommender : public RecommenderABC
{
  Model const&    mModel;
  int             mModelSize;
  int             mLast;
  FeatureFactory* mFF;

 public:
   ~FitTrashRecommender() {}

   FitTrashRecommender(std::string name, FeatureFactory *ff,
		       Model const& model)
     : RecommenderABC(name), 
     mModel(model),
     mModelSize(0),
     mLast(10),
     mFF(ff) { }

   void  chosen_feature(ConstFeaturePtr f)         { RecommenderABC::chosen_feature(f); make_feature(); }
   int   number_remaining()                 const
     { // If we have a y-hat AND we have sufficient dud x's, lets offer up a new variable
       int result = 0;
       int num_used = mModel.number_of_predictors();
       int num_tried = mModel.number_of_features();
       if ((num_used > 1) && (num_tried > 2 * num_used + 1.3 * mLast))
	 result = 1;
       //       std::cout << "TRASH: (" << num_used << " > 1) && "
       //		 << num_tried-num_used << " > " << mLast << ") == " << result << std::endl;
       return result;
     }
   void  print_to(std::ostream& os)         const
     {
       RecommenderABC::print_to(os); os << "FitTrash: " <<
				       mModel.features().size() << " " << mModel.number_of_predictors() << std::endl;
     }
  
 protected:
   bool make_feature ();
};

template<class Auction>
inline
FitTrashRecommender<Auction>*
make_fit_trash_recommender (std::string const& name,
			    FeatureFactory *ff,
			    Auction const& model)
{
  return new FitTrashRecommender<Auction>(name, ff, model);
}


//  XBRecommender  XBRecommender  XBRecommender  XBRecommender  XBRecommender  XBRecommender  XBRecommender  

template<class Source, class Model>
class XBRecommender : public RecommenderABC
{
  Source const&   mSource;
  Model const&    mModel;
  int             mModelSize;
  FeatureFactory* mFF;
  
 public:
   ~XBRecommender() {}

   XBRecommender(std::string name, FeatureFactory *ff, Source const& src, Model const& model)
     : RecommenderABC(name), mSource(src), mModel(model), mModelSize(0),  mFF(ff) { }

   void  chosen_feature(ConstFeaturePtr f)         { RecommenderABC::chosen_feature(f); make_feature(); }
   int   number_remaining()                 const  { if (mModel.number_of_predictors() > 1) return 1; else return 0; }
   void  print_to(std::ostream& os)         const  { RecommenderABC::print_to(os); os << " xb at q=" << mModelSize; }
  
 protected:
   bool make_feature ();
};

template <class Source, class Model>
inline
XBRecommender<Source, Model>*
make_xb_recommender (std::string const& name, FeatureFactory *ff, Source const& src, Model const& model)
{
  return new XBRecommender<Source, Model>(name, ff, src, model);
}

#include "recommenders.Template.h"

#endif
