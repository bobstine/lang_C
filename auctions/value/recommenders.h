// $Id: recommenders.h,v 3.0 2004/11/19 18:58:36 foster Exp $

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
  
   6 Apr 04 ... Separate making a feature from advancing position to handle empty initial source.
   16 Mar 04 ... Created.
*/

#include "range.h"
#include "function_iterators.h"
#include "anonymous_iterator.h"

#include "column.h"
#include "feature.h"

#include <iostream>
#include <vector>
#include <utility>
#include <algorithm> // random_shuffle


class RecommenderABC
{
 public:
  typedef std::pair<bool, double> ModelResult;     // <was result used, p-value>

 protected:
  Feature mFeature;
  
 private:
  std::string              mName;
  std::vector<ModelResult> mHistory;
  int                      mNumSinceUsed;
  
 public:
  virtual ~RecommenderABC() { }

  RecommenderABC()
    : mFeature(), mName(""), mHistory(), mNumSinceUsed(0) { }
  RecommenderABC(std::string name)
    : mFeature(), mName(name), mHistory(), mNumSinceUsed(0) { }
  RecommenderABC(RecommenderABC const& r)
    : mFeature(r.mFeature), mName(r.mName), mHistory(r.mHistory), mNumSinceUsed(r.mNumSinceUsed)
    { std::cout << "\nCOPY: recommender!\n"; }
  
  std::string              name()                          const { return mName; }
  bool                     has_a_feature()                 const { return mFeature.name() != ""; }
  Feature                  feature()                       const { return mFeature; }

  void                     update_history(bool b, double pval);
  int                      number_recommended()            const { return mHistory.size(); }
  int                      number_recommended_since_used() const { return mNumSinceUsed; }
  std::vector<ModelResult> history()                       const { return mHistory; }
  std::pair<int, int>      history_summary()               const { return std::make_pair(number_successes(), mHistory.size()); }

  virtual int              number_remaining()              const       = 0;    // pure virtual
  virtual void             chosen_feature(Feature const& f);
  virtual void             print_to(std::ostream& os)      const;

 protected:
  virtual void             make_feature()                              = 0;
  virtual void             increment_position()                        = 0;
  
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
   void  make_feature();
   
 private:
   void  increment_position()                       { ++mPosition; }
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
  Source const& mSource;
  int           mPos1, mPos2;
  
 public:
  virtual ~InteractionRecommender() {}

  InteractionRecommender(std::string name, Source const& src)
    : RecommenderABC(name), mSource(src), mPos1(0), mPos2() { make_feature(); }
  
  int  number_remaining()         const { int d (mSource.size()); int m (mPos2+1); return (d * (d+1))/2 - (m*(1+m))/2 + mPos1; }
  void print_to(std::ostream& os) const { RecommenderABC::print_to(os); os << " " << mPos1 << " x " << mPos2 << " "; }
  
 protected:
  void make_feature();
  void increment_position();
};

template <class Source>
inline
InteractionRecommender<Source>*
make_interaction_recommender (std::string const& name, Source const& s)
{
  return new InteractionRecommender<Source>(name, s);
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

   void  chosen_feature (Feature const& f);
   int   number_remaining()                 const  { return mSource.size() - mPosition; }
   void  print_to(std::ostream& os)         const; 
  
 protected:
  void  make_feature ();
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


#include "recommenders.Template.h"

#endif
