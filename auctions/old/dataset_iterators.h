// $Id: dataset_iterators.h,v 3.0 2004/11/19 18:58:36 foster Exp $
#ifndef _DATASET_ITERATORS_H_
#define _DATASET_ITERATORS_H_

/*

  Dataset iterators traverse the columns of a dataset, return centered
  columns or interactions.  When reach the end of the allowed range, they
  halt, returning a zero-length vector and negative indices.

  13 Aug 03 ... Update with negative stopping rule.
   1 Aug 03 ... Created.

*/

// #include "datasets.h"
// #include "model.h"
// #include "tags.h"

#include <iterator>
#include <vector>
#include <utility>


//  Circular column iterator

class DatasetColumnIterator : public std::iterator<std::forward_iterator_tag, std::vector<double> >
{
  NumericDataset const& mDataset;
  const int mFirst, mLast;
  int mIndex;
 public:
  typedef int IndexType;
  
  DatasetColumnIterator (NumericDataset const& dataset, int first, int last)
    : mDataset(dataset), mFirst(first),mLast(last),mIndex(first) { }

  void model_has_changed() const { }

  bool has_a_value() const { return true; }
  
  IndexType current_index () const { return mIndex; }
  
  std::vector<double> operator*() const
    {
      return mDataset.extract_centered_column(mIndex);
    }
  
  void operator++()
    {
      if (mIndex<mLast) ++mIndex;
      else mIndex = mFirst;
    }
};

// Recursive spliting column iterator

class DatasetRecursiveColumnIterator: public std::iterator<std::forward_iterator_tag,
				      std::vector<double> >
{
  NumericDataset const& mDataset;
  const int mFirst, mLast;
  int mIndex;
  int mSplit;

 public:
  typedef int IndexType;
  
  DatasetRecursiveColumnIterator (NumericDataset const& dataset, int first, int last)
    : mDataset(dataset), mFirst(first),mLast(last),mIndex(first), mSplit(0) { }
  
  void model_has_changed() const { }

  bool has_a_value() const { return (mSplit <= 2); }

  IndexType current_index () const { return mIndex; }
  
  std::vector<double> operator*() const
    {
      if (mSplit == 0)
	return mDataset.extract_column(mIndex);
      else
	return mDataset.extract_split_column(mIndex, mSplit);
    }
  
  void operator++()
    {
      if (mIndex<mLast) ++mIndex;
      else
	{ ++mSplit;
	  mIndex = mFirst;
	}
    }
};


// INTERACTION ITERATOR  INTERACTION ITERATOR  INTERACTION ITERATOR  INTERACTION ITERATOR 

class DatasetInteractionIterator : public std::iterator<std::forward_iterator_tag, std::vector<double> >
{
  NumericDataset const& mDataset;
  const int mMin, mMax;
  int mI, mJ;
 public:
  typedef std::pair<int,int> IndexType;
  DatasetInteractionIterator(NumericDataset const& dataset, int min, int max)
    : mDataset(dataset), mMin(min), mMax(max), mI(min), mJ(min) { }

  void model_has_changed() const { }
  
  bool has_a_value() const { return (mI >=0 ); }

  IndexType current_index() const { return std::make_pair(mI, mJ); }
  
  std::vector<double> operator*() const
    {
      return mDataset.extract_centered_interaction(mI, mJ);
    }

  // This version works from the upper left corner above the diagonal
  void operator++()
    {
      if (mI == mMin) // hop to next diagonal if valid
      {
	++ mJ; mI = mJ;
	if (mJ > mMax)
	  mI = mJ = -1;
      }
      else --mI;
      return;
    }
  
  /* This version does a row at a time
    void operator++()
    {
    if (mI < 0)  return;
    if (mJ < mMax) ++ mJ;
    else if (mI < mMax) // hop to next row
    { ++ mI; mJ = mMin; }
    else
    { mI = -1; mJ = -1; } // empty
    }
  */
};


//  Model-based iterator looks at the model to decide what comes next (if any)
//  It returns an index pair using only simple column predictors that it finds in the
//  regression model.

class DatasetModelIterator : public std::iterator<std::forward_iterator_tag, std::vector<double> >
{
  NumericDataset const& mDataset;
  Model const& mModel;
  int mCurrModelSize;
  int mPosition;                               // position in status list
  std::vector< std::pair<int,int> > mStatus;   // <column (simple predictor), #interactions tried>

 public:
  typedef std::pair<int,int> IndexType;    // only simple interactions
  
  DatasetModelIterator (NumericDataset const& dataset, Model const& model)
    :
    mDataset(dataset), mModel(model), mCurrModelSize(model.number_of_predictors()),
    mPosition(-1), mStatus() { }

  void model_has_changed()
    {
      int q (mModel.number_of_predictors());
      if (q > mCurrModelSize)
      { 
	mCurrModelSize = q;
	Tag<std::string, std::vector<int> > lastX (mModel.last_predictor_tag());
	if (lastX.type()=="column") // we can use this one
	  mStatus.push_back( std::make_pair(lastX.value()[0], 0) );
      }
      else
	std::cout << "ITER: Model size did not appear to change, but update requested."
		  << std::endl;
      update_position();
    }
  
  bool has_a_value() const { return (mPosition >= 0); }
  
  IndexType current_index () const
    {
      if (has_a_value())
	return std::make_pair(mStatus[mPosition].first, mStatus[mStatus[mPosition].second].first);
      else
	return std::make_pair(-1,0);
    }	
  
  std::vector<double> operator*() const
     {
       std::pair<int,int> index (current_index());
       return mDataset.extract_centered_interaction(index.first, index.second);
     }

   void operator++()
     {
       ++mStatus[mPosition].second;
       update_position();
     }
   
 private:

  void update_position()
  {
    int size (mStatus.size());
    mPosition = -1;
    for (int i=0; i<size; ++i)                 // find first available interaction
      if (mStatus[i].second < i+1)             // only up to your own index (i >= j)
      { mPosition = i;
	return;
      }
  }
     
};


#endif
