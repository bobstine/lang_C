// $Id: dataset.h,v 1.1 2005/06/13 20:47:51 bob Exp $

/*
  17 Jan 02 ... Weights via iterator.
  29 Nov 01 ... Convert from dataarray to use C++ vectors, observations, doubles.
  23 Aug 01 ... Alter how operators are applied to data arrays.
  7  Jul 01 ... Import operators from distinct file,then revised.
  28 Jun 01 ... Operator in place.
  26 Jun 01 ... Row-major form for mgs version of sweeper.
  15 Oct 98 ... Created in original form for sweeper.
*/

#ifndef _DATASET_H_
#define _DATASET_H_

#include <vector>
#include <string>

#include "observation.h"
#include "random.h"
#include "patch_iterator.h"

class DatasetWeightIterator : public iterator<forward_iterator_tag, double> {
  vector<Observation>::const_iterator mIter;
 public:
  DatasetWeightIterator (vector<Observation>::const_iterator iter)
    : mIter(iter) { }
  double operator*() const { return mIter->weight(); }
  void operator++() { ++mIter; }
  bool operator!=(const DatasetWeightIterator& it) const { return mIter != it.mIter; }
};


class Dataset {
  int mNRows, mNCols;
  double mSumWts;
  vector<Observation> mObservations;

 public:
  typedef vector<Observation>::const_iterator const_iterator;
  typedef vector<Observation>::iterator iterator;

  Dataset ()
    : mNRows(0), mNCols(0), mSumWts(0.0) { };
  Dataset (int nRows, int nCols, RandomGenerator& rand) // fill with random
    : mNRows(nRows), mNCols(nCols),mSumWts(nRows), mObservations() { fill_with_iid(rand); }
  Dataset (const char *fileName);
  Dataset (istream& input) { read_from(input); }

  pair<int,int> dimension() const { return make_pair(mNRows, mNCols); }
  int            nRows()     const { return mNRows; }
  int            nCols()     const { return mNCols; }

  double element (int i, int j) const
    { return mObservations[i].element(j); }
  const Observation&
    operator[] (int i) const { return mObservations[i]; }
  
  pair<const_iterator, const_iterator>
    range () const { return make_pair(mObservations.begin(), mObservations.end()); }
  const_iterator
    begin () const { return mObservations.begin(); }
  const_iterator
    end () const { return mObservations.end(); }

  double sum_weights () const { return mSumWts; }
  void set_estimation_weights (const vector<double>& wts);
  
  DatasetWeightIterator
    begin_weights() const { return DatasetWeightIterator(mObservations.begin()); }
  DatasetWeightIterator
    end_weights() const  { return DatasetWeightIterator(mObservations.end()); }
  pair<DatasetWeightIterator, DatasetWeightIterator>
    range_weights() const { return make_pair(mObservations.begin(), mObservations.end()); }
  
  void
    read_from (istream& input);

 private:
    void fill_with_iid(RandomGenerator& rand);

};

istream&
operator>>(istream& output, Dataset& data);

ostream&
operator<<(ostream& output, const Dataset& data);



#endif
