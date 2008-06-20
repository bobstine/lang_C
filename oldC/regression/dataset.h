// $Id: dataset.h,v 1.8 2003/11/26 16:49:18 bob Exp $

/*
  29 Jan 03 ... Append a column
  13 Dec 02 ... Update, make weight iterator random access.
  17 Jan 02 ... Weights via iterator.
  29 Nov 01 ... Convert from dataarray to use C++ vectors, observations, doubles.
  23 Aug 01 ... Alter how operators are applied to data arrays.
  7  Jul 01 ... Import operators from distinct file,then revised.
  26 Jun 01 ... Row-major form for mgs version of sweeper.
  15 Oct 98 ... Created in original form for sweeper.
*/

#ifndef _DATASET_H_
#define _DATASET_H_

#include <vector>
#include <iterator>
#include <string>

#include "observation.h"   // gets range things

class Dataset;

class DatasetWeightIterator : public std::iterator<std::random_access_iterator_tag, double>
{
  std::vector<Observation>::const_iterator mIter;
 public:
  DatasetWeightIterator (std::vector<Observation>::const_iterator iter)
    : mIter(iter) { }
  
  double operator*() const { return mIter->weight(); }

  void operator++() { ++mIter; }

  DatasetWeightIterator&
    operator+=(const int &n) { mIter += n; return *this; }
  DatasetWeightIterator&
    operator-=(const int &n) { mIter -= n; return *this;}

  int operator-(DatasetWeightIterator const& it) const {return mIter - it.mIter;}
  
  bool operator<(DatasetWeightIterator const& it) const { return mIter < it.mIter; }
  bool operator>(DatasetWeightIterator const& it) const { return mIter > it.mIter; }
  bool operator!=(const DatasetWeightIterator& it) const { return mIter != it.mIter; }
  bool operator==(const DatasetWeightIterator& it) const { return mIter == it.mIter; }
};


class Dataset
{
  int mNRows;
  double mSumWts;
  std::vector<Observation> mObservations;

 public:
  typedef Observation const& value_type;
  
  typedef DatasetWeightIterator  weight_iterator;
  typedef range<weight_iterator> weight_range;

  typedef std::vector<Observation>::const_iterator const_iterator;
  typedef range<const_iterator> obs_range;
  typedef std::vector<Observation>::iterator iterator;
  typedef assignable_range<iterator> writeable_obs_range;
  

  Dataset ()
    : mNRows(0), mSumWts(0.0) { };

  template <class UnaryOp>
  Dataset (int nRows, int nCols, UnaryOp f)
    : mNRows(nRows), mSumWts(nRows), mObservations() { fill_with(nCols, f); }
  
  Dataset (const char *fileName);
  Dataset (const std::vector<double> y);
  Dataset (std::istream& input) { read_from(input); }

  std::pair<int,int>       dimension()            const { return std::make_pair(nRows(), nCols()); }
  int                      nRows()                const { return mNRows; }
  int                      nCols()                const { return mObservations.begin()->size();  }

  double                   element (int i, int j) const { return mObservations[i].element(j); }
  Observation const&       operator[] (int i)     const { return mObservations[i]; }
  
  obs_range                observation_range ()   const { return make_range(mObservations); }
  const_iterator           begin ()               const { return mObservations.begin(); }
  const_iterator           end ()                 const { return mObservations.end(); }

  double                   sum_weights ()         const { return mSumWts; }
  void                     set_estimation_weights(const std::vector<double>& wts);
  
  DatasetWeightIterator    begin_weights()        const { return DatasetWeightIterator(mObservations.begin()); }
  DatasetWeightIterator    end_weights()          const { return DatasetWeightIterator(mObservations.end()); }
  weight_range             weights()              const
    { return make_range(
			DatasetWeightIterator(mObservations.begin()),
			DatasetWeightIterator(mObservations.end())
			); }
  
  void
    append_values_to_observations (std::vector<double> column);
  
  void
    read_from (std::istream& input);

 private:
  
  void fill_with( int nCols, double (*f)(int));

};

std::istream&
operator>>(std::istream& output, Dataset& data);

std::ostream&
operator<<(std::ostream& output, const Dataset& data);

#endif
