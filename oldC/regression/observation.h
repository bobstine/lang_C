/*  $Id: observation.h,v 1.6 2003/11/26 16:49:18 bob Exp $ -*- c++ -*-

  29 Jan 03 ... Append values.
   3 Jan 02 ... Distinguish estimation from sampling weights.
  13 Dec 01 ... Add iterator for underlying data.  
  29 Nov 01 ... Created to hold abstract observation.

*/
#ifndef _OBSERVATION_H_
#define _OBSERVATION_H_


#include "range.h"
#include "range_traits.h"

#include <iostream>
#include <vector>



class Observation
{
  double mSampleWeight, mEstimationWeight;
  std::vector<double> mData;

 public:
  Observation()
    : mSampleWeight(1.0), mEstimationWeight(1.0), mData() { }
  Observation(int p)
    : mSampleWeight(1.0), mEstimationWeight(1.0) { mData.reserve(p); }
  Observation(double weight, const std::vector<double> data)
    : mSampleWeight(weight), mEstimationWeight(1.0), mData(data) { }
  Observation(std::istream &input)
    : mSampleWeight(1.0), mEstimationWeight(1.0), mData() { read_from(input); }
  
  double weight()                         const { return mSampleWeight * mEstimationWeight; }
  double sample_weight ()                 const { return mSampleWeight; }
  double estimation_weight ()             const { return mEstimationWeight; }
  void   set_estimation_weight(double wt)       { mEstimationWeight = wt; }
  
  int    size()                           const { return mData.size(); }
  double operator[](int j)                const { return mData[j]; } 
  double element (int j)                  const { return mData[j]; }

  std::vector<double>                         data_vector()  const { return mData; }
  range_traits< std::vector<double> >::range  data_range()   const { return make_range(mData); }

  void append_value (double x) { mData.push_back(x); }
  
  void read_from (std::istream& input);
  void write_to (std::ostream& output) const;
} ;

std::istream&
operator>> (std::istream& input, Observation& obs);

std::ostream&
operator<< (std::ostream& output, const Observation& obs);
  
#endif
