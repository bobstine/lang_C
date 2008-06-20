/*  $Id: observation.h,v 1.4 2002/03/23 22:49:20 foster Exp $

   3 Jan 02 ... Distinguish estimation from sampling weights.
  13 Dec 01 ... Add iterator for underlying data.  
  29 Nov 01 ... Created to hold abstract observation.

*/
#ifndef _OBSERVATION_
#define _OBSERVATION_

#include <iostream>
#include <vector>

using namespace std::vector;

class Observation
{
  double mSampleWeight, mEstimationWeight;
  vector<double> mData;

 public:
  Observation()
    : mSampleWeight(1.0), mEstimationWeight(1.0), mData() { }
  Observation(long p)
    : mSampleWeight(1.0), mEstimationWeight(1.0) { mData.reserve(p); }
  Observation(double weight, const std::vector<double> data)
    : mSampleWeight(weight), mEstimationWeight(1.0), mData(data) { }
  Observation(istream &input)
    : mSampleWeight(1.0), mEstimationWeight(1.0), mData() { read_from(input); }
  
  double weight() const            { return mSampleWeight * mEstimationWeight; }
  double sample_weight () const    { return mSampleWeight; }
  double estimation_weight () const{ return mEstimationWeight; }
  void set_estimation_weight(double wt) { mEstimationWeight = wt; }
  
  long size() const                { return mData.size(); }
  double element (long j) const    { return mData[j]; }

  vector<double> data() const      { return mData; }
  vector<double>::const_iterator begin() const { return mData.begin(); }
  vector<double>::const_iterator end() const   { return mData.end(); }
  
  void Observation::read_from (istream& input);
  void Observation::write_to (ostream& output) const;
} ;

istream&
operator>> (istream& input, Observation& obs);

ostream&
operator<< (ostream& output, const Observation& obs);
  
#endif
