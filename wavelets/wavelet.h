// $Id: wavelet.h,v 1.8 2000/04/25 16:29:52 bob Exp $-*- c++ -*-
#ifndef _wavelet_
#define _wavelet_

#include <vector.h>
#include <deque.h>

#include "wavecoef.h"
#include "threshold.h"


class Wavelet
{
public:
  Wavelet(const Wave_coef&);

  void Wavelet::decompose(const vector<double> &data);

  double Wavelet::sum_of_squares() const;
  
  deque< vector<double> > Wavelet::estimated_coefficient_deque() const;
  
  vector<double> Wavelet::estimated_coefficient_vector() const;
  vector<double> Wavelet::estimated_coefficient_vector(ThresholdBaseClass*) const;

  vector<double> Wavelet::reconstruction() const;
  
  vector<double> Wavelet::reconstruction(ThresholdBaseClass*) const;

  template<class iter>
  void Wavelet::reconstruct_step(vector<double>::const_iterator loEst,
				vector<double>::const_iterator loEstEnd,
				vector<double>::const_iterator hiEst,
				iter output) const;
  
private:
  int mN;
  const vector<double> mLoCoefs;
  const vector<double> mHiCoefs;
  deque< vector<double> > mDecomposition;
  
  vector<double> Wavelet::reconstruction(deque<vector<double> >::const_iterator,
					deque<vector<double> >::const_iterator) const;

  // these perform one step of the decomposition/reconstruction
  
  void Wavelet::decompose_step(int count,
			      const vector<double> &inputData,
			      vector<double>::iterator loOutputBegin,
			      vector<double>::iterator hiOutputBegin);
};

#endif

/////////////////////////  EOF  ///////////////////////////////////
