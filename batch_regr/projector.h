// $Id: projector.h,v 1.2 2003/06/05 03:14:09 bob Exp $

/*
  30 May 03 ... Created to handle residuals.
*/

#ifndef _PROJECTOR_H_
#define _PROJECTOR_H_

#include <iostream>
#include <vector>

#include "sweep_matrix.h"
#include "datasets.h"
#include "range.h"

template <class Data>
class Projector
{
  Data const& mData;
  SweepMatrix const& mSM;

 public:
  
  Projector (Data const& data, SweepMatrix const& sm)
    : mData(data), mSM(sm) { }
  
  std::vector<double>
    ols_residual_vector() const
    {  std::vector<double> e     (mData.n_rows());
       std::vector<int>    preds (mSM.predictors());
       std::vector<double> beta  (mSM.slopes());
       for (int i=0; i<mData.n_rows(); ++i)
       { typename Data::CenteredDataRange devs (mData.centered_row(i));
	 e[i] = *begin(devs); // y in col 0
	 for (int j=0; j<mSM.number_of_predictors(); ++j)
	 {  // std::cout << "   " << e[i] << " -= " << beta[j] << " * " << *(begin(devs)+preds[j]) << ";";
	    e[i] -= beta[j] * *(begin(devs)+preds[j]);
	 }
       }
       return e;
    }

  std::vector<double>
    ols_fit_vector() const
    {  std::vector<double> resids (ols_residual_vector());
       for (int i=0; i<mData.n_rows(); ++i)
       { typename Data::DataRange row (mData.row(i));
	 resids[i] = (*begin(row)) - resids[i]; // y in column 0 at *begin
       }
       return resids;
    }

  std::vector<double>
    truncated_ols_fit_vector(double lower_limit, double upper_limit) const
    {  std::vector<double> resids (ols_residual_vector());
       for (int i=0; i<mData.n_rows(); ++i)
       { typename Data::DataRange row (mData.row(i));
	 resids[i] = (*begin(row)) - resids[i]; 
	 if (resids[i] < lower_limit) resids[i] = lower_limit;
	 else if (resids[i] > upper_limit) resids[i] = upper_limit;
       }
       return resids;
    }

  
  template <class Range>
    std::vector<double>
    weighted_partial_residual_ss_vector(Range wts) const
    {  std::vector<double> wss  (mSM.number_of_variables(), 0.0);
       std::vector<int> preds (mSM.predictors());
       for (int i=0; i<mData.n_rows(); ++i)
       { 
	 typename Data::CenteredDataRange devs (mData.centered_row(i));
	 for (unsigned int k=1; k<wss.size(); ++k)
	   if (false == mSM.skip(k))
	   { double z (*(begin(devs)+k));
	     for (int j=0; j<mSM.number_of_predictors(); ++j)
	       z -= *(begin(devs)+preds[j]) * mSM.partial_beta(j+1, k);
	     wss[k] += z * z * *(begin(wts)+i);
	   }
       }
       return wss;
     }

  template <class Range>
    std::pair< std::vector<double>, std::vector<double> >
    bennett_residual_pair(Range wts) const
    {  std::vector<double> a  (mSM.number_of_variables(), 0.0);
       std::vector<double> b  (mSM.number_of_variables(), 0.0);
       std::vector<int> preds (mSM.predictors());
       for (int i=0; i<mData.n_rows(); ++i)
       { 
	 typename Data::CenteredDataRange devs (mData.centered_row(i));
	 for (unsigned int k=1; k<a.size(); ++k)
	   if (!mSM.skip(k))
	   { double z (*(begin(devs)+k));
	     for (int j=0; j<mSM.number_of_predictors(); ++j)
	       z -= *(begin(devs)+preds[j]) * mSM.partial_beta(j+1, k);
	     b[k] += z * z * *(begin(wts)+i);
	     if (z < 0.0) z = - z;
	     if (z > a[k]) a[k] = z;   // Weight by fit as well (see binaryDataFile.c) ???
	   }
       }
       std::vector<double> rss (mSM.residual_ss_vector());
       for (int k=1; k<mSM.number_of_variables(); ++k)
       { if (!mSM.skip(k))
	 { a[k] =      a[k] /rss[k];
	   b[k] = sqrt(b[k])/rss[k];
	 }
       }
       return std::make_pair(a,b);
     }
};

#endif
