// $Id: raters.cc,v 1.6 2003/06/25 12:26:26 bob Exp $

/*
  30 May 03 ... Created
*/

#include "raters.h"
#include "datasets.h"

// From utils
#include "normal.h"
#include "bennett.h"
#include "coding.h"

#include <utility>
#include <string>
#include <iostream>
#include <vector>
#include <functional>

//  SORTING  SORTING  SORTING  SORTING  SORTING  SORTING  SORTING  SORTING  SORTING  SORTING

namespace {

  inline int
  min_int(int i, int j) { return (i < j) ? i : j; }

  inline double
  abs_val (double x) { return (x > 0) ? x : -x; }
  

  class SortPairsOperator : public std::binary_function<int, double, bool>
  {
  public:
    
    bool operator() (std::pair<int, double> const& left, std::pair<int, double> const& right) const
    { return left.second > right.second; }  // first element maximizes the 'good' quality
  };

  std::vector<int>
  sort_pairs (std::string const& label, int k, std::vector< std::pair<int, double> > *gfi)
  {
    std::vector<int> indices;
    int len (gfi->size());
    std::clog << "RTRS: " << label << " picks: ";
    if (len > 0)
    { std::sort(gfi->begin(), gfi->end(), SortPairsOperator());
      for (int j=0; j<min_int(4,len); ++j)
	std::clog << " (" << gfi->at(j).first << ", " << gfi->at(j).second << ")";
      std::clog << std::endl;
      for(int i=0; i<min_int(k,len); ++i)
	indices.push_back(gfi->at(i).first);
    } else
      std::clog << "      No predictors offer improvement." << std::endl;
    return indices;
  }
}

namespace {
  const double twoLnTwo (1.386294361);
}

std::vector<int>
TwoPartCodingRater::recommend_predictors (int k) const
{
  const double bitsForIndex (1.0+log2(mSM.number_of_variables()-1));  // cont + log_2 j 
  const double ss           (mSM.RSS());
  const int    q            (mSM.number_of_predictors());
  const int    df           (mN - q - 2);  // add for const
  SignedCauchyCoder scc;
  std::vector< std::pair<int,double> > gfi;
  std::vector<double> const& rcp (mSM.residual_cp_vector());
  std::vector<double> const& rss (mSM.residual_ss_vector());
  for (int j=1; j<mSM.number_of_variables(); ++j)
  {
    if (! mSM.skip(j))
    { double dRSS     (rcp[j]*rcp[j]/rss[j]);
      double ratio    (log (ss/(ss-dRSS))/twoLnTwo);
      double z        (sqrt (df * (dRSS/(ss-dRSS))));               // include effect of this one
      double bitsForZ (bitsForIndex + scc(z) + (q+1)*ratio);
      double compress (mN * ratio);
      double gain     (compress - bitsForZ);
      if (gain > 0.0)
	gfi.push_back(std::make_pair(j, gain));
    }
  }
  return sort_pairs("Two-part code", k, &gfi);
}
  


std::vector<int>
StepwiseRater::recommend_predictors (int k, double pValue) const
{
  double z     (normal_quantile(1.0-pValue/2.0));
  double sigma (sqrt(mSM.RSS()/mN));
  double zs    (z * sigma);
  std::vector< std::pair<int,double> > gfi;
  std::vector<double> const& rcp (mSM.residual_cp_vector());
  std::vector<double> const& rss (mSM.residual_ss_vector());
  for (int j=1; j<mSM.number_of_variables(); ++j)
    if (! mSM.skip(j))
    { double beta  (abs_val(rcp[j]/rss[j]));
      double bound (beta - zs/sqrt(rss[j]));
      if (bound > 0.0)
	gfi.push_back(std::make_pair(j, bound*bound*rss[j]));
    }
  return sort_pairs("Stepwise", k, &gfi);
}
  

std::vector<int>
WeightedRater<NumericDataset>::pick (int k, double pValue, std::vector<double> const& wss) const
{
  double z     (normal_quantile(1.0-pValue/2.0));
  std::vector< std::pair<int,double> > gfi;
  std::vector<double> const& rcp (mSM.residual_cp_vector());
  std::vector<double> const& rss (mSM.residual_ss_vector());
  for (int j=1; j<mSM.number_of_variables(); ++j)
    if (! mSM.skip(j)) 
    { double beta  (abs_val(rcp[j]/rss[j]));
      double bound (beta - z * sqrt(wss[j])/rss[j]);
      if (bound > 0.0)
	gfi.push_back(std::make_pair(j, bound*bound*rss[j]));
    }
  return sort_pairs("Weighted", k, &gfi);
}


std::vector<int>
BennettRater<NumericDataset>::pick (int k, double pValue, std::vector<double> const& a, std::vector<double> const& b) const
{
  std::vector< std::pair<int,double> > gfi;
  std::vector<double> const& rcp (mSM.residual_cp_vector());
  std::vector<double> const& rss (mSM.residual_ss_vector());
  for (int j=1; j<mSM.number_of_variables(); ++j)
    if (! mSM.skip(j)) 
    {
      double bound (bennett_bound(rcp[j]/rss[j], a[j], b[j], pValue));
      if (bound > 0.0)
	gfi.push_back(std::make_pair(j, bound*bound*rss[j]));
    }
  return sort_pairs("Bennett", k, &gfi);
}
