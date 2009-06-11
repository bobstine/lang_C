/* -*- c++ -*-
 *  auction.Template.h
 *
 *  Created from prior cc file by Robert Stine on 1/3/08.
 *  Copyright 2008. All rights reserved.
 *
 */


template <class Model>
void
Auction<Model>::write_csv_header_to(std::ostream& os, double ssIn, double ssOut) const
{
  os << "Round, Time, Goodness of Fit, Total Alpha  ";
  for (int b=0; b<number_of_experts(); ++b)
    os << ", " << mExperts[b]->name() << " Expert, Alpha, Current Bid";
  os << ", Winning Expert, High Bid, p-value, Variable, Outcome, Payoff, RSS, CVSS\n";
  os << " , , ,";
  for (int b=0; b<number_of_experts(); ++b)    os << ",  , , ";
  os << ", , , , , , , " << ssIn << "," << ssOut << std::endl;
}


// Output to OS must be *csv* delimited columns; each item adds its own prefix , delimiter
template <class ModelClass>
bool
Auction<ModelClass>::auction_next_feature (std::ostream& os)
{
  ++mRound;
  debugging::debug(0) << "AUCT: Beginning auction round #" << mRound << std::endl;
  // identify expert with highest total bid; collect_bids writes name, alpha, bid to os
  std::pair<ExpertABC*,double> winner (collect_bids(os));  
  ExpertABC* pWinningExpert = winner.first;
  double      highBid    = winner.second;
  if (0.0 == highBid) 
  { mHasActiveExpert = false;
    debugging::debug(0) << "AUCT: Auction does not have an active expert to bid; terminating.\n";
    if (os) os << std::endl;
    return false;
  } 
  // extract chosen features
  std::vector<Feature> features  (pWinningExpert->feature_vector()) ;
  if (features.empty())
  { debugging::debug(3) << "AUCT: *** ERROR **** Winning expert " << pWinningExpert->name() << " did not provide features.\n";
    if (os) os << std::endl;
    return false;
  }
  else
  { debugging::debug(0) << "AUCT: Winning expert  " << pWinningExpert->name() << " bids on ";
    print_features(features);
  }
  // build variables for testing
  std::vector< std::pair<std::string, FeatureABC::Iterator> > namedIterators;
  for (unsigned int j=0; j<features.size(); ++j)
    namedIterators.push_back( make_pair(features[j]->name(), features[j]->begin()));
  TestResult result (mModel.add_predictors_if_useful (namedIterators, highBid));
  debug(3) << "AUCT: Test results are  <" << result.first << "," << result.second << ">\n";
  if (os)
    os << ", " << result.second << ", " << features[0]->name();
  // report bid result
  double payoff = payoff_to_highest_bidder(pWinningExpert, highBid, result.second);            // result.second is p-value
  bool addedPredictors (payoff > 0.0);
  if (os)
    if (addedPredictors)
    { std::pair<double,double> rss (mModel.sums_of_squares());
      os << "," << features[0]->name() << "," << payoff << "," << rss.first << "," << rss.second;
    }
    else
      os <<               ", ,"               << payoff <<           ", , ";
  for (unsigned int j=0; j<features.size(); ++j)
  { features[j]->set_model_results(addedPredictors, result.second);                      // save attributes in feature for printing
    if (addedPredictors) 
    { mLogStream << "+F+   " << features[j] << std::endl;              // show selected feature in output with key for grepping
      mModelFeatures.push_back(features[j]);
    }
    else      
      mSkippedFeatures.push_back(features[j]);
  }
  return addedPredictors;
}


template <class ModelClass>
std::pair<ExpertABC*,double>
Auction<ModelClass>::collect_bids (std::ostream& os)
{
  if (os)
  { const time_t tmpTime (time(0));
    std::string timeStr (asctime(localtime(&tmpTime)));
    os << mRound << ", " << timeStr.substr(0,timeStr.size()-5)
       << ", " << model_goodness_of_fit()
       << ", " << total_expert_alpha();
  }
  ExpertABC* pWinningExpert        (NULL);
  double     highBid               (-7.7);     
  ExpertABC* pPriorityBidder       (NULL);
  double     priorityBid           (0.0);
  for(ExpertIterator it = mExperts.begin(); it != mExperts.end(); ++it)
  { double bid = (*it)->place_bid(mPayoffHistory, mModelFeatures, mSkippedFeatures);  // pass information to experts
    if (os)
      if (bid > 0.0)	os << ", "    << (*it)->feature_name() << ", " << (*it)->alpha() << ", " << bid;
      else       	os << ",  , "                                  << (*it)->alpha() << ", " << bid;
    if (bid > highBid)
    { highBid = bid;
      pWinningExpert = *it;
    }
    if ((*it)->priority() > 0) // priority bidder with bid jumps to top
    { 
      if (bid > 0)
      { debug(3) << "AUCT: Priority bidder takes bid " << bid << std::endl;
	pPriorityBidder = *it;
	priorityBid = bid;
      }
    }
  }
  if (pPriorityBidder) // override results
  { 
    highBid = priorityBid;
    pWinningExpert = pPriorityBidder;
  }
  if(os)
    os << ", " << pWinningExpert->name() << ", " << highBid;
  // inform experts whether win/lose auction (not model status, just whether bid was accepted)
  pWinningExpert->bid_accepted();
  for(ExpertIterator it = mExperts.begin(); it != mExperts.end(); ++it)
    if ((*it) != pWinningExpert) (*it)->bid_declined();
  return std::make_pair(pWinningExpert, highBid);
}


template<class ModelClass>
double
Auction<ModelClass>::payoff_to_highest_bidder (ExpertABC* pWinningExpert, double bid, double pValue)
{
  if (pValue > 1.0)
  { pWinningExpert->payoff(0.0);                       // singularity in predictors
    mPayoffHistory.push_back(0.0);
    return 0.0;
  }
  if (pValue < bid)                                    // add variable to model
  { pWinningExpert->payoff(mPayoff);
    mPayoffHistory.push_back(mPayoff);
    return mPayoff;
  }
  else
  { double cost = -bid/(1.0-bid);
    pWinningExpert->payoff(cost);
    mPayoffHistory.push_back(cost);
    return cost;
  }
}
  


template <class ModelClass>
double
Auction<ModelClass>::total_expert_alpha () const
{
  double total (0.0);
  for (int i=0; i<number_of_experts(); ++i)
    total += mExperts[i]->alpha();
  return total;
}


// Output

template <class ModelClass>
void
Auction<ModelClass>::print_to (std::ostream& os) const
{
  os << std::endl << "     Auction    " << mExperts.size() << " bidders with total alpha " << total_expert_alpha() << std::endl;
  os << mExperts << std::endl << mModel << std::endl;
}


template <class ModelClass>
void
Auction<ModelClass>::print_model_features_to (std::ostream& os) const
{
  for (int j=0; j<mModelFeatures.size(); ++j)
    os << "   Feature #" << j << ":  " << mModelFeatures[j] << std::endl;
}

template <class ModelClass>
FeatureABC*
Auction<ModelClass>::xb_feature(std::vector<double> const& beta) const
{
  int q (beta.size()-1);                     // dont count intercept
  FeatureVector useFeatures(q);              // only use leading q features (as needed with calibrator)
  for (int j=0; j<q; ++j)
    useFeatures[j] = mModelFeatures[j];
  std::ostringstream oss;
  oss << q;
  std::string name ("xb_" + oss.str());
  int     n      (mModel.len());                 // include those used in validation
  LinearCombinationFeature *f = new LinearCombinationFeature(n, beta,useFeatures);
  return f;
}


template <class ModelClass>
void
Auction<ModelClass>::print_features(FeatureVector const& features)   const
{
  size_t nFeatures (features.size());
  
  if (1==nFeatures) 
    mLogStream << "one feature: " << features[0]->name();
  else 
  { mLogStream << nFeatures << " features: "; 
    for (size_t j=0; j<nFeatures; ++j)  
      mLogStream << features[j]->name() << ", ";
  }
  mLogStream << std::endl;
}

template <class ModelClass>
void
Auction<ModelClass>::write_model_data_to       (std::ostream& os)       const
{
  mModel.write_data_to(os);
}


template <class ModelClass>
void
Auction<ModelClass>::print_model_to  (std::ostream& os, bool useHTML) const
{
  mModel.print_to(os, useHTML);
}


template <class ModelClass>
void
Auction<ModelClass>::write_model_to  (std::ostream& os) const
{
  os << "Auction Model: ";
  mModel.print_to(os);
  // feature list has <= q items; does not include combinations based on prior predictor
  for (int j=0; j<int(mModelFeatures.size()); ++j)      
  { mLogStream << "AUCT: Writing model feature " << mModelFeatures[j]->name()
	       << " (" << j << " / " << mModelFeatures.size() << ")" 
	       << " with range " << mModelFeatures[j]->range() << std::endl;
    mLogStream << "       center = " << mModelFeatures[j]->center()
	       << "    scale = " << mModelFeatures[j]->scale() << std::endl;
    mModelFeatures[j]->write_to(os);
  }
}

//  This version write the model to a file by dropping any calibration terms, then
//  writing the calibrator as a separate object.
/*
 void
 Auction::write_model_to  (std::ostream& os) const
 {
   os << "Auction Model: ";
   mModel.write_without_calibration_to(os);
   int q (number_of_predictors());
   if (calibrated())
   { --q;                   // dont count the calibrator
                            // calibration_feature()->write_to(os);
                            // only need to write the operator to the file
     mModel.write_calibrator_to(os);
   }
   for (int j=0; j<q; ++j)
     mModelFeatures[j]->write_to(os);
 }
 */




template <class ModelClass>
std::ostream&
operator<<(std::ostream& os, Auction<ModelClass> const& auction) 
{ 
  auction.print_to(os); 
  return os; 
}


