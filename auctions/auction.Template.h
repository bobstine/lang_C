/* -*- c++ -*-
 *  auction.Template.h
 *
 *  Created from prior cc file by Robert Stine on 1/3/08.
 *  Copyright 2008. All rights reserved.
 *
 */


template <class Model>
void
Auction<Model>::write_csv_header_to(std::ostream& os) const
{
  os << "Round, Time, Goodness of Fit, Total Alpha  ";
  for (int b=0; b<number_of_experts(); ++b)
    os << ", " << mExperts[b]->name() << " Expert, Alpha, Current Bid";
  os << ", Winning Expert, High Bid, p-value, Variable, Outcome, Payoff, RSS, CVSS\n";
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
  ExpertABC* pHighBidder = winner.first;
  double      highBid    = winner.second;
  if (0.0 == highBid) 
  { mHasActiveExpert = false;
    if (os) os << std::endl;
    return false;
  } 
  // extract chosen features
  Features::FeatureVector features  (pHighBidder->feature_vector()) ;
  int                     nFeatures (features.size());
  if (0 == nFeatures)
  { debugging::debug(3) << "AUCT: *** ERROR **** Expert " << pHighBidder->name() << " did not provide features.\n";
    if (os) os << std::endl;
    return false;
  }
  else
  { debugging::debug(0) << "AUCT: Winning expert  " << pHighBidder->name() << " bids on ";
    print_features(features);
  }
  //  test chosen features; accept if p-value < bid
  std::vector< std::pair<std::string, FeatureABC::Iterator> > namedIterators;
  for (int j=0; j<nFeatures; ++j)  // pass in name and begin iterator of data
    namedIterators.push_back( make_pair(features[j]->name(), features[j]->begin()));
  TestResult result (mModel.add_predictors_if_useful (namedIterators, highBid));
  if (os)
    os << ", " << result.second << ", " << features[0]->name();
  bool addedPredictors (false);
  std::string message;
  if (result.second > 1.0) {                                       // singularity in predictors
    pHighBidder->payoff(0.0);
    message = "*** Singular ***";
    if (os) os << ", Singular ";
  }
  else
  { addedPredictors = (result.second < highBid);                  //  result.second is p-value 
    if (addedPredictors)
    { message = "*** Add ***";
      mLastVariableAddedRound = mRound;
      pHighBidder->payoff(mPayoff);
      std::pair<double,double> rss (mModel.sums_of_squares());
      if (os) os << "," << features[0]->name() << "," << mPayoff << "," << rss.first << "," << rss.second;
    }
    else
    { message = "*** Decline ***";                                // write name again for those chosen
      double cost = -highBid/(1.0-highBid);
      pHighBidder->payoff(cost);
      if (os) os << ", ," << cost << ", , ";
    }
  }
  for (int j=0; j<nFeatures; ++j)
  { features[j]->set_model_results(addedPredictors, result.second); //  save attributes in the feature for printing
    if (addedPredictors) 
      mModelFeatures.push_back(features[j]);
    else      
      mSkippedFeatures.push_back(features[j]);
    mLogStream << "AUCT: " << message << " pvalue " << result.second << "  bid " << highBid << std::endl;
    mLogStream << "+F+   " << features[j] << std::endl;              // show this feature in output with key for grepping
  }
  return addedPredictors;
}


template <class ModelClass>
std::pair<ExpertABC*,double>
Auction<ModelClass>:: collect_bids (std::ostream& os)
{
  // get time, summary of experts
  if (os)
  { const time_t tmpTime (time(0));
    std::string timeStr (asctime(localtime(&tmpTime)));
    os << mRound << ", " << timeStr.substr(0,timeStr.size()-5)
       << ", " << model_goodness_of_fit()
       << ", " << total_expert_alpha();
  }
  ExpertABC* pHighBidder (mExperts[0]); 
  double     highBid     (-7.7);     
  for(ExpertIterator it = mExperts.begin(); it != mExperts.end(); ++it)
  { double bid = (*it)->place_bid(mLastVariableAddedRound, mRound, mModelFeatures, mSkippedFeatures);
    if (os)
      if (bid > 0.0)	os << ", "    << (*it)->feature_name() << ", " << (*it)->alpha() << ", " << bid;
      else       	os << ",  , "                                  << (*it)->alpha() << ", " << bid;
    if (bid > highBid)
    { highBid = bid;
      pHighBidder = *it;
    }
  }
  if(os)
    os << ", " << pHighBidder->name() << ", " << highBid;
  // inform experts whether win/lose auction
  pHighBidder->bid_accepted();
  for(ExpertIterator it = mExperts.begin(); it != mExperts.end(); ++it)
    if ((*it) != pHighBidder) (*it)->bid_declined();
  return std::make_pair(pHighBidder, highBid);
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
  double *x      (new double[n]);                // who manages this space???
  Column* colPtr (new Column(name.c_str(), x, x+n));
  LinearCombinationFeature *f = new LinearCombinationFeature(beta,useFeatures,colPtr);  
  return f;
}


template <class ModelClass>
FeatureABC*
Auction<ModelClass>::calibration_feature() const
{
  if (not calibrated())
  { std::cerr << "AUCT: *** ERROR *** Attempt to extract calibrator from uncalibrated model.\n";
    return 0;
  }
  else
  { int q (mModel.q()-1);   // dont count calibrator
    std::ostringstream oss;
    oss << q;
    mLogStream << "AUCT: Need to deal with output of a calibration feature.\n" ;
    /* std::string name ("Cal_" + oss.str());     // Name ought to be used
      return mFeatureFactory->make_unary_feature_ptr(mModel.calibration_operator(),
                                                     xb_feature(mModel.calibration_beta()));
    */
    return 0;
  }
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
  if (calibrated())
  { FeatureABC* cb (calibration_feature());
    cb->write_to(os);
    mLogStream << "AUCT: Writing calibration feature " << cb->name() << " with range " << cb->range() << std::endl;
    mLogStream << "       center = " << cb->center()       << "    scale = " << cb->scale() << std::endl;
    delete cb;
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


