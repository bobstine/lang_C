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
  debug("AUCT",2) << "Beginning auction round #" << mRound << std::endl;
  // identify expert with highest total bid; collect_bids writes name, alpha, bid to os
  std::pair<Expert,double> winner (collect_bids(os));  
  Expert  expert = winner.first;
  double  bid    = winner.second;
  // handle tax on bid
  double afterTaxBid = tax_bid(expert, bid);
  if (0.0 == afterTaxBid) 
  { mHasActiveExpert = false;
    debug("AUCT",3) << "Auction does not have an active expert to bid; terminating.\n";
    if (os) os << std::endl;
    return false;
  } 
  // extract chosen features
  std::vector<Feature> features  (expert->feature_vector()) ;
  if (features.empty())
  { debug("AUCT",3) << "*** ERROR **** Winning expert " << expert->name() << " did not provide features.\n";
    if (os) os << std::endl;
    return false;
  }
  else
  { debug("AUCT",0) << "Winning expert  " << expert->name()
		    << " bids $" << bid << "(net " << afterTaxBid <<  ")  on ";
    print_features(features);
  }
  // build variables for testing
  std::vector< std::pair<std::string, FeatureABC::Iterator> > namedIterators;
  for (unsigned int j=0; j<features.size(); ++j)
    namedIterators.push_back( make_pair(features[j]->name(), features[j]->begin()));
  TestResult result (mModel.add_predictors_if_useful (namedIterators, afterTaxBid));
  debug("AUCT",0) << "Test results are  <" << result.first << "," << result.second << ">\n";
  if (os)
    os << ", " << result.second << ", " << features[0]->name();
  // report bid result
  double amount;
  bool accepted (result.second < afterTaxBid);
  if (accepted)
    amount = pay_winning_expert(expert, features);                          // installs experts as needed
  else
    amount = collect_from_losing_expert(expert, bid, (result.second > 1));  // singular?
  if (os)
    if (accepted)
    { std::pair<double,double> rss (mModel.sums_of_squares());
      os << "," << features[0]->name() << "," << amount << "," << rss.first << "," << rss.second;
    }
    else
      os <<               ", ,"               << amount <<           ", , ";
  for (unsigned int j=0; j<features.size(); ++j)
  { features[j]->set_model_results(accepted, result.second);           // save attributes in feature for printing
    if (accepted) 
    { mLogStream << "+F+   " << features[j] << std::endl;              // show selected feature in output with key for grepping
      mModelFeatures.push_back(features[j]);
    }
    else      
      mRejectedFeatures.push_back(features[j]);
  }
  return accepted;
}


template <class ModelClass>
std::pair<Expert,double>
Auction<ModelClass>::collect_bids (std::ostream& os)
{
  if (os)
  { const time_t tmpTime (time(0));
    std::string timeStr (asctime(localtime(&tmpTime)));
    os << mRound << ", " << timeStr.substr(0,timeStr.size()-5)
       << ", " << model_goodness_of_fit()
       << ", " << total_expert_alpha();
  }
  Expert       winningExpert,    priorityExpert;
  double       highBid (-7.7),   priorityBid (0.0);
  AuctionState state (auction_state());
  for(ExpertVector::iterator it = mExperts.begin(); it != mExperts.end(); ++it)
  { double bid = (*it)->place_bid(state);               // pass information to experts
    if (os)
      if (bid > 0)	os << ", "    << (*it)->feature_name() << ", " << (*it)->alpha() << ", " << bid;
      else       	os << ",  , "                                  << (*it)->alpha() << ", " << bid;
    if (bid > highBid)
    { highBid = bid;
      winningExpert = *it;
    }
    if (bid>0 && (calibrate == (*it)->role()))
    { priorityExpert = *it;
      priorityBid    = bid;
    }
  }
  if (!priorityExpert.empty())                          // override results
  { highBid = priorityBid;
    winningExpert = priorityExpert;
    debug(3) << "AUCT: Priority bidder takes bid " << highBid << std::endl;
  }
  if(os)
    os << ", " << winningExpert->name() << ", " << highBid;
  winningExpert->bid_accepted();                     // inform experts whether win/lose auction
  for(ExpertVector::iterator it = mExperts.begin(); it != mExperts.end(); ++it)
    if ((*it) != winningExpert) (*it)->bid_declined();
  return std::make_pair(winningExpert, highBid);
}


template<class ModelClass>
double
Auction<ModelClass>::tax_bid (Expert winningExpert, double bid)
{
  if (winningExpert->role() == calibrate)    // do nothing to calibrator bid ???  Is that right? overfit calibration?
    return bid;
  else
  { double tax (mBidTaxRate * bid);
    int  parasiteCount (0);
    for (ExpertVector::iterator i=mExperts.begin(); i!=mExperts.end(); ++i) // count parasitic bidders
      if( (*i)->role() == parasite)
	++parasiteCount;
    debug("AUCT",4) << "Distributing bid tax $" << tax << " among " << parasiteCount << " parasites.\n";
    if (parasiteCount == 0)
      return bid;
    else
    { for (ExpertVector::iterator i=mExperts.begin(); i!=mExperts.end(); ++i) // distribute tax among parasitic bidders
	if( (*i)->role() == parasite)
	  (*i)->increment_alpha(tax/parasiteCount);
      return bid-tax;
    }
  }
}

template<class ModelClass>
double
Auction<ModelClass>::pay_winning_expert (Expert expert, FeatureVector const& features)
{
  double tax = mPayoffTaxRate * mPayoff;
  expert->payoff(mPayoff-tax);        
  if (expert->role() != calibrate)
  { 
    for(FeatureVector::const_iterator f = features.begin(); f!=features.end(); ++f) // add expert for interaction with other added features
    { double taxForEach = tax/features.size();
      if ((*f)->has_attribute("interacts"))
      {	FeatureVector fv = features_with_attribute((*f)->attribute_str_value("interact"));
	if (fv.size() == 0)
	  debug("AUCT",4) << "Warning: no features with attribute " << (*f)->attribute_str_value("interact") << std::endl;
	else
	{ taxForEach /= 2;
	  add_expert(Expert(custom, taxForEach,
			    UniversalBidder< RegulatedStream< FeatureProductStream< std::vector<Feature> > > >(),
			    make_feature_product_stream(*f, fv)  ));
	}
      }
      add_expert(Expert(parasite, taxForEach,
			UniversalBidder< RegulatedStream< FeatureProductStream< std::vector<Feature> > > >(),
			make_feature_product_stream(*f, model_features())  ));
    }
  }
  mPayoffHistory.push_back(mPayoff);
  return mPayoff;
}


template<class ModelClass>
double
Auction<ModelClass>::collect_from_losing_expert (Expert expert, double bid, bool singular)
{
  double cost (0.0);
  if (singular)                  // pays for tax on bid if singular
    cost = - bid * mBidTaxRate;
  else
    cost = - bid/(1-bid);
  expert->payoff(cost);
  mPayoffHistory.push_back(cost);
  return cost;
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

template <class ModelClass>
std::vector< Feature >
Auction<ModelClass>::features_with_attribute (std::string attr) const
{
  std::vector< Feature > fv;

  for(FeatureVector::const_iterator f = mSourceFeatures.begin(); f != mSourceFeatures.end(); ++f)
    if ( (*f)->has_attribute(attr) )
      fv.push_back(*f);
  return fv;
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


