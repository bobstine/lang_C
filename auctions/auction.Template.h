/* -*- c++ -*-
 *  auction.Template.h
 *
 *  Created from prior cc file by Robert Stine on 1/3/08.
 *  Copyright 2008. All rights reserved.
 *
 */

// #include <functional>  mem_fun

#include <set>
#include <algorithm>

namespace{
  std::string
    remove_comma(std::string const& s)
  {
    std::string result = s;
    for(std::string::iterator it=result.begin(); it!=result.end(); ++it)
      if(*it == ',') *it='_';
    return result;
  }
}

template <class Model>
void
Auction<Model>::write_csv_header_to_progress_stream () const
{
  mProgressStream << "Round, Time, Goodness of Fit, Total Alpha  ";
  for (int b=0; b<number_of_experts(); ++b)
    mProgressStream << ", " << mExperts[b]->name() << " Expert, Alpha, Current Bid";
  mProgressStream << ", Winning Expert, Winning Bid, p value, Variable, Outcome, Payoff, Res SS, CVSS" << std::endl;
  // initial conditions
  std::pair<double,double> ss =  mModel.sums_of_squares();
  mProgressStream << " , , ,";
  for (int b=0; b<number_of_experts(); ++b)
    mProgressStream << ",,, ";
  mProgressStream << ", , , , , , , " << ss.first << "," << ss.second << std::endl;
}


template <class ModelClass>
void
Auction<ModelClass>::prepare_to_start_auction ()
{
  debug("AUCT",3) << "Preparing to run the auction.\n";
  assert (mRound == 0);
  mNumInitialExperts = number_of_experts();
  if(mProgressStream) write_csv_header_to_progress_stream ();
}


template <class ModelClass>
unsigned int
Auction<ModelClass>::add_initial_features (FeatureVector const& f)
{
  debug("AUCT",2) << "Adding " << f.size() << " initial features to the auction model.\n";
  // use an expert to handle the conversion (context rows, cross-validation ordering)
  TestResult result (mModel.add_predictors_if_useful (mExperts[0]->convert_to_model_iterators(f), 1.1));
  debug("AUCT",2) << "Test results are  <" << result.first << "," << result.second << ">\n";
  return f.size();
}

  
// Output to OS must be *csv* delimited columns; each item adds its own prefix , delimiter
template <class ModelClass>
bool
Auction<ModelClass>::auction_next_feature ()
{
  ++mRound;
  debug("AUCT",2) << "Beginning auction round #" << mRound << std::endl;
  // reap empty custom experts
  purge_empty_experts();
  // identify expert with highest total bid; collect_bids writes name, alpha, bid to os
  std::pair<Expert,double> winner (collect_bids());  
  Expert  expert = winner.first;
  double  bid    = winner.second;
  // handle tax on bid
  double afterTaxBid = tax_bid(expert, bid);
  if (0.0 == afterTaxBid) 
  { mHasActiveExpert = false;
    debug("AUCT",0) << "Auction does not have an active expert to bid; terminating.\n";
    if (mProgressStream) mProgressStream << std::endl;
    return false;
  } 
  // extract chosen features
  FeatureVector features (expert->feature_vector());
  if (features.empty())
  { debug("AUCT",-1) << "*** ERROR **** Winning expert " << expert->name() << " did not provide features.\n";
    if (mProgressStream) mProgressStream << std::endl;
    return false;
  }
  else
  { debug("AUCT",3) << "Winning expert " << expert->name()
		    << " bid $" << bid << "(net " << afterTaxBid <<  ")  on ";
    print_features(features, debug());
  }
  // build variables for testing, conversion adjusts for initial context rows
  TestResult result (mModel.add_predictors_if_useful (expert->convert_to_model_iterators(features), afterTaxBid));
  debug("AUCT",2) << "Test results are  <" << result.first << "," << result.second << ">\n";
  if (mProgressStream)
    mProgressStream << ", " << result.second << ", \"" << remove_comma(features[0]->name()) << "\"";
  // report bid result
  double amount;
  bool accepted (result.second < afterTaxBid);
  for (unsigned int j=0; j<features.size(); ++j)
  { features[j]->set_model_results(accepted, result.second);                // save attributes in feature for printing
    if (accepted) 
    { debug("AUCT",0) << "+F+   " << features[j] << std::endl;              // show selected feature in output with key for grepping
      mModelFeatures.push_back(features[j]);
    }
    else      
      mRejectedFeatures.push_back(features[j]);
  }
  if (accepted)                                                             // inform all experts that variable was added
  { for(std::vector<Expert>::iterator it = mExperts.begin(); it != mExperts.end(); ++it)
      (*it)->model_adds_current_variable();
    amount = pay_winning_expert(expert, features);                          // installs experts as needed
    if (mProgressStream)
    { std::pair<double,double> rss (mModel.sums_of_squares());              // resid ss, cv ss
      mProgressStream << ",\"" << remove_comma(features[0]->name()) << "\"," << amount << "," << rss.first << "," << rss.second;
    }
  } else
  { amount = collect_from_losing_expert(expert, bid, (result.second > 1));  // singular?
    if (mProgressStream)
      mProgressStream <<               ", ,"               << amount <<           ", , ";
  }
  return accepted;
}

namespace {
  class Empty {
  private:
    BiddingHistory mHistory;
  public:
    Empty(BiddingHistory const& h) : mHistory(h) { }
    bool operator()(Expert & e) { return e->finished(mHistory); }
  };
}


template<class ModelClass>
int
Auction<ModelClass>::purge_empty_experts()  // purges if does not have feature and custom
{
  int numberPurged (0);
  Empty pred(auction_history());            // Empty defined above
  while (true)
  { ExpertVector::iterator ee (std::find_if(mExperts.begin(), mExperts.end(), pred));
    if(ee == mExperts.end())
      break;
    else
    { mRecoveredAlpha += (*ee)->alpha();
      debug("AUCT",3) << "Recovering alpha " << (*ee)->alpha() << " from " << (*ee)->name() << " boosts total to " << mRecoveredAlpha << ".\n";
      numberPurged += 1;
      mExperts.erase(ee);
    } 
  }
  return numberPurged;
}   
      
					   
namespace {
  double maximum (double a, double b) { return (a>b)?a:b; }
}


template <class ModelClass>
std::pair<Expert,double>
Auction<ModelClass>::collect_bids ()
{
  if (mProgressStream)  // write time stamp info
  { const time_t tmpTime (time(0));
    std::string timeStr (asctime(localtime(&tmpTime)));
    mProgressStream << mRound << ", " << timeStr.substr(0,timeStr.size()-5)
		    << ", " << model_goodness_of_fit()
		    << ", " << total_expert_alpha();
  }
  Expert         winningExpert,    priorityExpert;
  double         highBid (-7.7),   priorityBid (0.0);
  BiddingHistory history  (auction_history());
  int iExpert (0);
  for(ExpertVector::iterator it = mExperts.begin(); it != mExperts.end(); ++it, ++iExpert)
  { double bid = (*it)->place_bid(history);               // pass information to experts; check if has feature
    if (mProgressStream)
      if (iExpert < mNumInitialExperts)
      {	if (bid > 0)
	  mProgressStream << ", \""    << remove_comma((*it)->feature_name()) << "\", " << (*it)->alpha() << ", " << bid; 
	else
	  mProgressStream << ",  , "                                                    << (*it)->alpha() << ", " << bid;
      }
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
    debug("AUCT",3) << "Priority bidder takes bid " << highBid << std::endl;
  }
  if(mProgressStream)
    mProgressStream << "," << remove_comma(winningExpert->name()) << "," << highBid;
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
  { const double taxForEach = tax/features.size();
    for(FeatureVector::const_iterator f = features.begin(); f!=features.end(); ++f)     // add expert for interaction with other added features
    { std::vector<Expert> spawned;
      if ((*f)->has_attribute("interact_with_parent"))
      { std::set<std::string> s ((*f)->attribute_str_value("interact_with_parent"));
	FeatureVector fv;
	for(std::set<std::string>::const_iterator it=s.begin(); it != s.end(); ++it)    // could interact with children of many
	{ std::string parent (*it);  
	  FeatureVector toAppend = mFeatureSource.features_with_attribute("parent",parent);
	  debug("AUCT",4) << toAppend.size() << " features derived from interact_with attribute for parent " << parent << ".\n";
	  for(FeatureVector::const_iterator fit=toAppend.begin(); fit !=toAppend.end(); ++fit)
	    fv.push_back(*fit);
	}
	if (fv.size() > 0)
	  spawned.push_back(Expert(custom, mFeatureSource.number_skipped_cases(), 0.0,  // set alpha wealth later
				   UniversalBidder< RegulatedStream< FeatureProductStream< std::vector<Feature> > > >(),
				   make_feature_product_stream((*f)->name() + " interactions", *f, fv)  ));
      }
      if ((*f)->has_attribute("max_lag"))
      { std::set<int> lagSet ((*f)->attribute_int_value("max_lag"));
	int maxLag (*lagSet.begin());  // only one
	spawned.push_back(Expert(custom, mFeatureSource.number_skipped_cases(), 0.0,    // interacts winning feature with others in model stream
				 UniversalBoundedBidder< RegulatedStream< LagStream > >(),
				 make_lag_stream("Lag stream", *f, maxLag, mBlockSize, 2) ));  // 2 cycles over lags
      }
      // always add to interact winning feature with others in model stream
      spawned.push_back(Expert(custom, mFeatureSource.number_skipped_cases(), 0.0,
			       UniversalBoundedBidder< RegulatedStream< FeatureProductStream< std::vector<Feature> > > >(),
			       make_feature_product_stream("model feature interact", *f, model_features())  ));
      double alpha = taxForEach/spawned.size();
      for(std::vector<Expert>::const_iterator eit=spawned.begin(); eit != spawned.end(); ++eit)
      { (*eit)->increment_alpha(alpha);
	add_expert(*eit);
      }
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
Auction<ModelClass>::print_features(FeatureVector const& features, std::ostream &os)   const
{
  size_t nFeatures (features.size());
  
  if (1==nFeatures) 
    os << "one feature: " << features[0]->name();
  else 
  { os << nFeatures << " features: "; 
    for (size_t j=0; j<nFeatures; ++j)  
      os << features[j]->name() << ", ";
  }
  os << std::endl;
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
  { debug("AUCT",2) << "AUCT: Writing model feature " << mModelFeatures[j]->name()
		    << " (" << j << " / " << mModelFeatures.size() << ")" 
		    << " with range " << mModelFeatures[j]->range() << std::endl
		    << "       center = " << mModelFeatures[j]->center()
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


