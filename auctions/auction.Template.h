/* -*- c++ -*-
 *  auction.Template.h
 *
 *  Created from prior cc file by Robert Stine on 1/3/08.
 *  Copyright 2008. All rights reserved.
 *
 */

// #include <functional>  mem_fun

#include "debug.h"
using debugging::debug;

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

//  utilities      utilities      utilities      utilities      utilities      utilities

template <class ModelClass>
bool
Auction<ModelClass>::is_calibration_feature(Feature const& f)                      const
{
  if(mCalibrateFit)
  { std::string name = f->name();
    return (std::string::npos != name.find(mCalibrationSignature));       // ::npos means not found
  }
  else return false;
}

template <class ModelClass>
FeatureVector
Auction<ModelClass>::without_calibration_features(FeatureVector const& fv)          const
{
  FeatureVector result;
  for(FeatureVector::const_iterator i=fv.begin(); i != fv.end(); ++i)
    if (!is_calibration_feature(*i)) result.push_back(*i);
  return result;
}


//  intialization     intialization     intialization     intialization     intialization     intialization

template <class ModelClass>
unsigned int
Auction<ModelClass>::add_initial_features (FeatureVector const& f)
{
  debug("AUCT",2) << "Adding " << f.size() << " initial features to the auction model.\n";
  // use an expert to handle the conversion (context rows, cross-validation ordering)
  TestResult result (mModel.add_predictors_if_useful (mExperts[0]->convert_to_model_iterators(f), 1.1));
  for (unsigned int j=0; j<f.size(); ++j)
  { f[j]->set_model_results(true, 0.10);                  // assign bid results as if accepted with large bid
    mModelFeatures.push_back(f[j]);
  }
  debug("AUCT",2) << "Test of initial features gives  <" << result.first << "," << result.second << ">\n";
  return f.size();
}


template <class Model>
void
Auction<Model>::write_header_to_progress_stream () const
{
  mProgressStream << "Round\tTime\tGoodness of Fit\tTotal Alpha";
  for (int b=0; b<number_of_experts(); ++b)
    mProgressStream << "\t" << mExperts[b]->name() << "Expert\tAlpha\tCurrent Bid";
  mProgressStream << "\tWinning Expert\tWinning Bid\tp value\tVariable\tOutcome\tPayoff\tRes SS\tCVSS" << std::endl;
  // initial conditions
  std::pair<double,double> ss =  mModel.sums_of_squares();
  mProgressStream << "\t\t\t";
  for (int b=0; b<number_of_experts(); ++b)
    mProgressStream << "\t\t\t";
  mProgressStream << "\t\t\t\t\t\t\t" << ss.first << "\t" << ss.second << std::endl;
}


template <class ModelClass>
void
Auction<ModelClass>::prepare_to_start_auction ()
{
  debug("AUCT",3) << "Preparing to run the auction.\n";
  assert (mRound == 0);
  mNumInitialExperts = number_of_experts();
  if(mProgressStream) write_header_to_progress_stream ();
}

  
// Output to OS must be *csv* delimited columns; each item adds its own prefix , delimiter
template <class ModelClass>
bool
Auction<ModelClass>::auction_next_feature ()
{
  ++mRound;
  debug("AUCT",2) << "\n\nBeginning auction round #" << mRound << std::endl; 
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
  // extract chosen features, optionally print name of first
  FeatureVector features (expert->feature_vector());
  if (features.empty())
  { debug("AUCT",-1) << "*** ERROR **** Winning expert " << expert->name() << " did not provide features.\n";
    if (mProgressStream) mProgressStream << std::endl;
    return false;
  }
  else
  { const unsigned int len (40);
    debug("AUCT",1) << std::setw(len) << expert->name(len) << "   bid   $" << std::setw(10) << std::left << bid
		    << " on " << features.size() << " features, led by " << features[0]->name() << std::endl;
    debug("AUCT",3) << "Winning expert " << expert << std::endl;
  }
  // build variables for testing, conversion adjusts for initial context rows
  TestResult result (mModel.add_predictors_if_useful (expert->convert_to_model_iterators(features), afterTaxBid));
  debug("AUCT",2) << "Test results are  <" << result.first << "," << result.second << ">\n";
  if (mProgressStream)
    mProgressStream << "\t" << result.second << "\t" << remove_comma(features[0]->name());
  // report bid result
  double amount;
  bool accepted (result.second < afterTaxBid);
  for (unsigned int j=0; j<features.size(); ++j)
  { bool newFeature (!features[j]->was_tried_in_model());
    if (newFeature || accepted)
      features[j]->set_model_results(accepted, afterTaxBid);                // save attributes only from first attempt in auction or when accepted
    else
      debug("AUCT",4) << "Old feature " << features[j]->name() << " tested in auction.\n";
    if (accepted) 
    { debug("AUCT",0) << "+F+   " << features[j] << std::endl;              // show selected feature in output with key for grepping
      mModelFeatures.push_back(features[j]);
    }
    else if ( (!is_calibration_feature(features[j])) && newFeature )     
      mRejectedFeatures.push_back(features[j]);
  }
  if (accepted)                                                             // inform all experts that variable was added
  { for(std::vector<Expert>::iterator it = mExperts.begin(); it != mExperts.end(); ++it)
      (*it)->model_adds_current_variable();
    amount = pay_winning_expert(expert, features);                          // installs experts as needed
    if (mProgressStream)  mProgressStream << "\t" << remove_comma(features[0]->name()) << "\t" << amount;
  }
  else
  { amount = collect_from_losing_expert(expert, bid, (result.second > 1));  // singular?
    if (mProgressStream)  mProgressStream << "\t\t" << amount;
  }
  std::pair<double,double> rss (mModel.sums_of_squares());                  // resid ss, cv ss
  if (mProgressStream) mProgressStream << "\t" << rss.first << "\t" << rss.second;
  return accepted;
}

namespace {
  class Empty {
  private:
    BiddingHistory mHistory;
  public:
    Empty(BiddingHistory const& h) : mHistory(h) { }
    bool operator()(Expert & e) { return e->finished(); }
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
    mProgressStream << mRound
		    << "\t" << timeStr.substr(0,timeStr.size()-5)
		    << "\t" << model_goodness_of_fit()
		    << "\t" << total_expert_alpha();
  }
  Expert         winningExpert,    priorityExpert;
  double         highBid (-7.7),   priorityBid (0.0);
  BiddingHistory history  (auction_history());
  int iExpert (0);
  for(ExpertVector::iterator it = mExperts.begin(); it != mExperts.end(); ++it, ++iExpert)
  { double bid = (*it)->place_bid(history);               // pass information to experts; check if has feature
    if (mProgressStream)
      if (iExpert < mNumInitialExperts)                   // only track output for initial experts
      {	if (bid > 0)
	  mProgressStream << "\t"    << remove_comma((*it)->feature_name()) << "\t" << (*it)->alpha() << "\t" << bid; 
	else
	  mProgressStream << "\t\t"                                                 << (*it)->alpha() << "\t" << bid;
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
    mProgressStream << "\t" << remove_comma(winningExpert->name()) << "\t" << highBid;
  winningExpert->bid_accepted();                     // inform experts whether win/lose auction
  for(ExpertVector::iterator it = mExperts.begin(); it != mExperts.end(); ++it)
    if ((*it) != winningExpert) (*it)->bid_declined();
  return std::make_pair(winningExpert, highBid);
}


template<class ModelClass>
double
Auction<ModelClass>::tax_bid (Expert winningExpert, double bid)
{
  if (winningExpert->role() == calibrate)                                     // no tax revenue generated by taxing calibration bid
    return bid;
  else
  { double tax (mBidTaxRate * bid);
    int  parasiteCount (0);
    for (ExpertVector::iterator i=mExperts.begin(); i!=mExperts.end(); ++i)   // count parasitic bidders
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

namespace {
  int count_occurences (std::string const& s, char c)
  {
    int count (0);
    for (std::string::const_iterator it=s.begin(); it!=s.end(); ++it)
      if (*it == c) ++count;
    return count;
  }
}

template<class ModelClass>
double
Auction<ModelClass>::pay_winning_expert (Expert expert, FeatureVector const& features)
{
  typedef FeatureStream<LagIterator, Identity>                                              LagStream;
  typedef FeatureStream< QueueIterator<FeatureVector, SkipIfRelated>, BuildProductFeature>  ProductStream;
  double tax = mPayoffTaxRate * mPayoff;
  expert->payoff(mPayoff-tax);
  if (expert->role() != calibrate)
  { const double taxForEach = tax/features.size();
    for(FeatureVector::const_iterator f = features.begin(); f!=features.end(); ++f)       // add expert for interaction with other added features
    {
      // each added feature can potentially add several experts to the auction
      std::vector<Expert> spawned;
      // iteract feature with those that have indicated parent
      if ((*f)->has_attribute("interact_with_parent"))
      { std::set<std::string> s ((*f)->attribute_str_value("interact_with_parent"));
	FeatureVector fv;
	for(std::set<std::string>::const_iterator it=s.begin(); it != s.end(); ++it)      // could interact with children of many
	{ std::string parent (*it);  
	  FeatureVector toAppend = mFeatureSource.features_with_attribute("parent",parent);
	  debug("AUCT",4) << toAppend.size() << " features derived from interact_with attribute for parent " << parent << ".\n";
	  for(FeatureVector::const_iterator fit=toAppend.begin(); fit !=toAppend.end(); ++fit)
	    fv.push_back(*fit);
	}
	fv = without_calibration_features(fv);
	if (fv.size() > 0)
	  if((*f)->degree() == 1)                                                        // only cross simple features with stream
	    spawned.push_back(Expert("Spawn["+(*f)->name()+"_"+*s.begin()+"]", custom, mFeatureSource.number_skipped_cases(), 0.0,  // set alpha wealth later
				     UniversalBidder< ProductStream >(),
				     make_feature_product_stream("interactor", *f, fv)  ));
      }
      if ((*f)->has_attribute("max_lag"))
      { std::set<int> lagSet ((*f)->attribute_int_value("max_lag"));
	int maxLag (*lagSet.begin());                                                                          // only one
	spawned.push_back(Expert("Lag_"+(*f)->name(), custom, mFeatureSource.number_skipped_cases(), 0.0,      // interacts winning feature with others in model stream
				 UniversalBoundedBidder< LagStream >(),
				 make_lag_stream("Lag stream", *f, maxLag, 2, mBlockSize) ));                  // 2 cycles over lags
      }
      // interact winning feature with rest of model stream
      spawned.push_back(Expert("Cross["+(*f)->name()+",model]", custom, mFeatureSource.number_skipped_cases(), 0.0,
			       UniversalBoundedBidder< ProductStream >(),
			       make_feature_product_stream("winner", *f, without_calibration_features(model_features()))  ));
      double alpha = taxForEach/spawned.size();
      /*  used to have a global "in model/in model" interaction expert.  That's now replaced by these for each variable...
	  the new interaction stream does not allow dynamic growth, which is needed for this
	  Do we need both "individual" expert as well as the global expert for smoothing this out???
	  
	  double alpha = taxForEach/(1+spawned.size());
	  if(mExperts[0]->name() == "In/In")
	  { debugging::debug("AUCT", 3) << "Assigning alpha " << alpha << " to In/In interaction expert and " << spawned.size() << " spawned experts.\n";
	  mExperts[0]->increment_alpha(alpha);                                             // global expert in first slot
	  }
	  else
	  std::cerr << "ERROR: Interaction error not located at head of expert vector in auction.\n";
      */
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
  int     n      (mModel.n_all_cases());                 // include those used in validation
  LinearCombinationFeature *f = new LinearCombinationFeature(n, beta,useFeatures);
  return f;
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


