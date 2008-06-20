/*
 *  auction.Template.h
 *
 *  Created from prior cc file by Robert Stine on 1/3/08.
 *  Copyright 2008. All rights reserved.
 *
 */


template <class ModelClass>
bool
Auction<ModelClass>::auction_next_feature ()
{  
  const time_t tmp_time (time(0));
  std::string print_time (asctime(localtime(&tmp_time)));
  BidCollector collector;
  std::cout << "AUCT: Beginning auction round #" << mFeatures.size()+1 << " (" << print_time.substr(0,print_time.size() - 1) << ")" << std::endl;
                                                                   // find the feature with highest total bid
  for(BidderVector::iterator it = mBidders.begin(); it != mBidders.end(); ++it)
    collector(*it);
  BidderABC::Bid highBid(collector.high_bid());
  mFeatures.push_back(highBid.first);                              //  highbid.first is a feature, save in auction object
                                                                   //  identify the bidders for this feature
  BidderVector bidderVector (collector.bidders_on_feature(highBid.first));
  std::cout << "AUCT: Bidders on " << highBid.first->name() << " are\n" << bidderVector << std::endl;
                                                                   //  test chosen predictor; accept if p-value < bid
  TestResult result (mModel.add_predictor_if_useful (begin(highBid.first->range()), highBid.second));
  bool addedPredictor (result.second < highBid.second);            //  test result.second is p-value 
  highBid.first->set_model_results(addedPredictor, result.second); //  save attributes in the feature for printing
  if (addedPredictor) mModelFeatures.push_back(highBid.first);
  double payoff = (addedPredictor) ? mPayoff : 0.0;
  for (BidderVector::iterator  aBidder = bidderVector.begin(); aBidder != bidderVector.end(); ++aBidder)
  {                                                                // aBidder is basically a handle, a pointer to a pointer
    bool follow_payoff_from_jrss_paper (true);
    if (follow_payoff_from_jrss_paper) 
      (*aBidder)->bid_outcome (highBid.first, payoff);             // (feature, (0,w))    does not get told p-value
    else
    { double bid_amount = (*aBidder)->bid_amount();
      double share = payoff * bid_amount / highBid.second;
      (*aBidder)->bid_outcome (highBid.first, share);
    }
  }                                                                // tell those who passed what happened
  BidCollector::BidderVector passerVector (collector.not_bidders_on_feature(highBid.first));
  for(BidderVector::iterator aBidder = passerVector.begin(); aBidder != passerVector.end(); ++aBidder)
    (*aBidder)->bid_declined (highBid.first);
  std::cout << "+F+   " << highBid.first << std::endl;             // show this feature in output with key for grepping
  return addedPredictor;
} 


/* deal with calibration

std::cout << "AUCT: Removing prior calibrator from model.\n";
mModel.remove_last_predictor();                   // take off the calibrator
std::pair<double,double> resultWithoutCalibrator  // internal result for adding without calibrator
(mModel.add_predictor(bid.first->name(), bid.first->range(), bid.first->center(), bid.first->scale(), 1.0, 1.0)); // pEnter, df
if (resultWithoutCalibrator.first == 0.0)         // numerics failed
{ addedPredictor = false;
  result = resultWithoutCalibrator;
}
else if (resultWithoutCalibrator.second > bid.second)
std::cout << "AUCT: Warning. Auction model rejected predictor without validator (p = "
<< result.second << ") but added anyhow.\n";

if (addedPredictor && (mCalibrationDF>0) && (mModel.number_of_predictors() > 2))
{	std::pair<double, double> calibrationResult;
  std::cout << "AUCT: Adding calibrator with " << mCalibrationDF << " df to underlying logistic model.\n";
  calibrationResult = mModel.add_calibrator(mCalibrationDF, 0.05);
  if (calibrationResult.first == 0.0)  // failure of the MLE optimization
  { std::cout << "AUCT: Warning. Model refused calibrator with " << mCalibrationDF << " df; trying " << mCalibrationDF+3 << "df.\n";
    calibrationResult = mModel.add_calibrator(mCalibrationDF+3, 0.05);
    if (calibrationResult.first == 0.0) 
      std::cout << "AUCT: Warning. Model also refused calibrator with " << mCalibrationDF+3 << " df.\n";
  }
}
*/

template <class ModelClass>
bool
Auction<ModelClass>::has_active_bidder()      const
{
  bool hasBidder (false);
  for (int i=0; i<number_of_bidders(); ++i)
  { hasBidder = mBidders[i]->has_bid();
    if (hasBidder) break;
  }
  return hasBidder;
}  

template <class ModelClass>
double
Auction<ModelClass>::total_bidder_alpha () const
{
  double total (0.0);
  for (int i=0; i<number_of_bidders(); ++i)
  { double prob (mBidders[i]->alpha());
    total += prob;
  }
  return total;
}

// Output

template <class ModelClass>
void
Auction<ModelClass>::print_to (std::ostream& os) const
{
  os << std::endl << "     Auction    " << mBidders.size()
  << " bidders with total alpha " << total_bidder_alpha() << std::endl;
  std::set<const RecommenderABC*> r;
  for (int b=0; b<number_of_bidders(); ++b)
  {
    os << "   Bidder #" << b << " ";
    const RecommenderABC* rec = mBidders[b]->print_to(os);
    os << " [" << rec->name() << "]" << std::endl;
  }
  os << "Recommenders" << std::endl;
  for(std::set<const RecommenderABC*>::const_iterator i = r.begin(); i != r.end(); ++i)
  {
    os << "\n         ";
    (*i)->print_to(os);
  }
  os << "\n" << mModel << std::endl;
}


template <class ModelClass>
void
Auction<ModelClass>::print_features_to (std::ostream& os) const
{
  for (int j=0; j<number_of_features(); ++j)
    os << "   Feature #" << j << ":  " << mFeatures[j] << std::endl;
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
  return mFeatureFactory->make_linear_combination_feature_ptr(beta, useFeatures);
}


template <class ModelClass>
FeatureABC*
Auction<ModelClass>::calibration_feature() const
{
  if (not calibrated())
  { std::cout << "AUCT: *** Error *** Attempt to extract calibrator from uncalibrated model.\n";
    return 0;
  }
  else
  { int q (mModel.q()-1);   // dont count calibrator
    std::ostringstream oss;
    oss << q;
    std::cout << "AUCT: Need to deal with output of a calibration feature.\n" ;
    /* std::string name ("Cal_" + oss.str());     // Name ought to be used
      return mFeatureFactory->make_unary_feature_ptr(mModel.calibration_operator(),
                                                     xb_feature(mModel.calibration_beta()));
    */
  }
}

template <class ModelClass>
void
Auction<ModelClass>::write_model_to  (std::ostream& os) const
{
  os << "Auction Model: ";
  mModel.print_to(os);
  // feature list has <= q items; does not include combinations based on prior predictor
  for (int j=0; j<int(mModelFeatures.size()); ++j)      
  { std::cout << "AUCT: Writing model feature " << mModelFeatures[j]->name()
    << " (" << j << " / " << mModelFeatures.size() << ")" 
    << " with range " << mModelFeatures[j]->range() << std::endl;
    std::cout << "       center = " << mModelFeatures[j]->center()
    << "    scale = " << mModelFeatures[j]->scale() << std::endl;
    mModelFeatures[j]->write_to(os);
  }
  if (calibrated())
  { FeatureABC* cb (calibration_feature());
    cb->write_to(os);
    std::cout << "AUCT: Writing calibration feature " << cb->name() << " with range " << cb->range() << std::endl;
    std::cout << "       center = " << cb->center()       << "    scale = " << cb->scale() << std::endl;
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

template <class Model>
void
Auction<Model>::write_alphas_to (std::ostream& os) const
{
  double total (0.0);
  for (int b=0; b<number_of_bidders(); ++b)
  { double alpha (mBidders[b]->alpha());
    total += alpha;
    os << alpha << " ";
  }
  os << " " << total << " "; 
  mModel.print_gof_to(os) ;
  os << std::endl;
}

template <class ModelClass>
std::ostream&
operator<<(std::ostream& os, Auction<ModelClass> const& auction) 
{ 
  auction.print_to(os); 
  return os; 
}


