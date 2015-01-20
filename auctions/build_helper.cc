#include "build_helper.h"

#include "feature_streams.h"
#include "light_threads.Template.h"
#include "auction.Template.h"
#include "column.h"
#include "debug.h"


void
FiniteCauchyShare::init()
{
  for (int j=0; j<mCount; ++j)
    mSum = mSum + p(j);
}

double
FiniteCauchyShare::operator()(int j) const
{
  assert(j >= 0);
  assert(j < mCount);
  return mTotalAlpha * p(j) / mSum;
}


double
FiniteCauchyShare::p(int j) const
{
  return 1.0/(double)((j+1)*(j+1)); 
}

//     build_regression_model     build_regression_model     build_regression_model     build_regression_model

ValidatedRegression
build_regression_model(Column y, Column inOut, int prefixRows, int blockSize, bool useShrinkage, std::ostream& os)
{
  bool                      useSubset    (0 != inOut->size());
  constant_iterator<double> equalWeights (1.0);
  int                       nRows        ((int)y->size()-prefixRows);
  
  os << "Building regression with " << y->size() << "-" << prefixRows << "=" << nRows << " cases; response is " << y;
  if (useShrinkage)
    os << " with shrinkage." << std::endl;
  else
    os << " without shrinkage." << std::endl;
  if (useSubset)
  { os << "        Validation cases identified by " << inOut << std::endl;
    return ValidatedRegression(y->name(), y->begin()+prefixRows, inOut->begin()+prefixRows, nRows, blockSize, useShrinkage);
  } 
  else
  { os << "        No validation.\n";
    constant_iterator<bool>   noSelection(true);
    return ValidatedRegression (y->name(), y->begin()+prefixRows, noSelection             , nRows, blockSize, useShrinkage);  
  } 
}

//     add_experts_to_auction     add_experts_to_auction     add_experts_to_auction     add_experts_to_auction     add_experts_to_auction     

void
add_source_experts_to_auction (std::vector<Column> const& xColumns, int nPrefixCases, int nContextCases, double wealth, Auction<ValidatedRegression> &auction)
{
  using std::string;
  using debugging::debug;
  
  FeatureSource featureSource (xColumns, nPrefixCases);
  featureSource.print_summary(debug("MAIN",1));

  std::vector<string> streamNames (featureSource.stream_names());
  FeatureVector lockedStream;
  for(auto it = streamNames.begin(); it!=streamNames.end(); ++it)                // remove locked stream
  { if (*it == "LOCKED")
    { debug("MAIN",4) << "Found locked stream; it is not a bidding stream.\n";
      streamNames.erase(it);
      lockedStream =  featureSource.features_with_attribute("stream", "LOCKED");
      break;
    }
  }
  debug("MAIN",1) << "Found " << streamNames.size() << " bidding streams; locked stream has " << lockedStream.size() << " features." << std::endl;
  if(lockedStream.size()>0) 
  { FeatureVector lockIn = featureSource.features_with_attribute ("stream", "LOCKED");
    auction.add_initial_features(lockIn);
    debug("AUCT",1) << auction << std::endl << std::endl;
  }
  typedef FeatureStream< CyclicIterator      <FeatureVector, SkipIfInModel    >, Identity>  FiniteStream;
  typedef FeatureStream< InteractionIterator <FeatureVector, SkipIfRelatedPair>, Identity>  InteractionStream;
  typedef FeatureStream< CrossProductIterator<               SkipIfRelatedPair>, Identity>  CrossProductStream;
  std::vector<FeatureVector> featureStreams(streamNames.size());
  {
    bool     hasLockStream (lockedStream.size() > 0);
    double   alphaShare    (wealth/(double)streamNames.size());
    double   alphaMain     (alphaShare * (hasLockStream ? 0.40 : 0.60 ));  // percentage of alpha to features as given
    double   alphaInt      (alphaShare * (hasLockStream ? 0.31 : 0.40 ));  //                        interactions of given
    double   alphaCP       (alphaShare * (hasLockStream ? 0.29 : 0    ));  //                        cross products
    for (int s=0; s < (int)streamNames.size(); ++s)
    { debug("MAIN",1) << "Allocating alpha $" << alphaShare << " to source experts for stream " << streamNames[s] << std::endl;	
      featureStreams[s] = featureSource.features_with_attribute("stream", streamNames[s]);
      auction.add_expert(Expert("Strm["+streamNames[s]+"]", source, nContextCases, alphaMain,
				   UniversalBoundedBidder<FiniteStream>(), 
				   make_finite_stream(streamNames[s],featureStreams[s], SkipIfInModel())));
      auction.add_expert(Expert("Interact["+streamNames[s]+"]", source, nContextCases, alphaInt,                  // less avoids tie 
				   UniversalBoundedBidder<InteractionStream>(),
				   make_interaction_stream("within " + streamNames[s],
							   featureStreams[s], true)                                  // true means to include squared terms
				   ));
      if (hasLockStream)                                                                                             // cross with locked stream
	auction.add_expert(Expert("CrossProd["+streamNames[s]+" x Lock]", source, nContextCases, alphaCP, 
				     UniversalBoundedBidder<CrossProductStream>(),
				     make_cross_product_stream("CP[" + streamNames[s] + " x Lock]",
							       featureStreams[s], lockedStream )                     
				     ));
    }
  }
}

//     Utilities     Utilities     Utilities     Utilities     Utilities     Utilities     Utilities     Utilities     
double
time_since(time_t const& start)
{
  return  double(clock() - start)/CLOCKS_PER_SEC;
}

std::pair< std::pair<int,double>, std::pair<int,double> >
initialize_sums_of_squares(std::vector<Column> y);

