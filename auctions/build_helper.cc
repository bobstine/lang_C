#include "build_helper.h"
#include "validated_regression.Template.h"

#include "debug.h"
 
#include "features.Template.h"
#include "feature_predicates.Template.h"
#include "feature_iterators.Template.h"
#include "feature_streams.h"
#include "bidders.h"
#include "experts.Template.h"

#include "light_threads.Template.h"
#include "column.Template.h"


void
FiniteCauchyShare::init()
{
  for (int j=0; j<mCount; ++j)
    mSum = mSum + p(j);
}

SCALAR
FiniteCauchyShare::operator()(int j) const
{
  assert(j >= 0);
  assert(j < mCount);
  return mTotalAlpha * p(j) / mSum;
}


SCALAR
FiniteCauchyShare::p(int j) const
{
  return (SCALAR)1.0/(SCALAR)((j+1)*(j+1)); 
}

//     build_regression_model     build_regression_model     build_regression_model     build_regression_model

Regression
build_regression_model(Column<SCALAR> y, Column<SCALAR> inOut, int prefixRows, int blockSize, bool useShrinkage, std::ostream& os)
{
  bool  useSubset    (0 != inOut->size());
  int   nRows        ((int)y->size()-prefixRows);
  
  os << "Building regression with " << y->size() << "-" << prefixRows << "=" << nRows << " cases; response is " << y;
  if (useShrinkage)
    os << " with shrinkage." << std::endl;
  else
    os << " without shrinkage." << std::endl;
  if (useSubset)
  { os << "        Validation cases identified by " << inOut << std::endl;
    return Regression(y->name(), y->begin()+prefixRows, inOut->begin()+prefixRows, nRows, blockSize, useShrinkage);
  } 
  else
  { os << "        No validation.\n";
    constant_iterator<bool>   noSelection(true);
    return Regression(y->name(), y->begin()+prefixRows,       noSelection        , nRows, blockSize, useShrinkage);  
  } 
}

//     add_experts_to_auction     add_experts_to_auction     add_experts_to_auction     add_experts_to_auction     add_experts_to_auction     

void
add_source_experts_to_auction (FeatureSource const& featureSource, int nContextCases, SCALAR wealth,
			       std::vector<FeatureVector> &featureStreams, Auction<Regression> &auction)
{
  using std::string;
  using debugging::debug;
  
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
  typedef FeatureStream< CyclicIterator      <FeatureVector, SkipIfInModel               >, Identity>  FiniteStream;
  typedef FeatureStream< InteractionIterator <FeatureVector, SkipIfIndicatorsOfSameParent>, Identity>  InteractionStream;
  typedef FeatureStream< CrossProductIterator<               SkipIfRelatedPair>           , Identity>  CrossProductStream;
  const bool purgable = true;
  
  {
    bool     hasLockStream (lockedStream.size() > 0);
    SCALAR   alphaShare    (wealth/(SCALAR)streamNames.size());
    SCALAR   alphaMain     (alphaShare * (hasLockStream ? (SCALAR) 0.40 : (SCALAR) 0.60 ));  // percentage of alpha to features as given
    SCALAR   alphaInt      (alphaShare * (hasLockStream ? (SCALAR) 0.31 : (SCALAR) 0.40 ));  //                        interactions of given
    SCALAR   alphaCP       (alphaShare * (hasLockStream ? (SCALAR) 0.29 : (SCALAR) 0    ));  //                        cross products
    assert (featureStreams.size() == 0);
    if (featureStreams.size() > 0)
    { std::clog << "MAIN:  *** Warning *** Input feature stream will be emptied.\n";
      featureStreams.empty();
    }
    for (int s=0; s < (int)streamNames.size(); ++s)
    { debug("MAIN",1) << "Allocating alpha $" << alphaShare << " to source experts for stream " << streamNames[s] << std::endl;	
      featureStreams.push_back( featureSource.features_with_attribute("stream", streamNames[s]));
      auction.add_expert(Expert("Strm["+streamNames[s]+"]", source, !purgable, nContextCases, alphaMain,
				   UniversalBoundedBidder<FiniteStream>(), 
				   make_finite_stream(streamNames[s],featureStreams[s], SkipIfInModel())));
      auction.add_expert(Expert("Interact["+streamNames[s]+"]", source, !purgable, nContextCases, alphaInt,     // less avoids tie 
				   UniversalBoundedBidder<InteractionStream>(),
				   make_interaction_stream("within " + streamNames[s],
							   featureStreams[s], true)                             // true means to include squared terms
				   ));
      if (hasLockStream)                                                                                             // cross with locked stream
	auction.add_expert(Expert("CrossProd["+streamNames[s]+" x Lock]", source, !purgable, nContextCases, alphaCP, 
				     UniversalBoundedBidder<CrossProductStream>(),
				     make_cross_product_stream("CP[" + streamNames[s] + " x Lock]",
							       featureStreams[s], lockedStream )                     
				     ));
    }
    if (hasLockStream) featureStreams.push_back(lockedStream);
  }
}

  /*
  typedef FeatureStream< DynamicIterator     <FeatureVector, SkipIfDerived    >, BuildPolynomialFeatures >    PolynomialStream;
  typedef FeatureStream< BundleIterator      <FeatureVector, SkipIfInBasis    >, EigenAdapter<PCA> >          PCAStream;
  typedef FeatureStream< BundleIterator      <FeatureVector, SkipIfInBasis    >, EigenAdapter<RKHS<Kernel::Radial> > > RKHSStream;
  typedef FeatureStream< ModelIterator       <ValidatedRegression>, BuildCalibrationFeature<ValidatedRegression> >     CalibrationStream;
  
  // scavenger experts

  theAuction.add_expert(Expert("In*Out", scavenger, !purgable, nContextCases, 0,
			       UniversalBidder<CrossProductStream>(),
			       make_cross_product_stream("accept x reject", theAuction.model_features(), theAuction.rejected_features()) ));

  
  theAuction.add_expert(Expert("Poly", scavenger, !purgable, nContextCases, 0,
			       UniversalBidder< PolynomialStream >(),
			       make_polynomial_stream("Skipped-feature polynomial", theAuction.rejected_features(), 3)     // poly degree
			       ));
  

  //   Principle component type features
  theAuction.add_expert(Expert("PCA", source, !purgable, nContextCases, totalAlphaToSpend/6,                                    // kludge alpha share
			       UniversalBidder<PCAStream>(),
			       make_subspace_stream("PCA", 
						    theAuction.rejected_features(),
						    EigenAdapter<PCA>(PCA(0, true), "PCA", nContextCases),           // # components, standardize? (0 means sing values)
						    30))) ;                                                          // bundle size

  //   RKHS stream
  theAuction.add_expert(Expert("RKHS", source, !purgable, nContextCases, totalAlphaToSpend/6,
			       UniversalBidder<RKHSStream>(),
			       make_subspace_stream("RKHS", 
						    theAuction.rejected_features(),
						    EigenAdapter<RKHS<Kernel::Radial> >(RKHS<Kernel::Radial>(0, true), "RKHS", nContextCases),
						    30)));
  */


//     Utilities     Utilities     Utilities     Utilities     Utilities     Utilities     Utilities     Utilities     
SCALAR
time_since(time_t const& start)
{
  return  SCALAR(clock() - start)/CLOCKS_PER_SEC;
}

std::pair< std::pair<int,SCALAR>, std::pair<int,SCALAR> >
initialize_sums_of_squares(std::vector<Column<SCALAR>> y);

