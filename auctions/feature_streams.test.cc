/*
 *  feature_streams.test.cc
 *  auctions
 *
 *  Created by Robert Stine on 6/3/10.
 *  Copyright 2010. All rights reserved.
 *
 */


#include "debug.h"
#include "column.h"

#include "feature_streams.h"

#include <iostream>
#include <algorithm>


// Source needs to supply the accept and reject streams

class Model
{
private:
  int mQ;
  int mCases;
  FeatureVector mAccepted;
  FeatureVector mRejected;
  
public:
  Model() : mQ(0), mCases(0), mAccepted(), mRejected() { }
  Model(FeatureVector const& accept, FeatureVector const& reject) : mQ(0), mCases(10), mAccepted(accept), mRejected(reject) { }

  int q()                       const  { return mQ; }
  void increment_q()                   { ++ mQ; }
  int n_total_cases()           const  { return mCases; }
  double y_bar()                const  { return 0.0; }
  
  void fill_with_fit(double *x, bool) const  { for (int i=0; i<mCases;++i) *x++ = 2*i; }
  
  std::vector<std::string> predictor_names() const { std::vector<std::string> names; names.push_back("test"); return names; }
};


template<class Iterator, class Transformation>
void drain_features (FeatureStream<Iterator,Transformation> & fs, int loopLimit)
{
  float ms = 0.010 * 1e3;
  boost::posix_time::milliseconds workTime(ms);
  std::cout << "\nTEST_drain: Loop pauses for " << ms << "ms\n";
  ms = 0.25 * 1e3;
  boost::posix_time::milliseconds pause(ms);
  
  int nPopped (0);
  int nIdle   (0);
  FeatureVector saved;
  bool busy = false;
  bool has = false;
  // test whether has feature first, since that test might make it busy
  while (( (has=fs.has_feature()) || (fs.is_busy()) ) && loopLimit--)
  {
    std::cout << "TEST_drain: At top, is_busy=" << fs.is_busy() << "  has_feature=" << fs.has_feature() << "\n";
    boost::this_thread::sleep(workTime);
    if (fs.has_feature())
    { std::vector<Feature> fv (fs.pop());
      ++nPopped;
      std::for_each(fv.begin(), fv.end(), [&saved](Feature const& f) { saved.push_back(f); });
      if (fv.size()) std::cout << "TEST_drain: feature " << fv[0] << " with " << fs.number_remaining() << " remaining\n" ;
      else           std::cout << "TEST_drain: stream has_feature=true, but popped empty feature vector.\n"; 
    }
    else ++nIdle;
  }
  boost::this_thread::sleep(pause);
  std::cout << "TEST_drain: exiting feature pop loop after popping " << nPopped << " features with " << nIdle << " idle cycles; more = " << loopLimit 
	    << " iterations left with  busy=" << busy << "  and  has_feature=" << has << std::endl;
  std::cout << "TEST_drain: popped features are: \n";
  std::for_each(saved.begin(), saved.end(), [](Feature const& f) { std::cout << "       " << f->name() << std::endl; });
  std::cout << " ------- drain ended.\n\n";
}

int
main()
{
  debugging::debug_init(std::cout,4);
  
  // build vector of columns from file
  const std::string columnFileName ("/Users/bob/C/gsl_tools/data/bank_post45.dat");
  std::vector<Column> columns;
  insert_columns_from_file(columnFileName, back_inserter(columns));
  std::cout << "TEST: Data file " << columnFileName << " produced vector of " << columns.size() << " columns.\n";

  FeatureVector features;
  FeatureVector featureVec1, featureVec2;
  FeatureVector empty;
  
  std::cout << "\n\nTEST: building collection of features\n";
  const int numFeatures (100);
  for (int i=0; i<numFeatures; ++i)
  { features.push_back(Feature(columns[i]));
    featureVec1.push_back(Feature(columns[i]));
    featureVec2.push_back(Feature(columns[i+numFeatures]));
    std::cout << "      : Adding feature " << features[i] << std::endl;
  }
  std::cout << "  -------------------------------------------------------\n\n";



  if (false)         // test cyclic streams
  { 
    std::cout << "\n\nTEST: making feature stream with cyclic iterator over finite collection\n";
    FeatureStream< CyclicIterator<FeatureVector, SkipNone>, Identity> fs (make_finite_stream ("test", features, SkipNone()));
    drain_features(fs, 30);
  }

  
  if (false)         // test dynamic stream
  {
    std::cout << "\n\nTEST: dynamic stream\n";
    FeatureVector fv;
    FeatureStream< DynamicIterator<FeatureVector, SkipIfDerived>, Identity> ds (make_dynamic_stream("dyno", fv, SkipIfDerived(), Identity()));
    int more (25);
    for (int i=0; i<more; ++i)
      fv.push_back(features[i]);
    std::cout << "TEST: Added features; FV.size() = " << fv.size() << std::endl;
    // this drain will not empty any since the test 'has_feature' is late getting all started; that's okay
    drain_features(ds, more+2);
    std::cout << "TEST: After first drain, FV.size() = " << fv.size() << std::endl;
    for (int i=0; i<more; ++i)
      fv.push_back(features[i+more]);
    std::cout << "TEST: Added more; FV.size() = " << fv.size() << std::endl;
    drain_features(ds, 2*more);
    std::cout << "TEST: at end, FV.size() " << fv.size() << std::endl;
  }

  
  if (false)
  {   // test lag streams
    std::cout << "\n\nTEST: lag stream\n";
    FeatureStream<LagIterator, Identity> ls (make_lag_stream("Test", features[0], 4, 2, 1)); // max lag 4, 2 cycles, blocksize 1
    drain_features(ls, 10);
  }

  
  if (false)   // test polynomial stream
  {
    std::cout << "\n\nTEST: making polynomial stream\n";
    FeatureStream< DynamicIterator<FeatureVector, SkipIfDerived>, BuildPolynomialFeatures > ps (make_polynomial_stream("Test", features, 3));
    std::cout << ps << std::endl;
    std::cout << "  Polynomial stream  has_feature=" << ps.has_feature() << " with " << ps.number_remaining() << " left.\n";
    drain_features(ps, 10); // does not apply to dummy inputs
  }
  

  if (false)
  {  // test neighborhood stream; start by making a neighbor vector of integers out of a column
    // will not apply to indicators, so don't get many test data series
    std::cout << "\n\nTEST: making neighborhood stream\n";
    double *p = columns[5]->begin();
    for (int i = 0; i < columns[5]->size(); ++i)
      *p++ = i % 3; // 0 1 2 0 1 2 ...
    std::cout << "    : building integer column.\n";
    IntegerColumn ic(columns[5]);
    std::cout << "      Input column is " << columns[5] << std::endl;
    std::cout << "      Integer column is " << ic << std::endl;
    std::cout << "      Make an indexed feature externally " << make_indexed_feature(features[1],ic) << std::endl;
    FeatureStream< DynamicIterator<FeatureVector, SkipIfDerived>,BuildNeighborhoodFeature> ns (make_neighborhood_stream("Test", features, ic));
    std::cout << "TEST: building features for neighborhood\n";
    drain_features(ns,15);
  }


  if (false)     // test product stream
  {
    std::cout << "\n\nTEST: making product stream\n";
    FeatureStream< QueueIterator<FeatureVector, SkipIfRelated>, BuildProductFeature> ps (make_feature_product_stream ("test", features[0], features));
    std::cout << ps << std::endl;
    std::cout << "  Product stream  has_feature=" << ps.has_feature() << " with " << ps.number_remaining() << " left.\n";
    drain_features(ps,10);
  }


  if (false)    // test calibration stream
  {
    std::cout << "\n\nTEST: making calibration stream\n";
    int  const degree = 3;
    int  const skip = 0;
    bool const binaryResponse (false);
    Model model (featureVec1, featureVec2);

    FeatureStream< ModelIterator<Model>, BuildCalibrationFeature<Model> > cs (make_calibration_stream ("test", model, degree, "Y_hat_", skip, binaryResponse));
    std::cout << cs << std::endl;
    std::cout << "  Calibration stream  has_feature=" << cs.has_feature() << std::endl;
    model.increment_q();
    std::cout << "  After increment, stream  has_feature=" << cs.has_feature() << std::endl;
    drain_features(cs,10);  // hard to test since need to increment q inside drain.
  }


  if(false)    // test subspace 
  {
    std::cout << "\n\nTEST: making subspace stream\n";
    FeatureVector bundle;
    int bundleSize = 5;
    FeatureStream< BundleIterator<FeatureVector, SkipIfInBasis>, VIdentity> bs (make_subspace_stream("test", features, VIdentity(), bundleSize));
    drain_features(bs,15);
  }

    
  if (true)     // test interactions
  { bool const useSquares (true);
    std::cout << "\n\nTEST:  Test of interaction stream.\n";
    FeatureStream< InteractionIterator<FeatureVector, SkipIfRelatedPair>, Identity> is (make_interaction_stream("test", features, useSquares));
    std::cout << " IS has " << is.number_remaining() << " features remaining\n";
    std::cout << "TEST: Cross-product stream has_feature = " << is.has_feature() << std::endl;
    std::cout << "TEST:   on second call,    has_feature = " << is.has_feature() << std::endl;
    std::cout << "TEST:       third call,    has_feature = " << is.has_feature() << std::endl;
    std::cout << "TEST:      fourth call,    has_feature = " << is.has_feature() << std::endl;
    std::cout << "TEST:       fifth call,    has_feature = " << is.has_feature() << std::endl;
    std::cout << "TEST:       sixth call,    has_feature = " << is.has_feature() << std::endl;
    is.print_to(std::cout); std::cout << std::endl;
    drain_features(is,30);
  }

  
  if (false)    // test dynamic cross-product stream
  { std::cout << "\n\nTEST:  Moving on to test other feature streams, now cross-product stream.\n";
    FeatureStream< CrossProductIterator<SkipIfRelatedPair>, Identity > cp (make_cross_product_stream("test", featureVec1, featureVec2));
    std::cout << "TEST: Cross-product stream has_feature = " << cp.has_feature() << std::endl;
    std::cout << "TEST: On second call,    has_feature = " << cp.has_feature() << std::endl;
    drain_features(cp,10);
  }

  
  float ms = 0.5 * 1e3;
  boost::posix_time::milliseconds delay(ms);
  
  std::cout << "\n\nDONE:\n";
  boost::this_thread::sleep(delay);   // try to avoid exiting with running thread
  return 0;
}

