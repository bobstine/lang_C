/*
 *  feature_streams.test.cc
 *  auctions
 *
 *  Created by Robert Stine on 6/3/10.
 *  Copyright 2010. All rights reserved.
 *
 */

#include "column.h"
#include "features.h"
#include "feature_streams.h"
#include "range.h"

#include <iostream>

// Source needs to supply the accept and reject streams

class Model
{
private:
  int mQ;
  int mCases;
  FeatureList mAccepted;
  FeatureList mRejected;
  
public:
  Model(FeatureList const& accept, FeatureList const& reject) : mQ(0), mCases(10), mAccepted(accept), mRejected(reject) { }

  int q()                       const  { return mQ; }
  void increment_q()                   { ++ mQ; }
  int n_total_cases()           const  { return mCases; }
  
  void fill_with_fit(double *x) const  { for (int i=0; i<mCases;++i) *x++ = 2*i; }

  FeatureList const& accepted_features() const { return mAccepted; }
  FeatureList const& rejected_features() const { return mRejected; }
};



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
  FeatureList   featureList1, featureList2;
  FeatureList   empty;
  
  std::cout << "\n\nTEST: building collection of features\n";
  for (int i=0; i<10; ++i)
  { features.push_back(Feature(columns[i]));
    featureList1.push_back(Feature(columns[i]));
    featureList2.push_back(Feature(columns[i+10]));
    std::cout << "      : Adding feature " << features[i] << std::endl;
  }
  std::cout << "  -------------------------------------------------------\n";

  if (false)
  {   // test Finite streams
    std::cout << "\n\nTEST: making regulated finite stream\n";
    FeatureStream< CyclicIterator<FeatureVector,SkipNone>, Identity > fs (make_finite_stream("Test", features, SkipNone()));

    std::cout << "TEST: FS has_feature = " << fs.has_feature() << std::endl;
    std::vector<Feature> fv (fs.pop());
    std::cout << "TEST:    Popped feature " << fv[0] << std::endl;
    std::cout << "TEST: FS has_feature = " << fs.has_feature() << std::endl;
    
    int more (7);
    while(fs.has_feature() && more--)
    { std::vector<Feature> fv (fs.pop());
      std::cout << "TEST:    Popped feature " << fv[0] << " with " << fs.number_remaining() << " remaining\n" ;
    }

    std::cout << "\n\nTEST:  Setting model results for features \n";
    fv[0] -> set_model_results(true, 0.001);  // used in model, p-value
    more = 7;
    while(fs.has_feature() && more--)
    { std::vector<Feature> fv (fs.pop());
      std::cout << "TEST:    Popped feature " << fv[0] << std::endl;
    }
  }


  if (false)
  {   // test lag streams
    std::cout << "\n\n\n\nTEST: making regulated lag stream\n";
    FeatureStream< LagIterator, Identity > ls (make_lag_stream("Test", features[0], 4, 1, 2)); // max lag 4, 2 cycles

    for (unsigned i=0; i<10; ++i)
    { if (ls.has_feature())
      {	FeatureVector fv (ls.pop());
	std::cout << "   popped...  " << fv[0] << std::endl;
      }
      ls.print_to(std::cout);
    }
  }


  if (false)
  {  // test neighborhood stream; start by making a neighbor vector of integers out of a column
    std::cout << "\n\n\n\nTEST: making neighborhood stream\n";
    double *p = columns[5]->begin();
    for (int i = 0; i < columns[5]->size(); ++i)
      *p++ = i % 3; // 0 1 2 0 1 2 ...
    std::cout << "    : building integer column.\n";
    IntegerColumn ic(columns[5]);
    std::cout << "      Input column is " << columns[5] << std::endl;
    std::cout << "      Integer column is " << ic << std::endl;
    std::cout << "      Make an indexed feature externally " << make_indexed_feature(features[1],ic) << std::endl;
    FeatureStream< DelayedIterator<FeatureVector, SkipIfDerived>,BuildNeighborhoodFeature> ns (make_neighborhood_stream("Test", features, ic));
    std::cout << "TEST: building features for neighborhood\n";
    if (ns.has_feature())
    { FeatureVector fv (ns.pop());
      std::cout << "   feature is " << fv[0] << std::endl;
    }
    else std::cout << "    NS says it does not have a feature; its number remaining is " << ns.number_remaining() << std::endl;
    for (unsigned i=0; i<2; ++i)
    { if (ns.has_feature())
      { FeatureVector ff (ns.pop());
	ff[0]->print_to(std::cout);
      }
      else std::cout << "TEST: neighborhood stream is empty.\n";
    }
  }

  
  if (false)   // test polynomial streams, two versions of the regulated streams (one dynamic and the other static)
  {
    std::cout << "\n\n\n\nTEST: making polynomial stream\n";
    FeatureStream< DelayedIterator<FeatureVector, SkipIfDerived>, BuildPolynomialFeature> ps (make_polynomial_stream("Test", features, 3));
    ps.print_to(std::cout);
    std::cout << "  Polynomial stream  has_feature=" << ps.has_feature() << " with " << ps.number_remaining() << " left.\n";
    while(ps.has_feature())
    { FeatureVector fv = ps.pop();
      std::cout << "  Leading popped feature: " << fv[0] << "  ; " << fv.size() << " features in vector\n";
    }
    std::cout << " stream has " << ps.number_remaining() << " features remaining\n";
  }
  

  if (false)     // test product stream
  {
    std::cout << "\n\n\nTEST: making product stream\n";
    FeatureStream< FeatureQueueIterator<FeatureVector, SkipIfRelated>, BuildProductFeature> ps (make_feature_product_stream ("test", features, features[0]));
    std::cout << ps << std::endl;
    std::cout << "  Product stream  has_feature=" << ps.has_feature() << " with " << ps.number_remaining() << " left.\n";
    while(ps.has_feature())
    { FeatureVector fv = ps.pop();
      std::cout << "  Leading popped feature: " << fv[0] << "  ; " << fv.size() << " features in vector\n";
    }
    std::cout << " stream has " << ps.number_remaining() << " features remaining\n";
  }

  
  if (true)    // test calibration stream
  {
    std::cout << "\n\n\nTEST: making calibration stream\n";
    int degree = 3;
    int skip = 0;
    Model model (featureList1, featureList2);

    FeatureStream< ModelIterator<Model>, BuildCalibrationFeature<Model> > cs (make_calibration_stream ("test", model, degree, skip));
    std::cout << cs << std::endl;
    std::cout << "  Calibration stream  has_feature=" << cs.has_feature() << std::endl;
    int max = 3;
    model.increment_q();
    std::cout << "  After increment, stream  has_feature=" << cs.has_feature() << std::endl;
    while(cs.has_feature() && --max)
    { FeatureVector fv = cs.pop();
      std::cout << "  Leading popped feature: " << fv[0] << "  ; " << fv.size() << " features in vector\n";
      std::cout << "  Calibration stream " << cs << std::endl;
      model.increment_q();
    }
  }
    
  /*  
  if (true)     // test interactions
  { std::cout << "\n\nTEST:  Test of interaction stream.\n";
    typedef  RegulatedStream< InteractionStream< std::vector<Feature> > > IS;
    IS is (make_interaction_stream("test", make_range(empty), features, true));  // true means to use diagonal
    std::cout << " IS has " << is.number_remaining() << " features remaining\n";
    
    std::cout << "TEST: has_feature = " << is.has_feature() << std::endl;
    is.print_to(std::cout); std::cout << std::endl;
    
    std::cout << "TEST: pop off in loop\n";
    int count (0);
    while(is.has_feature())
    { FeatureVector fv = is.pop();
      std::cout << "  Popped feature " << ++count << ": " << fv[0] << "  ====  " << is.number_remaining() << " remain" << std::endl;
    }
    std::cout << " IS has " << is.number_remaining() << " features remaining\n";
  }
  */

  /*
  if (true)    // test dynamic cross-product stream
  { std::cout << "\n\nTEST:  Moving on to test other feature streams, now cross-product stream.\n";
    typedef  RegulatedStream< CrossProductStream< std::vector<Feature>,std::vector<Feature> > > CP;
    std::vector<Feature> featuresSlow, featuresFast;
    CP cp (make_cross_product_stream("CP stream",  make_range(empty), featuresSlow, featuresFast));
    
    std::cout << "TEST: has_feature = " << cp.has_feature() << std::endl;
    cp.print_to(std::cout); std::cout << std::endl;
    
    std::cout << "\n\nTEST: adding features\n";
    featuresSlow.push_back(Feature(columns[0]));  std::cout << "Slow <- " << columns[0]->name() << std::endl;
    featuresFast.push_back(Feature(columns[1]));  std::cout << "Fast <- " << columns[1]->name() << std::endl;
    featuresFast.push_back(Feature(columns[2]));  std::cout << "Fast <- " << columns[2]->name() << std::endl;
    
    std::cout << "TEST: has_feature = " << cp.has_feature() << std::endl;
    cp.print_to(std::cout); std::cout << std::endl;
    
    std::cout << "TEST: first pops\n";
    FeatureVector blank;
    while(!cp.has_feature())
    { std::vector<Feature> fv (cp.pop());
      std::cout << "Popped feature " << fv[0] << std::endl;
    }
    std::cout << "TEST: has_feature = " << cp.has_feature() << std::endl;
    cp.print_to(std::cout); std::cout << std::endl;

    return 0;
    
    std::cout << "\n\nTEST: adding features\n";
    featuresFast.push_back(Feature(columns[3]));  std::cout << "Fast <- " << columns[3]->name() << std::endl;
    featuresFast.push_back(Feature(columns[4]));  std::cout << "Fast <- " << columns[4]->name() << std::endl;
    featuresFast.push_back(Feature(columns[5]));  std::cout << "Fast <- " << columns[5]->name() << std::endl;
    
    std::cout << "TEST: has_feature = " << cp.has_feature() << std::endl;
    cp.print_to(std::cout); std::cout << std::endl;
    
    std::cout << "\nTEST: second pops\n";
    while(cp.has_feature())
    { std::vector<Feature> fv (cp.pop());
      std::cout << "Popped feature " << fv[0] << std::endl;
    }
    
    std::cout << "\n\nTEST: adding features\n";
    featuresSlow.push_back(Feature(columns[6]));  std::cout << "Slow <- " << columns[6]->name() << std::endl;
    featuresSlow.push_back(Feature(columns[7]));  std::cout << "Slow <- " << columns[7]->name() << std::endl;
    featuresFast.push_back(Feature(columns[8]));  std::cout << "Fast <- " << columns[8]->name() << std::endl;
    
    std::cout << "TEST: has_feature = " << cp.has_feature() << std::endl;
    cp.print_to(std::cout); std::cout << std::endl;
    
    std::cout << "\nTEST: third pops\n";
    while(!cp.has_feature())
    { std::vector<Feature> fv (cp.pop());
      std::cout << "Popped feature " << fv[0] << std::endl;
    }
  }
  */
  
  std::cout << "\n\nDONE:\n";
  return 0;
}

