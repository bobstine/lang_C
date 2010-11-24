/*
 *  feature_iterators.test.cc
 *  auctions
 *
 *  Created by Robert Stine on 11/17/10.
 *  Copyright 2010. All rights reserved.
 *
 */

#include "debug.h"
#include "column.h"
#include "features.h"
#include "feature_iterators.h"
#include "feature_predicates.h"

#include <iostream>


//  use this model as feature source
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


std::ostream&
operator<< (std::ostream& os, Model const& m) { os << "Model @ q = " << m.q() << " "; return os; }



//   ---------------------------------

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
  FeatureVector featureVec1,  featureVec2;
  FeatureList   featureList1, featureList2;
  std::cout << "\n\nTEST: building collections of features\n";
  const int numberOfFeatures (10);
  for (int i=0; i<numberOfFeatures; ++i)
  { features.push_back(Feature(columns[i]));
    featureList1.push_back(Feature(columns[i]));
    featureList1.push_back(Feature(columns[i+numberOfFeatures]));
    featureVec1.push_back(Feature(columns[i]));
    featureVec2.push_back(Feature(columns[i+numberOfFeatures]));
    std::cout << "      : Adding feature " << features[i] << std::endl;
  }
  std::cout << "  -------------------------------------------------------\n";
  

  
  if (false)        // test queue iterator
  {
    std::cout << "\n\nTEST: queue iterator\n";
    bool include (true);
    for (unsigned i; i<features.size(); ++i)  // set half to be in model, with bids in reverse order
    { include = !include;
      features[i]->set_model_results(include, ((double)i)/10.0);
    }
    QueueIterator<FeatureVector, SkipIfInModel> it (features, SkipIfInModel());
    int more (20);
    while(it.valid() && --more)
    { std::cout << "TEST_queue: *it = " << *it << std::endl;
      ++it;
    }
    std::cout << "TEST_queue: complete, it = " << it << std::endl;
  }


  if (false)         // test cyclic iterator
  { 
    std::cout << "\n\nTEST: making cyclic iterator over finite collection\n";
    CyclicIterator<FeatureVector, SkipNone> it (features, SkipNone());

    for(int i=0; i<20; ++i)
    { if (it.valid())
	std::cout << "TEST_cyclic: *it = " << (*it)->name() << std::endl;
      ++it;
    }
  }


  if (false)         // test dynamic iterator
  {
    std::cout << "\n\nTEST: dynamic iterator\n";
    FeatureVector fv;
    DynamicIterator<FeatureVector, SkipNone> it (fv, SkipNone());

    int nToAdd (3);
    for (int i=0; i<nToAdd; ++i)
      fv.push_back(features[i]);
    while(it.valid())
    { std::cout << "TEST_dynamic: it = " << it << "     *it = " << (*it)->name() << std::endl;
      ++it;
    }
    for (int i=0; i<nToAdd; ++i)
      fv.push_back(features[nToAdd + i]);
    std::cout << "FV.size() " << fv.size() << std::endl;
    while(it.valid())
    { std::cout << "TEST_dynamic: it = " << it << "     *it = " << (*it)->name() << std::endl;
      ++it;
    }
    std::cout << "FV.size() " << fv.size() << std::endl;
  }

  
  if (false)     //    test lag iterators
  {   
    std::cout << "\n\nTEST: making lag iterator\n";
    LagIterator it (features[0], 4, 2, 1); // max lag 4, 2 cycles, blocksize 1
    
    while(it.valid())
    { std::cout << "TEST_lag: it = " << it << "     *it = " << (*it)->name() << std::endl;
      ++it;
    }
    std::cout << "TEST_lag: completed, it = " << it << std::endl;
  }

    
  if (false)    //        test model iterator
  {
    std::cout << "\n\nTEST:  make model iterator\n";
    Model model (featureList1, featureList2);
    ModelIterator<Model> it (model);
    
    int max (4);
    model.increment_q();
    while(it.valid() && --max)
    { 
      std::cout << "TEST_model: popped " << *it << std::endl;
      model.increment_q();
    }
    std::cout << "TEST_model: completed, it = " << it << std::endl;
  }


  if(false)           // test subspace iterator
  {
    std::cout << "\n\nTEST: testing subspace iterator\n";
    FeatureVector bundle;
    int bundleSize = 3;
    BundleIterator<FeatureVector, SkipIfInModel> it (bundle, bundleSize, SkipIfInModel());
    for (int i = 0; i<numberOfFeatures; ++i)
    { bundle.push_back(features[i]);
      if (it.valid())
      {	std::cout << "TEST_bundle: *it = " << *it << std::endl;
	++it;
      }
      else
	std::cout << "TEST_bundle: not ready " << it << " external size is " << bundle.size() << std::endl;
    }
    std::cout << "TEST_bundle: completed " << it << std::endl;
  }


  if (false)     // test interaction iterator
  { std::cout << "\n\nTEST:  Test interaction iterator.\n";
    FeatureVector fv;
    for (int i=0; i<4; ++i)      // small vector to check end
      fv.push_back(features[i]);
    InteractionIterator<FeatureVector, SkipIfRelatedPair> it (fv, false, SkipIfRelatedPair());  // use squares?
    std::cout << "TEST_inter: has " << it.number_remaining() << " features remaining\n";
    int more (40);
    while(it.valid() && --more)
    { std::cout << "TEST_inter: *it = " << *it << std::endl;
      ++it;
    }
    std::cout << "TEST_inter: completed " << it << std::endl;
  }


  if (true)    // test dynamic cross-product iterator
  { std::cout << "\n\nTEST:  test cross-product iterator.\n";
    FeatureVector featuresSlow, featuresFast;
    CrossProductIterator it (featuresSlow, featuresFast);
    std::cout << "TEST_cp: initial state of iterator is " << it << std::endl;
    
    featuresSlow.push_back(Feature(columns[0]));  std::cout << "Slow <- " << columns[0]->name() << std::endl;
    featuresFast.push_back(Feature(columns[1]));  std::cout << "Fast <- " << columns[1]->name() << std::endl;
    featuresFast.push_back(Feature(columns[2]));  std::cout << "Fast <- " << columns[2]->name() << std::endl;
    std::cout << "TEST_cp: after adding features " << std::endl;
    
    int more (10);
    while(it.valid() && --more)
    { std::cout << "TEST_cp: *it = " << *it << std::endl;
      ++it;
      std::cout << it << std::endl;
    }

    std::cout << "\n\nTEST: adding features\n";
    featuresFast.push_back(Feature(columns[3]));  std::cout << "Fast <- " << columns[3]->name() << std::endl;
    featuresFast.push_back(Feature(columns[4]));  std::cout << "Fast <- " << columns[4]->name() << std::endl;
    featuresFast.push_back(Feature(columns[5]));  std::cout << "Fast <- " << columns[5]->name() << std::endl;
    
    std::cout << "\nTEST: after adding second group of features, it = " << it << std::endl;
    it.valid();
    std::cout << "\nTEST:                                        it = " << it << std::endl;
    more = 10;
    while(it.valid() && --more)
    { std::cout << "TEST_cp: *it = " << *it << std::endl;
      ++it;
      std::cout << it << std::endl;
    }
    
    std::cout << "\n\nTEST: adding features\n";
    featuresSlow.push_back(Feature(columns[6]));  std::cout << "Slow <- " << columns[6]->name() << std::endl;
    featuresFast.push_back(Feature(columns[8]));  std::cout << "Fast <- " << columns[8]->name() << std::endl;
    featuresSlow.push_back(Feature(columns[7]));  std::cout << "Slow <- " << columns[7]->name() << std::endl;
    
    std::cout << "\nTEST: third pops\n";
    more = 10;
    while(it.valid() && --more)
    { std::cout << "TEST_cp: *it = " << *it << std::endl;
      ++it;
      std::cout << it << std::endl;
    }
    std::cout << "TEST_cp: completed " << it << std::endl;
  }
  
  
  std::cout << "\n\nDONE:\n";
  return 0;
}

