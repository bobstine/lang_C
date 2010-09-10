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

#include <iostream>


int
main()
{
  // build vector of columns from file
  const std::string columnFileName ("/Users/bob/C/gsl_tools/data/bank_post45.dat");
  std::vector<Column> columns;
  insert_columns_from_file(columnFileName, back_inserter(columns));
  std::cout << "TEST: Data file " << columnFileName << " produced vector of " << columns.size() << " columns.\n";
  

  // test Finite streams
  {
    FeatureVector features;
    
    std::cout << "\n\nTEST: adding features\n";
    features.push_back(Feature(columns[0]));  
    features.push_back(Feature(columns[1]));  
    features.push_back(Feature(columns[2]));  

    typedef RegulatedStream< FiniteStream > FS;
    FS fs (make_finite_stream("Fin St", features));

    std::cout << "TEST: FS has_feature = " << fs.has_feature(features, features) << std::endl;

    std::vector<Feature> fv (fs.pop());
    std::cout << "TEST:    Popped feature " << fv[0] << std::endl;

    std::cout << "TEST: FS has_feature = " << fs.has_feature(features, features) << std::endl;
    fs.print_features_to(std::cout); std::cout << std::endl;
    
    int more (11);
    while(fs.has_feature(features,features) && more--)
    { std::vector<Feature> fv (fs.pop());
      std::cout << "TEST:    Popped feature " << fv[0] << std::endl;
      fs.print_features_to(std::cout); std::cout << std::endl;
    }

    std::cout << "\n\nTEST:  Setting model results for fv[0] to true with p-value 0.001\n";
    fv[0]->set_model_results(true, 0.001);  // used in model, p-value
    more = 11;
    while(fs.has_feature(features,features) && more--)
    { std::vector<Feature> fv (fs.pop());
      std::cout << "TEST:    Popped feature " << fv[0] << std::endl;
      fs.print_features_to(std::cout); std::cout << std::endl;
    }

    fs.print_features_to(std::cout); std::cout << std::endl;
  }
  /*  
  std::cout << "\n\nTEST:  Moving on to test other feature streams, now cross-product stream.\n";
  // test dynamic cross-product stream
  typedef  RegulatedStream< CrossProductStream< std::vector<Feature>,std::vector<Feature> > > CP;
  std::vector<Feature> featuresSlow, featuresFast;
  CP cp (make_cross_product_stream("CP stream", featuresSlow, featuresFast));

  std::cout << "TEST: has_feature = " << cp.has_feature(featuresSlow, featuresFast) << std::endl;
  cp.print_to(std::cout); std::cout << std::endl;

  std::cout << "\n\nTEST: adding features\n";
  featuresSlow.push_back(Feature(columns[0]));  std::cout << "Slow <- " << columns[0]->name() << std::endl;
  featuresFast.push_back(Feature(columns[1]));  std::cout << "Fast <- " << columns[1]->name() << std::endl;
  featuresFast.push_back(Feature(columns[2]));  std::cout << "Fast <- " << columns[2]->name() << std::endl;
  
  std::cout << "TEST: has_feature = " << cp.has_feature(featuresSlow, featuresFast) << std::endl;
  cp.print_to(std::cout); std::cout << std::endl;

  std::cout << "TEST: first pops\n";
  while(!cp.empty())
  { std::vector<Feature> fv (cp.pop());
    std::cout << "Popped feature " << fv[0] << std::endl;
  }
  std::cout << "TEST: has_feature = " << cp.has_feature(featuresSlow, featuresFast) << std::endl;
  cp.print_to(std::cout); std::cout << std::endl;
  
  std::cout << "\n\nTEST: adding features\n";
  featuresFast.push_back(Feature(columns[3]));  std::cout << "Fast <- " << columns[3]->name() << std::endl;
  featuresFast.push_back(Feature(columns[4]));  std::cout << "Fast <- " << columns[4]->name() << std::endl;
  featuresFast.push_back(Feature(columns[5]));  std::cout << "Fast <- " << columns[5]->name() << std::endl;

  std::cout << "TEST: has_feature = " << cp.has_feature(featuresSlow, featuresFast) << std::endl;
  cp.print_to(std::cout); std::cout << std::endl;

  std::cout << "\nTEST: second pops\n";
  while(!cp.empty())
  { std::vector<Feature> fv (cp.pop());
    std::cout << "Popped feature " << fv[0] << std::endl;
  }

  std::cout << "\n\nTEST: adding features\n";
  featuresSlow.push_back(Feature(columns[6]));  std::cout << "Slow <- " << columns[6]->name() << std::endl;
  featuresSlow.push_back(Feature(columns[7]));  std::cout << "Slow <- " << columns[7]->name() << std::endl;
  featuresFast.push_back(Feature(columns[8]));  std::cout << "Fast <- " << columns[8]->name() << std::endl;

  std::cout << "TEST: has_feature = " << cp.has_feature(featuresSlow, featuresFast) << std::endl;
  cp.print_to(std::cout); std::cout << std::endl;

  std::cout << "\nTEST: third pops\n";
  while(!cp.empty())
  { std::vector<Feature> fv (cp.pop());
    std::cout << "Popped feature " << fv[0] << std::endl;
  }
  */
  std::cout << "\n\nDONE:\n";
  return 0;
}

