// $Id: recommenders.test.cc,v 1.11 2004/04/16 14:07:11 bob Exp $

#include "recommenders.h"
#include "column.h"
#include "feature_factory.h"

#include "range.h"
#include "range_ops.h"

#include <iostream>
#include <vector>
#include <set>


int
main()
{
  // Open the data stream from a file
  FileColumnStream columnStream("../seq_regr/bank_small.dat");
  std::cout << "TEST: n = " << columnStream.n() << std::endl;

  // Push columns from file onto vector
  std::vector< Column > columnVector;
  for (int j=0; j<5; ++j)
  { columnVector.push_back(*columnStream);
    ++columnStream;
  }

  // initialize factory and extract initial column features
  FeatureFactory factory(columnVector);
  std::vector<FeatureABC*> features = factory.features("ColumnFeature");
  
  // Define sequence recommender from these
  std::cout << "TEST: Sequence recommender\n";
  SequenceRecommender<FeatureFactory::FeatureVector> sr("Seq", features);

  sr.chosen_feature(features[0]);
  
  std::cout << "TEST: 1 " << &sr << std::endl;
  std::cout << "TEST: has feature " << sr.has_a_feature() << std::endl;
  std::cout << "      1st feature " << sr.feature() << std::endl;
  sr.update_history(true,0.001);

  std::cout << "TEST: 2 " << &sr << std::endl;
  std::cout << "TEST: has feature " << sr.has_a_feature() << std::endl;
  std::cout << "      2nd feature " << sr.feature() << std::endl;
  sr.update_history(false,0.1);

  std::cout << "TEST: 3 " << &sr << std::endl;

  // now one with interactions
  std::cout << "\nTEST: Interaction recommender\n";
  InteractionRecommender<FeatureFactory::FeatureVector> ir("Test", &factory, features);
  std::cout << "TEST: " << &ir << std::endl;
  std::cout << "      has " << ir.has_a_feature() << std::endl;
  std::cout << "      1st feature " << ir.feature() << std::endl;
  ir.update_history(true,0.001);
  std::cout << "TEST: interaction has " << ir.number_remaining() << " features remaining\n";
  std::cout << "TEST: has " << ir.has_a_feature() << std::endl;
  std::cout << "      2nd feature " << ir.feature() << std::endl;
  ir.update_history(false,0.1);
  std::cout << "      " << &ir << std::endl;

  std::cout << std::endl << features <<  std::endl;

  // permutation
  std::cout << "\nTEST: Permutation recommender\n";
  PermutationRecommender<FeatureFactory::FeatureVector> pr("Test", features);
  std::cout << "      1st feature " << pr.feature() << std::endl;
  pr.update_history(true,0.001);
  std::cout << "      2nd feature " << pr.feature() << std::endl;
  pr.update_history(false,0.1);
  std::cout << "      has " << pr.number_remaining() << " features remaining\n";
  if(pr.has_a_feature())
  { std::cout << "      3rd feature " << pr.feature() << std::endl; 
    pr.update_history(false,0.1);
  }
  if(pr.has_a_feature())
  { std::cout << "      4th feature " << pr.feature() << std::endl;
    pr.update_history(false,0.1);
  }
  std::cout << "      has " << pr.number_remaining() << " features remaining\n";
  std::cout << "      " << &pr << std::endl;

  std::cout << std::endl << features <<  std::endl;
  return 0;
}  
  
