// $Id: recommenders.test.cc,v 3.5 2008/01/18 03:59:19 bob Exp $

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
  FileColumnStream columnStream("/Users/bob/C/seq_regr/data/bank_small.dat");
  std::cout << "TEST: n = " << columnStream.n() << std::endl;

  // Push columns from file onto vector
  std::vector< Column > columnVector;
  for (int j=0; j<5; ++j)
  { columnVector.push_back(*columnStream);
    ++columnStream;
  }

  // initialize factory and extract vector of column features
  FeatureFactory factory(columnVector);
  std::vector<FeatureABC*> features = factory.features("ColumnFeature");
  
  
  // Define sequence recommender from these
  std::cout << "TEST: Sequence recommender\n";
  SequenceRecommender<FeatureFactory::FeatureVector> sr("Seq", features);

  sr.chosen_feature(features[0]);
  
  std::cout << "TEST: 1 " << &sr << std::endl;
  std::cout << "TEST: has feature " << sr.has_a_feature() << std::endl;
  std::cout << "      1st feature " << sr.feature() << std::endl;
  sr.update_history(features[0],true);

  std::cout << "TEST: 2 " << &sr << std::endl;
  std::cout << "TEST: has feature " << sr.has_a_feature() << std::endl;
  std::cout << "      2nd feature " << sr.feature() << std::endl;
  sr.update_history(features[0], false);

  std::cout << "TEST: 3 " << &sr << std::endl;

  // now one with interactions
  std::cout << "\nTEST: Interaction recommender\n";
  InteractionRecommender<FeatureFactory::FeatureVector> ir("Test", &factory, features);
  std::cout << "TEST: " << &ir << std::endl;
  std::cout << "      has " << ir.has_a_feature() << std::endl;
  std::cout << "      1st feature " << ir.feature() << std::endl;
  ir.update_history(ir.feature(), true);
  std::cout << "TEST: interaction has " << ir.number_remaining() << " features remaining\n";
  std::cout << "TEST: has " << ir.has_a_feature() << std::endl;
  std::cout << "      2nd feature " << ir.feature() << std::endl;
  ir.update_history(ir.feature(), false);
  std::cout << "      " << &ir << std::endl;

  std::cout << std::endl << features <<  std::endl;

  // Define Two-part shrinkage recommender
  /*
	 std::cout << "TEST: Two-part shrinkage recommender\n";
  FitTrashRecommender<FeatureFactory::FeatureVector,
                      FeatureFactory::FeatureVector,
                      Factory> trash("trash (two-part shrinkage)", features, features, &factory);

  trash.chosen_feature(features[0]);
  
  std::cout << "TEST: 1 " << &trash << std::endl;
  std::cout << "TEST: has feature " << trash.has_a_feature() << std::endl;
  std::cout << "      1st feature " << trash.feature() << std::endl;
  trash.update_history(true,0.001);

  std::cout << "TEST: 2 " << &trash << std::endl;
  std::cout << "TEST: has feature " << trash.has_a_feature() << std::endl;
  std::cout << "      2nd feature " << trash.feature() << std::endl;
  trash.update_history(false,0.1);

  std::cout << "TEST: 3 " << &trash << std::endl;
 */
	
  // permutation
  std::cout << "\nTEST: Permutation recommender\n";
  PermutationRecommender<FeatureFactory::FeatureVector> pr("Test", features);
  std::cout << "      1st feature " << pr.feature() << std::endl;
  pr.update_history(pr.feature(), true);
  std::cout << "      2nd feature " << pr.feature() << std::endl;
  pr.update_history(pr.feature(), false);
  std::cout << "      has " << pr.number_remaining() << " features remaining\n";
  if(pr.has_a_feature())
  { std::cout << "      3rd feature " << pr.feature() << std::endl; 
    pr.update_history(pr.feature(), false);
  }
  if(pr.has_a_feature())
  { std::cout << "      4th feature " << pr.feature() << std::endl;
    pr.update_history(pr.feature(), false);
  }
  std::cout << "      has " << pr.number_remaining() << " features remaining\n";
  std::cout << "      " << &pr << std::endl;

  std::cout << std::endl << features <<  std::endl;
  return 0;
}  
  
