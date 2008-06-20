// $Id: dataset_iterators.test.cc,v 3.0 2004/11/19 18:58:36 foster Exp $

/*
   6 Aug 03 ... Created
*/

#include "dataset_iterators.h"
#include "print_utils.h"

#include <iostream>

// const std::string fileName ("/Users/bob/raslisp/stepwise/C/wind/wind2/6weeks/est.dat");
const std::string fileName ("/Users/bob/C/batch_regr/test/covar.dat");
const bool        transposed (false);

std::vector<int>
make_index (int i)
{
  std::vector<int> vi;
  vi.push_back(i);
  return vi;
}

int
main(void)
{

  // create  dataset
  NumericDataset data(fileName, transposed);

  // linear ordering
  DatasetColumnIterator columnIter (data,1,data.n_cols()-1);
  std::cout << "First column " << *columnIter << std::endl;
  ++columnIter;
  std::cout << "Second column " << *columnIter << std::endl;

  // quadratic ordering
  DatasetInteractionIterator quadraticIter (data,1,data.n_cols()-1);
  std::cout << "First interaction " << *quadraticIter << std::endl;
  ++ quadraticIter;
  std::cout << "Second interaction " << *quadraticIter << std::endl;

  // model-based order
  std::cout << std::endl << std::endl;
  Model regr (data.extract_column(0));
  DatasetModelIterator modelIter(data, regr);
  std::string xName ("column");
  regr.add_predictor(make_tag(xName, make_index(1)), data.extract_centered_column(1));
  modelIter.model_has_changed();
  std::cout << "Model iterator @  " << modelIter.current_index() << std::endl;
  std::cout << "              --> " << *modelIter << std::endl;
  ++modelIter;
  regr.add_predictor(make_tag(xName, make_index(2)), data.extract_centered_column(2));
  modelIter.model_has_changed();
  std::cout << "Model iterator @  " << modelIter.current_index() << std::endl;
  std::cout << "              --> " << *modelIter << std::endl;
  ++modelIter;
  std::cout << "has_a_value() returns " << modelIter.has_a_value() << std::endl;
  std::cout << "Model iterator @ " << modelIter.current_index() << std::endl;
  std::cout << "              --> " << *modelIter << std::endl;

  // go too far
  ++modelIter;
  std::cout << "has_a_value() returns " << modelIter.has_a_value() << std::endl;
  std::cout << "Model iterator @ " << modelIter.current_index() << std::endl;

  return 0;
}
