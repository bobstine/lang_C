// $Id: dataset.test.cc,v 1.4 2003/11/26 16:49:18 bob Exp $

#include <iostream>
#include <string>
#include <vector>

#include "dataset.h"
#include "random.h"
#include "range.h"
#include "range_ops.h"

RandomGenerator mRand;

double genVal(int column)
{ return column + mRand.normal(); }

class Indexer : public std::unary_function<const Observation&, double>
{
  int mCol;
public:
  Indexer(int col)
    : mCol(col) { }
  double
  operator()(const Observation& obs)
  { return obs.element(mCol); }
};

int main (void)
{
  std::cout << "Data read from file" << std::endl;
  Dataset dataFromFile("test/dataset");
  std::cout << dataFromFile;
  std::vector<double> x;
  for (int i=0; i<dataFromFile.nRows(); ++i) x.push_back(12.2+i);
  dataFromFile.append_values_to_observations(x);
  std::cout << "   with added column " << std::endl << dataFromFile;
  
  std::cout << "Data generated by random engine" << std::endl;
  Dataset dataFromGenerator(10,5,genVal);
  std::cout << dataFromGenerator;

  std::cout << std::endl << "Extract dataset[2] using indexer" << std::endl;
  std::cout <<  make_unary_range(Indexer(2), dataFromGenerator);
  
  return 0;
}
    
