/*  $Id: controller.cc,v 1.1 2003/02/09 20:03:13 bob Exp $

  Driver for the regression program, linking it to a dynamic source.
  
 
*/
#include <iostream>
#include <vector>

#include "model.h"
#include "print_utils.h"

bool
invalid_input (char ch)
{
  char inChar;
  std::cin >> inChar;
  if (inChar != ch)
  { std::clog << "Terminating: invalid input symbol "
	      << inChar << " != " << ch << std::endl;
    return true;
  }
  return false;
}

int main()
{
  // first read sample size from the standard input
  if (invalid_input('n')) return 1;
  int sampleSize (0);
  std::cin >> sampleSize;

  // alloc space for holding temporary values
  std::vector<double> dataVec(sampleSize, 0.0);
  
  // read a column of values for the response, init dataset
  if (invalid_input ('y')) return 1;
  for (int i=0; i<sampleSize; ++i)
    std::cin >> dataVec[i];
  Dataset data (dataVec);

  // init the regression model
  int j (0);
  Variable Y = make_index_variable(j);
  std::clog << "Y " << Y << " == " << dataVec << std::endl;
  Model regr(Y, data, "");
  
  // read loop for series of X's
  std::pair <double,double> xFstatPval;
  while (not(invalid_input('x')))
  {
    ++j;
    double sum(0.0);
    for (int i=0; i<sampleSize; ++i)
    {
      std::cin >> dataVec[i];
      sum += dataVec[i];
    }
    double avg = sum / sampleSize;
    data.append_values_to_observations(dataVec);
    Variable X = make_index_variable(j);
    std::clog << "X " << X << " == " << dataVec << std::endl;
    bool add(false);
    xFstatPval = regr.gaussian_predictor_evaluation (X, avg);
    if (xFstatPval.second < 0.05/j)
    {
      std::pair<double,double> xTestPval;
      xTestPval = regr.bennett_predictor_evaluation (X, avg);
      if (xTestPval.second < 0.05/j)
      {
	add = true;
	std::clog << "Adding predictor based on evaluation:  lower bound "
		  << xTestPval.first << "   p = " << xTestPval.second << std::endl;
	regr.add_predictor(X, avg);
      }
    }
    if (add)
      std::cout << std::endl << 1 << std::endl;
    else
      std::cout << std::endl << 0 << std::endl;
    std::cout.flush();
  }
  std::cout.flush();
  return 0;
}
