// $Id: dataset.cc,v 1.1 2005/06/13 20:47:51 bob Exp $

/*
   9 Jul 01 ... Use the new type of operators
  21 Jun 01 ... Row-major form for mgs version of sweeper.
  15 Oct 98 ... Created in original form for sweeper.
*/

#include <algorithm>
#include <fstream>
#include <vector>

#include "dataset.h"
#include "utility.h"

void
Dataset::set_estimation_weights (const vector<double>& wts)  // How to do this right???
{
  for (unsigned int i=0; i<mObservations.size(); ++i)
    mObservations[i].set_estimation_weight(wts[i]);
}

void
Dataset::fill_with_iid(RandomGenerator& rand)
{
  vector<double> x(mNCols);
  for (int i=0; i<mNRows; ++i) {
    for (int j=0; j<mNCols; ++j)
      x[j] = rand.normal();
    mObservations.push_back(Observation(1.0,x));
  }
}

Dataset::Dataset(const char *fileName)
{
  ifstream input(fileName);
  if (input)
    read_from(input);
  else
    { cout << "file " << fileName << " found empty or missing\n"; }
}

void
Dataset::read_from (istream &input)
{
  mSumWts = 0.0;
  while (not(input.eof())) {
    Observation temp;
    input >> temp;
    if (temp.size()>0){
      mObservations.push_back(temp);
      mSumWts += temp.sample_weight();
    }
    else
      cout << "Dataset: read blank row" << endl;
  }
  mNRows = mObservations.size();
  mNCols = mObservations[0].size();
  cout << "Read data set of dimension ("
       << mNRows << "," << mNCols << ")" << endl;
}

istream&
operator>>(istream& input, Dataset& data)
{
  data.read_from(input); return input;
}

ostream&
operator<<(ostream& output, const Dataset& data)
{
  output << "Dataset [" << data.nRows() << "," << data.nCols() << "]" << endl;
  for_each (data.begin(), data.end(), LinePrinter<Observation>(output));
  return output;
}
