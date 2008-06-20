// $Id: dataset.cc,v 1.2 2003/02/09 20:03:13 bob Exp $

/*
   9 Jul 01 ... Use the new type of operators
  21 Jun 01 ... Row-major form for mgs version of sweeper.
  15 Oct 98 ... Created in original form for sweeper.
*/

#include <algorithm>
#include <fstream>
#include <vector>

#include "dataset.h"
#include "print_utils.h"

using namespace std;

void
Dataset::fill_with( int nCols, double (*f)(int))
{
  std::vector<double> x(nCols);
  for (int i=0; i<mNRows; ++i) {
    for (int j=0; j<nCols; ++j)
      x[j] = f(j);
    mObservations.push_back(Observation(1.0,x));
  }
}

void
Dataset::set_estimation_weights (const vector<double>& wts)  // How to do this right???
{
  for (unsigned int i=0; i<mObservations.size(); ++i)
    mObservations[i].set_estimation_weight(wts[i]);
}

Dataset::Dataset(const vector<double> y)
{
  mNRows = y.size();
  mSumWts = mNRows;
  for (int i=0; i<mNRows; ++i)
  { Observation obs;
    obs.append_value(y[i]);
    mObservations.push_back(obs);
  }
}

Dataset::Dataset(const char *fileName)
{
  ifstream input(fileName);
  if (input)
    read_from(input);
  else
    { clog << "file " << fileName << " found empty or missing\n"; }
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
      clog << "Dataset: read blank row" << endl;
  }
  mNRows = mObservations.size();
  clog << "Read data set of dimension ("
       << mNRows << "," << nCols() << ")" << endl;
}



void
Dataset::append_values_to_observations (std::vector<double> column)
{
  iterator obs(mObservations.begin());
  for (unsigned int i=0; i<column.size(); ++i, ++obs)
    obs->append_value(column[i]);
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
  std::for_each (data.begin(), data.end(), LinePrinter<Observation>(output));
  return output;
}
