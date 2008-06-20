// $Id: datasets.cc,v 1.5 2003/08/23 21:56:58 bob Exp $

/*
  30 May 03 ... Created
*/

#include "datasets.h"

#include <fstream>

#include "file_utils.h"
#include "stat_utils.h"

NumericDataset::NumericDataset(std::string const& fileName, bool transposed)
{
  std::clog << "DATA: Reading from file " << fileName << std::endl;
  if (transposed)
  {
    mNRows = File_Utils::count_lines(fileName);
    mNCols = File_Utils::count_fields(fileName)-1;   // skip weights
    FILE* input (fopen(fileName.c_str(),"r"));
    if (mNRows > 0 && mNCols > 0)
    { allocate();
      read_transposed_from(input);
    }
    else
      std::clog << "DATA: File " << fileName << " found empty or missing (n="
		<< mNRows << ", p=" << mNCols << ")"
		<< std::endl;
  }
  else
  {
    std::ifstream input(fileName.c_str());
    if (input)
    {
      input >> mNRows >> mNCols;
      allocate();
      read_from(input);
    }
    else
      std::clog << "DATA: File " << fileName << " found empty or missing\n";
  }
  std::cout << "DATA: Read completed; First two averages are "
	    << mAvg[0] << "  " << mAvg[1]
	    << std::endl;
}

void
NumericDataset::read_from (std::istream &input)
{
  for (int j=0; j<mNCols; ++j)      // data is a list of cols which get transposed
    for (int row=0; row<mNRows; ++row)
    { input >> mData[row][j];
      mAvg[j] += mData[row][j];
    }
  for(int j=0; j<mNCols; ++j)
    mAvg[j] = mAvg[j] / (double) mNRows;
}

void
NumericDataset::read_transposed_from (FILE *input)
{
  double weight;
  for (int row=0; row<mNRows; ++row)
  { fscanf(input, "%lf", &weight);              // skip over leading weight
    for (int j=0; j<mNCols; ++j)
    { fscanf(input, "%lf", &mData[row][j]);
      mAvg[j] += mData[row][j];
    }
  }
  for(int j=0; j<mNCols; ++j)
    mAvg[j] = mAvg[j] / (double) mNRows;
}

void
NumericDataset::allocate ()
{
  std::clog << "DATA: Allocating dim [" << mNRows << " , " << mNCols << "] ... ";
  mExtra = new double[mNRows];
  mAvg = new double[mNCols];
  for (int j=0; j<mNCols; ++j)
    mAvg[j] = 0.0;
  mData = new double*[mNRows];
  for (int i=0; i<mNRows; ++i)
    mData[i] = new double[mNCols];
  std::clog << "done." << std::endl;
}

void
NumericDataset::free ()
{
  std::clog << "DATA: Freeing dim [" << mNRows << " , " << mNCols << "]" << std::endl;
  for (int i=0; i<mNRows; ++i)
    delete(mData[i]);
  delete(mData);
  delete(mAvg);
  delete(mExtra);
}

NumericDataset::DataRange
NumericDataset::linear_combination (std::vector<double> const& beta)
{
  if ((int)beta.size() == mNCols)
  {
    for (int i=0; i<mNRows; ++i)
    {
      double *xi (mData[i]);
      double sum (0.0);
      for (int j=0; j<mNCols; ++j)
      {
	sum += beta[j]*xi[j];
      }
      mExtra[i] = sum;
    }
  } else
    std::clog << "DATA: dimension of input beta for lin comb " << beta.size()
	      << " does not match number of data set columns " << mNCols << std::endl;
  return make_range(mExtra,mExtra+mNRows);
}

NumericDataset::DataRange
NumericDataset::linear_combination (std::vector<double> const& beta, std::vector<int> const& indices, double b0)
{
  for (int i=0; i<mNRows; ++i)
  {
    double *xi (mData[i]);
    double sum (b0);
    for (unsigned int j=0; j<indices.size(); ++j)
    {
      sum += beta[j]*xi[indices[j]];
    }
    mExtra[i] = sum;
  }
  return make_range(mExtra,mExtra+mNRows);
}

void
NumericDataset::insert_column (std::vector<double> const& z, int column)
{
  if ((int)z.size() == mNRows)
    if ((column >= 0) && (column < mNCols))
      {
	double avg (0.0);
	for (int i=0; i<mNRows; ++i)
	  {
	    mData[i][column] = z[i];
	    avg += z[i];
	  }
	mAvg[column] = avg/mNRows;
      }
    else
      std::clog << "DATA: *** Error *** Invalid column " << column << " not in range 0-" << mNCols-1 << std::endl;
  else
    std::clog << "DATA: *** Error  *** Input number of rows " << z.size() 
	      << " does not match number of data set rows " << mNRows << std::endl;
}


std::vector<double>
NumericDataset::extract_column (int column) const
{
  std::vector<double> z(mNRows);
  if ((column >=0) && (column < mNCols))
  {
    for (int i=0; i<mNRows; ++i)
      z[i] = mData[i][column];
  }
  else
    std::clog << "DATA: requested column " << column
	      << " is not valid. "  << std::endl;
  return z;
}


std::vector<double> 
NumericDataset::extract_centered_column (int column) const
{
  std::vector<double> z (extract_column(column));
  for (int i=0; i<mNRows; ++i)
    z[i] -= mAvg[column];
  return z;
}

std::vector<double> 
NumericDataset::extract_split_column(int column, int split) const
{
  std::vector<double> z (extract_column(column));
  if (split == 1)  // dogleg above mean
  {
    double avg (mAvg[column]);
    for (int i=0; i<mNRows; ++i)
      z[i] = (z[i] > avg) ? z[i]-avg : 0.0 ;
  }
  else if (split == 2) // partially sigmoid
  {
    double avg (mAvg[column]);
    double sd (standard_deviation(z, avg));
    double high (avg + sd/0.67);
    double low  (avg - sd/0.67);
    for (int i=0; i<mNRows; ++i)
      z[i] = (z[i] < low || z[i] > high) ? z[i]-avg : 0.0 ;
  }
  else std::cout << "DATA: *** ERROR *** Run out of splits in dataset." << std::endl;
  return z;
}
  

std::vector<double> 
NumericDataset::extract_interaction (int i, int j) const
{
  std::vector<double> z (extract_column(i));
  for (int row=0; row<mNRows; ++row)
    z[row] *= mData[row][j];
  return z;
}

std::vector<double> 
NumericDataset::extract_centered_interaction (int i, int j) const
{
  std::vector<double> z (extract_centered_column(i));
  for (int row=0; row<mNRows; ++row)
    z[row] *= mData[row][j]-mAvg[j];
  return z;
}
