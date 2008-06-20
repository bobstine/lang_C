// $Id: datasets.h,v 1.8 2004/08/30 18:20:14 bob Exp $

/*
  30 May 03 ... Created as feeder to the sequential regression code.
*/

#ifndef _DATASETS_H_
#define _DATASETS_H_

#include <iostream>
#include <string>
#include <vector>
#include <functional>

#include "range.h"
#include "range_traits.h"
#include "range_ops.h"
#include <stddef.h>

/*
  The dataset policy requires that the object be able to respond to
  the messages to return a 'row', compute the averages, and finally to
  reveal its dimensions.

  NumericDatasets store the data as a collection of rows, with each
  row a pointer to doubles.  Row major order is to obtain some sense
  of locality of reference.

  In default form, the data file should be set up with leading
  dimensions and then a variable at a time

              n p
	      x_1 x_2 x_3 ... x_n
	      y_1 y_2 y_3 ... y_n
	      z_1 z_2 z_3 ... z_n
	      ...

  In the alternative transposed form (as used in sweeper's est.dat
  files), the data is read raw from a file, figuring out the number of
  rows and the number of columns itself.  It assumes that the leading
  column holds sampling weights --- these are skipped.

              w_1   x_1 y_1 z_1 ...
	      w_2   x_2 y_2 z_2 ...
	      w_3   x_3 y_3 z_3 ...
	      ...
	      
*/

class NumericDataset {
  int mNCols;
  int mNRows;
  double*  mAvg;
  double** mData;
  double*  mExtra;
  
 public:

  typedef range_traits< range<double*> >::range DataRange;
  typedef binary_range_traits< std::minus<double>, range<double*>, range<double*>  >::range CenteredDataRange;
  
  NumericDataset (std::string const& fileName, bool transposed = false);
  NumericDataset (std::istream& input) { read_from(input); }
  ~NumericDataset () { free(); }
  
  int n_rows()     const { return mNRows; }
  int n_cols()     const { return mNCols; }

  std::pair<int,int>
    dimensions() const { return std::make_pair(n_rows(), n_cols()); }

  double
    average (int j) const { return mAvg[j]; }
  std::vector<double>
    average_vector () const { std::vector<double> avg (mAvg, mAvg+mNRows); return avg; }
  DataRange
    averages () const { return make_range(mAvg, mAvg+mNCols); }
  
  DataRange
    row (int i) const { return make_range(mData[i], mData[i]+mNCols); }

  DataRange
    linear_combination (std::vector<double> const& beta);  // range lives in extra column
  DataRange
    linear_combination (std::vector<double> const& beta, std::vector<int> const& indices, double b0);

  void
    insert_column (std::vector<double> const& z, int column);

  std::vector<double> 
    extract_column (int column) const;
  std::vector<double> 
    extract_centered_column (int column) const;

  std::vector<double> 
    extract_split_column(int column, int split) const;

  std::vector<double> 
    extract_interaction (int i, int j) const;
  std::vector<double> 
    extract_centered_interaction (int i, int j) const;
  
  CenteredDataRange
    centered_row (int i) const { return make_binary_range( std::minus<double>(),
							   row(i),
							   averages()
							   ); }
    
 private:

  void
    allocate ();
  void
    free ();
  void
    read_from (std::istream& input);
  void
    read_transposed_from (FILE *file);

};

#endif
