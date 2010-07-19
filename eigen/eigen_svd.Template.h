#include "debug.h"

#include <Eigen/QR>

template<class K>
Eigen::MatrixXd
rkhs<K>::operator()(Eigen::MatrixXd const& input) const
{
  using debugging::debug;
  
  int n (input.rows());
  int p (input.cols());
  // standardize to mean 0, var 1 or kernel makes no sense
  Eigen::MatrixXd data(n,p);
  if (mStandardize)
    data = SVD::standardize_columns(input);
  else
    data = input;

  // sample rows to estimate eigen directions
  const int maxRows = 400;
  int dim ((n < maxRows) ? n : maxRows);
  Eigen::MatrixXd subset (SVD::sample_rows(data, dim));
  debug("EIGN",4) << "Subset matrix has " << subset.rows() << " rows & " << subset.cols() << " cols.\n";
  // compute the distance matrix, column at a time, filling lower triangular portion
  // iterate until spectrum removes from diagonal
  //
  //  Tried to use block to pad extra rows on dist for later calculation, as in
  //   soln(dist.block(0,0,dim,dim));
  //  but that did not work with the eigenvalue calculations.
  //
  double scale (1.0);
  Eigen::MatrixXd ev;
  Eigen::VectorXd s;
  { int repeat (5);
    while (repeat--)
    { Eigen::MatrixXd dist(dim,dim);
      for (int j=0; j<dim; ++j)
	for (int i=j; i<dim; ++i)
	  dist(i,j) = dist(j,i) = K(scale)(subset.row(i),subset.row(j));
      // find the eigenvector decomposition
      Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> soln(dist);
      s  = soln.eigenvalues();      // weirdly sorted in ascending order
      debug("EIGN",3) << "Last 6 eigenvalues of distance matrix are {" << s.segment(dim-6,6).transpose() << "}; computing full dist matrix.\n";
      // find range of e-values
      double min(s.minCoeff());
      double max(s.maxCoeff());
      double range (max-min);
      debug("EIGN",3) << "E-value range is " << max << " - " << min << " = " << range << " gives ratio " << range/((max+min)/2) << std::endl;
      if (range/((max+min)/2) < 1.5)
	scale = scale * 2;
      else
      { ev = soln.eigenvectors();
	repeat = 0;
      }
    }
  }
  //  build augmented distance matrix, filling in all elements
  Eigen::MatrixXd aDist(n,dim);
  for (int i=0; i<n; ++i)
    for (int j=0; j<dim; ++j)
      aDist(i,j) = K(scale)(data.row(i), subset.row(j));
  // count e-values larger than 1
  int nPC (mNumComponents);
  if(nPC == 0) 
    for(int i=0; i<(int)s.size(); ++i)
      if (s(i) > 1.0)
	++nPC;
      else break;
  nPC = nPC > 2 ? nPC : 3;  // at least 3
  // build the principal components in feature space
  //
  //  avoid use of block functions and build explicitly
  //
  Eigen::MatrixXd evectors (ev.rows(),nPC);
  for(int j=0; j<nPC; ++j)
    evectors.col(j) = ev.col(ev.cols()-1-j);
  Eigen::MatrixXd pc;
  pc = aDist * evectors;
  debug("EIGN",4) << "Basis for rkhs with dim " << pc.rows() << "x" << pc.cols()
			     << "; SD of RKHS basis elements are ";
  for(int j=0; j<nPC; ++j)
    debug("EIGN",4) << SVD::standard_deviation(pc.col(j)) << " ";
  std::cout << std::endl;
  return pc;
}
