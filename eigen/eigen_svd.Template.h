#include "debug.h"

#include <Eigen/QR>

template<class K>
Eigen::MatrixXd
rkhs<K>::operator()(Eigen::MatrixXd const& input) const
{
  int n (input.rows());
  int p (input.cols());
  // standardize by shrinking toward mean
  Eigen::MatrixXd data(n,p);
  if (mStandardize)
    data = SVD::standardize_columns(input);
  else
    data = input;
  const int maxRows = 400;
  int dim ((n < maxRows) ? n : maxRows);
  Eigen::MatrixXd subset (SVD::sample_rows(data, dim));
  // compute the distance matrix, column at a time, filling lower triangular portion
  //
  //  Tried to use block to pad extra rows on dist for later calculation, as in
  //   soln(dist.block(0,0,dim,dim));
  //  but that did not work with the eigenvalue calculations.
  //
  debugging::debug("EIGN",4) << "Subset matrix has " << subset.rows() << " rows & " << subset.cols() << " cols.\n";
  Eigen::MatrixXd dist(dim,dim);
  for (int j=0; j<dim; ++j)
    for (int i=j; i<dim; ++i)
      dist(i,j) = dist(j,i) = K()(subset.row(i),subset.row(j));
  // find the eigenvector decomposition
  //
  //  weirdness... eigen values are in reversed order
  //
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> soln(dist);
  Eigen::MatrixXd ev (soln.eigenvectors());
  Eigen::VectorXd s  (soln.eigenvalues());
  debugging::debug("EIGN",2) << "Last 6 eigenvalues of distance matrix are {" << s.segment(dim-6,6).transpose() << "}; computing full dist matrix.\n";
  //  build augmented distance matrix, filling in all elements
  Eigen::MatrixXd aDist(n,dim);
  for (int i=0; i<n; ++i)
    for (int j=0; j<dim; ++j)
      aDist(i,j) = K()(data.row(i), subset.row(j));
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
  debugging::debug("EIGN",4) << "Basis for rkhs with dim " << pc.rows() << "x" << pc.cols()
			     << "; SD of RKHS basis elements are ";
  for(int j=0; j<nPC; ++j)
    std::cout << SVD::standard_deviation(pc.col(j)) << " ";
  std::cout << std::endl;
  return pc;
}
