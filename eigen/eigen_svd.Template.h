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
  // sample data has nRows 
  const int maxRows = 400;
  int nRows ((n < maxRows) ? n : maxRows);
  Eigen::MatrixXd subset (SVD::sample_rows(data, nRows));
  // compute the distance matrix, column at a time, filling lower triangular portion  
  Eigen::MatrixXd dist(n,nRows);          // extra rows used below
  for (int j=0; j<nRows; ++j)
    for (int i=j; i<nRows; ++i)
      dist(i,j) = dist(j,i) = K()(subset.row(i),subset.row(j));
  // find the eigenvector decomposition, with evals sorted in diminishing order
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> soln(dist);
  Eigen::MatrixXd ev (soln.eigenvectors());
  Eigen::VectorXd s  (soln.eigenvalues());
  std::cout << "EIGEN: Singular values of distance matrix are {" << s.transpose() << "}\n";
  // expand the distance matrix, filling in all elements
  for (int i=0; i<n; ++i)
    for (int j=0; j<nRows; ++j)
      dist(i,j) = K()(data.row(i), subset.row(j));
  // count e-values larger than 1
  int nPC (mNumComponents);
  if(nPC == 0) 
    for(int i=0; i<(int)s.size(); ++i)
      if (s(i) > 1.0)
	++nPC;
      else break;
  nPC = nPC > 2 ? nPC : 3;  // at least 3
  // build the principal components in feature space
  Eigen::MatrixXd pc(n, nPC);
  pc = dist * ev.block(0,0, n,nPC);
  std::cout << "EIGEN: SD of RKHS basis elements are ";
  for(int j=0; j<nPC; ++j)
    std::cout << SVD::standard_deviation(pc.col(j)) << " ";
  std::cout << std::endl;
  return pc;
}
