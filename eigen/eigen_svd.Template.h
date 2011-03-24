#include "debug.h"

// #include <Eigen/QR>
#include <Eigen/Eigenvalues>


//   SVD namespace     SVD namespace     SVD namespace     SVD namespace     SVD namespace     SVD namespace     SVD namespace     

template<class EigenVec>
typename EigenVec::Scalar
SVD::mean (EigenVec const& x)
{
  return x.sum()/x.size();
}


template<class EigenVec>
typename EigenVec::Scalar
SVD::standard_deviation (EigenVec const& x)
{
  typename EigenVec::Scalar m (mean(x));
  return sqrt( (x.array()-m).matrix().squaredNorm()/(x.size()-1) );
}


template<class EigenMatrix>
EigenMatrix
SVD::sample_rows(EigenMatrix const& m, int nRows)
{
  EigenMatrix result (nRows,m.cols());
  int slotsLeft (nRows);
  int rowsLeft  (m.rows());
  while (rowsLeft && slotsLeft)
  {
    if ( ((double)(rand())/RAND_MAX) <  (((double)slotsLeft) / rowsLeft)  )
    { result.row(nRows-slotsLeft) = m.row(rowsLeft-1);
      --slotsLeft;
    }
    --rowsLeft;
  }
  return result;
}


template<class EigenMatrix>
EigenMatrix
SVD::standardize_columns (EigenMatrix const& data, bool useSD)
{
  EigenMatrix result (data.rows(), data.cols());
  // center cols, moving into result
  EigenMatrix mean (data.colwise().sum()/data.rows());   // vector
  for (int j=0; j<data.cols(); ++j)
    result.col(j) = data.col(j).array() - mean(j);
  // scale
  EigenMatrix ss (data.colwise().squaredNorm());         // vector
  for (int j=0; j<data.cols(); ++j)
  { typename EigenMatrix::Scalar s = ss(j);
    if (useSD) s /= data.rows()-1;  // sample var
    result.col(j) = result.col(j) / sqrt(s);
  }
  return result;
}


template<class EigenMatrix>
void
SVD::standardize_columns_in_place (EigenMatrix& data, bool useSD)
{
  // center cols
  EigenMatrix mean (data.colwise().sum()/data.rows());   // vector
  for (int j=0; j<data.cols(); ++j)
    data.col(j) = data.col(j).array() - mean(j);
  // scale
  EigenMatrix ss (data.colwise().squaredNorm());         // vector
  for (int j=0; j<data.cols(); ++j)
  { typename EigenMatrix::Scalar s = ss(j);
    if (useSD) s /= data.rows()-1;  
    data.col(j) = data.col(j) / sqrt(s);
  }
}


template<class EigenMatrix>
EigenMatrix
SVD::random_linear_combinations (EigenMatrix const& data,int k)
{
  EigenMatrix rand (EigenMatrix::Random(data.cols(), k));
  return  data * rand;
}



//   RKHS      RKHS      RKHS      RKHS      RKHS      RKHS      RKHS      RKHS      RKHS      RKHS      RKHS      RKHS      RKHS

template<class K>
Eigen::MatrixXd
RKHS<K>::operator()(Eigen::MatrixXd const& input) const
{
  using debugging::debug;
  
  int n (input.rows());
  // standardize to mean 0, var 1 or kernel makes no sense; use float 
  Eigen::MatrixXf data = input.cast<float>();
  if (mStandardize) SVD::standardize_columns_in_place(data);
  // sample rows to estimate distance matrix
  const int maxRows = 500;
  int dim ((n < maxRows) ? n : maxRows);
  Eigen::MatrixXf subset (SVD::sample_rows(data, dim));
  debug("EIGN",4) << "Subset matrix has " << subset.rows() << " rows & " << subset.cols() << " cols.\n";
  // compute the distance matrix, column at a time, filling lower triangular portion
  // iterate until spectrum removes from diagonal
  //
  //  Tried to use block to pad extra rows on dist for later calculation, as in
  //   soln(dist.block(0,0,dim,dim));
  //  but that did not work with the eigenvalue calculations.
  //
  float scale (1.0);
  Eigen::MatrixXf ev;
  Eigen::VectorXf s;
  { int repeat (5);         // may need to rescale
    while (repeat--)
    { Eigen::MatrixXf dist(dim,dim);
      for (int j=0; j<dim; ++j)
	for (int i=j; i<dim; ++i)
	  dist(i,j) = dist(j,i) = K(scale)(subset.row(i),subset.row(j));
      // find the eigenvector decomposition
      Eigen::SelfAdjointEigenSolver<Eigen::MatrixXf> soln(dist);
      s  = soln.eigenvalues();      // weirdly sorted in ascending order
      debug("EIGN",3) << "Last 6 eigenvalues of RKHS distance matrix are {" << s.segment(dim-6,6).transpose() << "}; computing full dist matrix." << std::endl;
      // find range of e-values
      float min(s.minCoeff());
      float max(s.maxCoeff());
      float range (max-min);
      debug("EIGN",3) << "E-value range in RKHS is " << max << " - " << min << " = " << range << " gives ratio " << range/((max+min)/2) << std::endl;
      if (range/((max+min)/2) < 1.5)
	scale = scale * 2;
      else
      { ev = soln.eigenvectors();
	repeat = 0;
      }
    }
  }
  // augment distance matrix, filling in all elements
  Eigen::MatrixXf aDist(n,dim);
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
  nPC = nPC > 1 ? nPC : 1;  // at least 1
  // build the principal components in feature space
  //
  //  avoid use of block functions and build explicitly
  //
  Eigen::MatrixXf evectors (ev.rows(),nPC);
  for(int j=0; j<nPC; ++j) // rearrange order of eigenvectors??? why do this rather than make a block???
    evectors.col(j) = ev.col(ev.cols()-1-j);
  // return double
  Eigen::MatrixXd pc ((aDist * evectors).cast<double>());
  debug("EIGN",4) << "RKHS basis with dim " << pc.rows() << "x" << pc.cols() << "; SD of RKHS basis elements are ";
  for(int j=0; j<nPC; ++j)
    debug() << SVD::standard_deviation(pc.col(j)) << " ";
  debug() << std::endl;
  return pc;
}
