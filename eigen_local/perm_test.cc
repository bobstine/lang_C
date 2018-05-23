#include <iostream>
#include <vector>
#include <algorithm>
#include <Eigen/Core>
#include <Eigen/SparseCore>


using std::endl;

typedef float ScalarType;
typedef Eigen::VectorXf Vector;
typedef Eigen::MatrixXf Matrix;
typedef Eigen::SparseMatrix<ScalarType,Eigen::ColMajor> SparseMatrix;

  
int main()
{
  int nrows = 5000;
  int ncols = 30000;
  typedef Eigen::Triplet<ScalarType> T;
  std::vector<T> tripletList;
  for(int i=0; i<nrows; ++i)
  { for (int j = 0; j < ncols/5; ++j)
    { j = rand();
      tripletList.push_back(T(i,j%ncols,j));
    }
  }
  SparseMatrix sm1(nrows,ncols);
  sm1.setFromTriplets(tripletList.begin(), tripletList.end());

  std::vector<int> indx (ncols);
  for(int i=0; i<ncols; ++i) indx[i] = i;
  std::random_shuffle( indx.begin(), indx.end() );
  Eigen::VectorXi indices(ncols);
  for(int i=0; i<ncols; ++i) indices[i] = indx[i];

  Eigen::PermutationMatrix<Eigen::Dynamic,Eigen::Dynamic> perm (indices);
  SparseMatrix sm2;
  sm2 = sm1 * perm;
  std::cout << "*******  Done *******\n";
  
  return 0;
}
  
