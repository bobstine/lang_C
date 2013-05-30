#ifndef _EIGEN_UTILS_H_
#define _EIGEN_UTILS_H_

#include <Eigen/Core>
#include <Eigen/SparseCore>

#include <string>
#include <vector>
#include <map>
#include <list>


template<class Value1, class Value2>
void
fill_sparse_matrix (Eigen::SparseMatrix<Value1,Eigen::RowMajor> &B, std::map<std::pair<int,int>,Value2> const& indexValueMap)
{
  typedef Eigen::Triplet<Value1> T;
  std::list<T> triplets(indexValueMap.size());
  for(auto it = indexValueMap.cbegin(); it != indexValueMap.cend(); ++it)
    triplets.push_back(T(it->first.first, it->first.second, it->second));
  B.setFromTriplets(triplets.begin(), triplets.end());
}


// return number of elements or rows written to file.  Zero signals error.

int
write_matrix_to_file (std::string fileName, Eigen::MatrixXf const& x, bool append=false);

int
write_labelled_matrix_to_file (std::string fileName, std::vector<std::string> const& rowLabels, Eigen::MatrixXf const& x, bool append=false);

int
write_vector_to_file (std::string fileName, Eigen::VectorXf const& x, bool append=false);

#endif
