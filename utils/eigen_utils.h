#ifndef _EIGEN_UTILS_H_
#define _EIGEN_UTILS_H_

#include <Eigen/Core>
#include <Eigen/SparseCore>

#include <string>
#include <vector>
#include <map>
#include <list>


namespace EigenUtils {

  using std::string;
  
  template<class Value1, class Value2>
    void
    fill_sparse_matrix (Eigen::SparseMatrix<Value1,Eigen::ColMajor> &B, std::map<std::pair<int,int>,Value2> const& indexValueMap)
  {
    typedef Eigen::Triplet<Value1> T;
    std::vector<T> triplets(indexValueMap.size());
    for(auto it = indexValueMap.cbegin(); it != indexValueMap.cend(); ++it)
      triplets.push_back(T(it->first.first, it->first.second, it->second));
    B.setFromTriplets(triplets.begin(), triplets.end());
  }


  // return number of elements or rows written to file.  Zero signals error.

  inline string
    version ()
    {
      return std::to_string(EIGEN_WORLD_VERSION) + "." + std::to_string(EIGEN_MAJOR_VERSION) + "." + std::to_string(EIGEN_MINOR_VERSION);
    }

  int
    write_matrix_to_file (string fileName, Eigen::MatrixXf const& x, bool append=false);
  
  int
    write_labelled_matrix_to_file (string fileName, std::vector<string> const& rowLabels, Eigen::MatrixXf const& x, bool append=false);
  
  int
    write_vector_to_file (string fileName, Eigen::VectorXf const& x, bool append=false);
}

#endif
