#ifndef _EIGEN_UTILS_H_
#define _EIGEN_UTILS_H_

#include <Eigen/Core>
#include <string>
#include <vector>


// Returns the number of elements or rows written to file.  Zero signals error.

int
write_matrix_to_file (std::string fileName, Eigen::MatrixXf const& x, bool append=false);

int
write_labelled_matrix_to_file (std::string fileName, std::vector<std::string> const& rowLabels, Eigen::MatrixXf const& x, bool append=false);

int
write_vector_to_file (std::string fileName, Eigen::VectorXf const& x, bool append=false);

#endif
