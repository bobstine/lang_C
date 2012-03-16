#ifndef _EIGEN_UTILS_H_
#define _EIGEN_UTILS_H_

#include <Eigen/Core>


typedef Eigen::VectorXf Vector;

typedef Eigen::MatrixXf Matrix;



// Returns the number of elements or rows written to file.  Zero signals error.

int
write_matrix_to_file (std::string fileName, Matrix const& x, bool append=false);


int
write_vector_to_file (std::string fileName, Vector const& x, bool append=false);



#endif
