#ifndef _EIGEN_BASE_TYPES_H
#define _EIGEN_BASE_TYPES_H

#include <Eigen/Core>

#define use_small_scalar

#ifdef use_small_scalar
#define SCALAR  float
#define VECTOR  Eigen::VectorXf
#define MATRIX  Eigen::MatrixXf
#else
#define SCALAR  double
#define VECTOR  Eigen::VectorXd
#define MATRIX  Eigen::MatrixXd
#endif

#endif
