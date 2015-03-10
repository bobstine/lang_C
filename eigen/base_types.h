#ifndef _BASE_TYPES_H

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
