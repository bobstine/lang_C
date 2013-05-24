#include "eigen_utils.h"

#include <iostream>
#include <fstream>
#include <ios>
#include <iomanip>

typedef Eigen::VectorXf Vector;

typedef Eigen::MatrixXf Matrix;


///////////////////////////////////////  Write data to file  /////////////////////////////

int
write_matrix_to_file (std::string fileName, Matrix const& x, bool append)
{
  std::ios_base::openmode mode = (append) ? std::ios_base::app : std::ios_base::trunc;
  std::ofstream output (fileName.c_str(), mode);
  if (! output)
  { std::cerr << "ERROR: Cannot open output text file for writing vector to file " << fileName << std::endl;
    return 0;
  }
  output << x ;
  return x.rows();
}


int
write_labelled_matrix_to_file (std::string fileName, std::vector<std::string> const& rowLabels, Matrix const& x, bool append)
{
  std::ios_base::openmode mode = (append) ? std::ios_base::app : std::ios_base::trunc;
  std::ofstream output (fileName.c_str(), mode);
  if (! output)
  { std::cerr << "ERROR: Cannot open output text file for writing vector to file " << fileName << std::endl;
    return 0;
  }
  for(int i=0; i<x.rows(); ++i)
    output << std::setw(8) << rowLabels[i] << "   " << x.row(i) << std::endl;
  return x.rows();
}

int
write_vector_to_file (std::string fileName, Vector const& x, bool append)
{
  std::ios_base::openmode mode = (append) ? std::ios_base::app : std::ios_base::trunc;
  std::ofstream output (fileName.c_str(), mode);
  if (! output)
  { std::cerr << "ERROR: Cannot open output text file for writing matrix to file " << fileName << std::endl;
    return 0;
  }
  output << x.transpose() << std::endl;
  return x.size();
}

