#include <iostream>
#include <vector>

#include <Eigen/Array>
#include <Eigen/SVD>


int main(int argc, char *argv[])
{
  std::cout.precision(2);

  typedef Eigen::VectorXd Vector;
  typedef Eigen::MatrixXd Matrix;
  
  // ras... element by element insertion
  {
    const int dim (10);
    std::vector<float> sv;
    for(int i = 0; i<dim; ++i)
      sv.push_back(-i);

    // insert columns into matrix
    Vector vec (dim);
    for(int i = 0; i<dim; ++i)
      vec[i] = sv[i];

    std::cout << "TEST: vec[1] = " << vec(1) << "   abs val = " << abs(vec(1)) << std::endl;


    Matrix mat (dim,3);
    mat.col(0) = vec;
    mat.col(1) = vec; // odd that this does not work ... vec + 3... need vec.cwise() + scalar
    mat.col(2) = vec * 3;

    std::cout << "TEST: mat[1,1] = " << mat(1,1) << "   abs val = " << abs(mat(1,1)) << std::endl;

    std::cout << "\nElement by element assignment:\n" << vec << "\nmat\n" <<  mat << std::endl;

    
    // dot products
    std::cout << "v'v = " << vec.squaredNorm() <<  "  v'M = " << vec.transpose() * mat << std::endl;
    // generates compile error
    // std::cout << " not conformable " <<     mat * mat << std::endl;

    // try centering and scaling the matrix
    Vector mean (mat.colwise().sum());
    mean = mean / mat.rows();
    for (int j=0; j<mat.cols(); ++j)
      mat.col(j) = mat.col(j).cwise() - mean(j);
    std::cout << "Matrix after centering \n" << mat << std::endl;
    Vector ss (mat.colwise().squaredNorm());
    ss = ss / (mat.rows() - 1);
    for (int j=0; j<mat.cols(); ++j)
    { double sd (sqrt(ss(j)));
      mat.col(j) = mat.col(j) / sd;
    }
    std::cout << "Matrix after scaling \n" << mat << std::endl;
    std::cout << "Sqrt(SS) -- not div by n -- of first column is " << mat.col(0).norm() << std::endl;
    
    // SVD operations
    Eigen::SVD<Matrix> svd(mat);
    Matrix u (svd.matrixU());
    Matrix v (svd.matrixV());

    std::cout << "Matrix U is \n" << u << std::endl;
    std::cout << "Singular values \n" << svd.singularValues().transpose() << std::endl;
    std::cout << " u'u = \n" << u.transpose() * u << std::endl;
    std::cout << " u'Mv = \n" << u.transpose() * mat * v << std::endl;
  }
  
  // demo static functions
  Eigen::Matrix3f m3 = Eigen::Matrix3f::Random();
  Eigen::Matrix4f m4 = Eigen::Matrix4f::Identity();

  std::cout << "\n*** Step 1 ***\nm3:\n" << m3 << "\nm4:\n" << m4 << std::endl;

  // demo non-static set... functions
  m4.setZero();
  m3.diagonal().setOnes();
  
  std::cout << "\n*** Step 2 ***\nm3:\n" << m3 << "\nm4:\n" << m4 << std::endl;

  // demo fixed-size block() expression as lvalue and as rvalue
  m4.block<3,3>(0,1) = m3;
  m3.row(2) = m4.block<1,3>(2,0);

  std::cout << "\n*** Step 3 ***\nm3:\n" << m3 << "\nm4:\n" << m4 << std::endl;

  // demo dynamic-size block()
  {
    int rows = 3, cols = 3;
    m4.block(0,1,3,3).setIdentity();
    std::cout << "\n*** Step 4 ***\nm4:\n" << m4 << std::endl;
  }

  // demo vector blocks
  // this did not compile... m4.diagonal().block(1,2).setOnes();
  m4.diagonal().setOnes();
  std::cout << "\n*** Step 5 ***\nm4.diagonal():\n" << m4.diagonal() << std::endl;
  std::cout << "m4.diagonal().start(3)\n" << m4.diagonal().start(3) << std::endl;

  
  // demo coeff-wise operations
  m4 = m4.cwise()*m4;
  m3 = m3.cwise().cos();
  std::cout << "*** Step 6 ***\nm3:\n" << m3 << "\nm4:\n" << m4 << std::endl;

  // sums of coefficients   RAS: accummulate an arbirary function along an axis???
  std::cout << "*** Step 7 ***\n m4.sum(): " << m4.sum() << std::endl;
  std::cout << "m4.col(2).sum(): " << m4.col(2).sum() << std::endl;
  std::cout << "m4.colwise().sum():\n" << m4.colwise().sum() << std::endl;
  std::cout << "m4.rowwise().sum():\n" << m4.rowwise().sum() << std::endl;

  // demo intelligent auto-evaluation
  /*
    m4 = m4 * m4;                             // auto-evaluates so no aliasing problem (performance penalty is low)
  Eigen::Matrix4f other = (m4 * m4).lazy(); // forces lazy evaluation
  m4 = m4 + m4;                             // here Eigen goes for lazy evaluation, as with most expressions
  m4 = -m4 + m4 + 5 * m4;                   // same here, Eigen chooses lazy evaluation for all that.
  m4 = m4 * (m4 + m4);                      // here Eigen chooses to first evaluate m4 + m4 into a temporary.
                                            // indeed, here it is an optimization to cache this intermediate result.
  m3 = m3 * m4.block<3,3>(1,1);             // here Eigen chooses NOT to evaluate block() into a temporary
                                            // because accessing coefficients of that block expression is not more costly than accessing
                                            // coefficients of a plain matrix.
  m4 = m4 * m4.transpose();                 // same here, lazy evaluation of the transpose.
  m4 = m4 * m4.transpose().eval();          // forces immediate evaluation of the transpose

  std::cout << "*** Step 8 ***\nm3:\n" << m3 << "\nm4:\n" << m4 << std::endl;
  */
}
