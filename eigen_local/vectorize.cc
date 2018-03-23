#include<Eigen/Core>

using namespace Eigen;

void foo(VectorXf const& u, VectorXf const& v, VectorXf& w)
{
  EIGEN_ASM_COMMENT("begin SSE");
  w = u + 3*v;
  EIGEN_ASM_COMMENT("end");
}

int main(int, char**)
{
  int n = 100;
  
  VectorXf x(n);
  VectorXf y(n);
  VectorXf z(n);

  for(int i=0; i<n; ++i)
  { x(i) = i;
    y(i) = 2*i;
  }
  foo(x,y, z);

}
