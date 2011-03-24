#include "adapter.h"

#include "features.h"


#include <iostream>
// #include <gsl/gsl_matrix.h>
#include <Eigen/Dense>



class gslMult: std::unary_function<gsl_matrix const*, gsl_matrix *>
{
public:
  gsl_matrix* operator()(gsl_matrix const* m)
    {
      gsl_matrix *z (gsl_matrix_alloc(m->size1, 1));
      for(int i=0; i<(int)m->size1; ++i)
	gsl_matrix_set(z,i,0, 100 * gsl_matrix_get(m,i,0));
      return z;
    }
};


class eigenMult: std::unary_function<Eigen::MatrixXd const&, Eigen::MatrixXd>
{
public:
  Eigen::MatrixXd operator()(Eigen::MatrixXd const& m)
  {
    Eigen::MatrixXd result = m * 1000;
    return result;
  }
};



int main()
{
  int n (30);
  std::cout << "\n\nTEST: Adapter with GSL matrix.\n";

  std::vector<double> x;
  for (int i = 0; i < n; ++i)
  { x.push_back(i);
  }

  Column c1 ("Column 1", "role x description", n, x.begin());
  Column c2 ("Column 2", "role x description", n, x.begin());
  Column c3 ("Column 3", "role x description", n, x.begin());

  std::vector<Feature> fv;
  fv.push_back(c1);
  fv.push_back(c2);
  fv.push_back(c3);

  int skip (10);
  std::vector<Feature> gslresult = GSL_adapter<gslMult>(gslMult(),skip)(fv); 
  std::cout << "TEST: Num columns in result = " << gslresult.size() << std::endl;
  std::cout << "TEST: First col of result: " << gslresult[0] << std::endl;

  std::vector<Feature> eigenresult = Eigen_adapter<eigenMult>(eigenMult(),skip)(fv); 
  std::cout << "TEST: Num columns in result = " << eigenresult.size() << std::endl;
  std::cout << "TEST: First col of result: " << eigenresult[0] << std::endl;

  return 0;
}
