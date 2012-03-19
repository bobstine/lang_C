#include "line_search.Template.h"
#include "normal.h"
#include "bellman.h"

#include <functional>
#include <iostream>
#include <math.h>

#include <ctime>
#include <Eigen/Core>
 

int  main()
{
  double gamma        ( 2.0 );
  double omega        ( 0.05);
  int    nSteps       ( 5 );
  bool   writeDetails ( true);

  if (false)
  {
    std::cout << "\n\nTEST: Bellman vector recursion.\n\n";
    RejectVectorUtility utility (gamma, omega);
    solve_bellman_utility (gamma, omega, nSteps, utility, universal, writeDetails);
  }


  if (true)
  {
    std::cout << "\n\nTEST: Bellman matrix recursion.\n\n";
    RejectMatrixUtility utility(gamma, omega);
    solve_bellman_utility (gamma, omega, nSteps, utility, universal, universal, writeDetails);
  }


  return 0;
}


