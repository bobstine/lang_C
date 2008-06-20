// $Id: threshold.test.cc,v 1.3 2000/04/25 15:35:55 bob Exp $-*- c++ -*-

#include <iostream.h>
#include <vector.h>

#include "threshold.h"

#include "utils.h"



///////////////////////////////////////////////////////////////////////

			 
main()
{

  double e[8] = {0.5, 0.3, -8.5, 1.5, 0.2, 10.2, 6.3, -3.3 };
  vector<double>  est (e, e+8);
  cout << "Input coefficients " << est << "=======================" << endl;

  {
    HardThreshold hard(1.0);
    vector<double> postEst = hard(est);
    cout << "After hard thresholding " << postEst;
  }
   {
     SoftThreshold soft(1.0);
     vector<double> postEst = soft(est);
     cout << "After soft thresholding " << postEst;
   }
   {
     LoopThreshold loop(1.0);
     vector<double> postEst = loop(est);
     cout << "After loop thresholding " << postEst;
   }
    {
      // not properly implemented
      AdaptiveThreshold adapt(1.0);
      vector<double> postEst = adapt(est);
      cout << "After adaptive thresholding " << postEst;
    }
}


/////////////////////  EOF  /////////////////////////


