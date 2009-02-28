// $Id: mands.cc,v 1.4 2007/11/08 21:58:49 bob Exp $

/*
  This filter reads a sequence of records from standard input.  Each
  record should consist of the same number of columns of numerical
  data.  The result of the filter is sequence of records, one for each
  column.  The output begins with n and p (# input cols), then a
  summary line for each of the p column

                 n p
                 mean, sd, se, min, max
                 mean, sd, se, min, max
                 ...
		 
  Any comment lines (those beginning with a * in column 1) are copied
  to the output.  Comments are only allowed at the head of the file.
 
  Stop the reading with any non-numeric character.
*/

#include "stat_utils.h"
#include <math.h>

#include <iostream>
#include <sstream>

int
main()
{
  std::string firstLine;

  int counter (0);
  while (true)
  { getline(std::cin, firstLine);
    if ('*' != firstLine[0])
      break;
    std::cout << firstLine << std::endl; // copy comments to output
    ++counter;
  }
  std::clog << "Skipped " << counter << " comment lines." << std::endl;
  
  std::istringstream input(firstLine);
  std::clog << "Echo first line \n" << firstLine << "\n";
  std::vector<double> obs;
  double x;
  while (input>>x)
    obs.push_back(x);
  int p (obs.size());
  std::clog << "Expecting " << p << " items per row.\n" << std::endl;
  std::vector< std::vector<double> > data;
  std::vector<double> xBar (obs);
  std::vector<double> sd  (p, 0.0);
  std::vector<double> min (p, 2000000);
  std::vector<double> max (p,-2000000);
  data.push_back( obs );
  while (std::cin>>x)
  { std::vector<double> nextObs (p);
    nextObs[0] = x;
    xBar[0] += x;
    for (int j=1; j<p; ++j)
    { std::cin >> nextObs[j];
      xBar[j] += nextObs[j];
    }
    data.push_back( nextObs );
  }
  int n (data.size());
  for (int j=0; j<p; ++j)
    xBar[j] = xBar[j]/n;
  for (int i=0; i<n; ++i)
  {
    for (int j=0; j<p; ++j)
    { double x (data[i][j]);
      if (x < min[j])	min[j] = x;
      else if (x > max[j]) max[j] = x;
      double dev (x - xBar[j]);
      sd[j] += dev * dev;
    }
  }
  std::cout << n << " " << p << std::endl;
  double rn (sqrt((double)n));
  for (int j=0; j<p; ++j)
  { sd[j] = sqrt(sd[j]/(n-1));
    std::cout << xBar[j] << " " << sd[j] << " " << sd[j]/rn << " "
	      << min[j] << " " << max[j] << std::endl;
  }
}
      
  
