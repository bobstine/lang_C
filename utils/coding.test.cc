//  $Id: coding.test.cc,v 1.3 2008/01/18 03:59:39 bob Exp $

#include <stdio.h>
#include <stdlib.h>
#include <iostream>

#include "coding.h"

int main(void)
{

  {
    int j;
    CauchyCoder cc;
    
    j = 1; std::cout << "Code length for j=" << j << " is " << cc(j) << std::endl;
    j = 2; std::cout << "Code length for j=" << j << " is " << cc(j) << std::endl;
    j = 3; std::cout << "Code length for j=" << j << " is " << cc(j) << std::endl;
    j = 4; std::cout << "Code length for j=" << j << " is " << cc(j) << std::endl;
    j =10; std::cout << "Code length for j=" << j << " is " << cc(j) << std::endl;

    double RSS;
    int n;
    n = 100; RSS = 0.0;
    std::cout << "n=" << n << " RSS=" << RSS << " --> " << gaussian_data_length(n,RSS)
	      << " bits"  << std::endl;
    n = 100; RSS = 100.0;
    std::cout << "n=" << n << " RSS=" << RSS << " --> " << gaussian_data_length(n,RSS)
	      << " bits"  << std::endl;

    
    int q (1); double dRSS(4.35365);
    std::cout << "q=" << q << " dRSS=" << dRSS << " --> " << change_in_code_length(n,RSS,q,dRSS)
	      << " bits" << std::endl;
    dRSS = 7.544;
    std::cout << "q=" << q << " dRSS=" << dRSS << " --> " << change_in_code_length(n,RSS,q,dRSS)
	      << " bits" << std::endl;

    
    SignedCauchyCoder scc;
    RunLengthCoder<CauchyCoder,SignedCauchyCoder> rlc (make_run_length_coder(cc,scc));
    std::vector< std::pair<int,double> > j_z;
    j_z.push_back (std::make_pair(1, 5.5));
    j_z.push_back (std::make_pair(6, 2.5));
    j_z.push_back (std::make_pair(8,15.5));
    std::cout << "Run length code for " ;
    for (unsigned int i=0; i<j_z.size(); ++i)
      std::cout << "(" << j_z[i].first << "," << j_z[i].second << ") ";
    std::cout << " is " << rlc(j_z) << " bits " << std::endl;
  }
  
  const double geoProb  = 0.6;
  const double geoShare = 0.5;
  
  printf("   k  univ(k)  slow(k)  geo(k)   cauchy(k)   ell(k)   f-tresh(k) \n");
  for (long k=1; k<11; ++k)
    printf("[%3ld] %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f \n",
           k,
           universalPDF(k),
	   slowUniversalPDF(k),
           geometricPDF(geoProb, k),
           cauchyPDF(k),
           codeIndexLength(geoShare, geoProb, k),
           fStatisticThreshold(geoShare, geoProb, k));
  printf("[%3ld] %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f \n",
	 (long) 50,
         universalPDF(50),
	 slowUniversalPDF(50),
         geometricPDF(geoProb, 50),
         cauchyPDF(50),
         codeIndexLength(geoShare, geoProb, 50),
         fStatisticThreshold(geoShare, geoProb, 50));
  printf("[%3ld] %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f \n",
         (long) 250,
         universalPDF(250),
	 slowUniversalPDF(50),
         geometricPDF(geoProb, 250),
         cauchyPDF(250),
         codeIndexLength(geoShare, geoProb, 250),
         fStatisticThreshold(geoShare, geoProb, 250));
  printf("[%3ld] %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f \n",
         (long) 999,
         universalPDF(999),
	 slowUniversalPDF(50),
         geometricPDF(geoProb, 999),
         cauchyPDF(999),
         codeIndexLength(geoShare, geoProb, 999),
         fStatisticThreshold(geoShare, geoProb, 999));
  double sumGeo=0.0, sumCauchy=0.0, sumSlow = 0.0, sumUniv=0.0;
  for(long i=1; i<25000; ++i)
  { sumGeo += geometricPDF(geoProb, i);
    sumUniv += universalPDF(i);
    sumSlow += slowUniversalPDF(i);
    sumCauchy += cauchyPDF(i);
  }
  printf("[inf] %9.6f %9.6f %9.6f %9.6f\n", sumUniv, sumSlow, sumGeo, sumCauchy);
}
	 
	   
