#include <stdio.h>
#include <stdlib.h>

#include "coding.h"

int main(void)
{
  double sumGeo=0.0, sumCauchy=0.0;
  const double geoProb  = 0.6;
  const double geoShare = 0.5;
  
  printf("   k  geo(k)   cauchy(k)   ell(k)   f-tresh(k) \n");
  for (long k=1; k<7; ++k)
    printf("[%3ld] %8.3f %8.3f %8.3f %8.3f \n",
	   k,
	   geometricPDF(geoProb, k),
	   cauchyPDF(k),
	   codeIndexLength(geoShare, geoProb, k),
	   fStatisticThreshold(geoShare, geoProb, k));
  printf("[%3ld] %8.3f %8.3f %8.3f %8.3f \n",
	 (long) 50,
	 geometricPDF(geoProb, 50),
	 cauchyPDF(50),
	 codeIndexLength(geoShare, geoProb, 50),
	 fStatisticThreshold(geoShare, geoProb, 50));
  printf("[%3ld] %8.3f %8.3f %8.3f %8.3f \n",
	 (long) 250,
	 geometricPDF(geoProb, 250),
	 cauchyPDF(250),
	 codeIndexLength(geoShare, geoProb, 250),
	 fStatisticThreshold(geoShare, geoProb, 250));
  printf("[%3ld] %8.3f %8.3f %8.3f %8.3f \n",
	 (long) 999,
	 geometricPDF(geoProb, 999),
	 cauchyPDF(999),
	 codeIndexLength(geoShare, geoProb, 999),
	 fStatisticThreshold(geoShare, geoProb, 999));
  for(long i=1; i<500; ++i)
  { sumGeo += geometricPDF(geoProb, i);
    sumCauchy += cauchyPDF(i);
  }
  printf("[inf] %8.3f %8.3f\n", sumGeo, sumCauchy);
}
	 
	   
