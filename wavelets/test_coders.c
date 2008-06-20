/* 			


		Test threshold coders based on data compression


*/


#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "coders.h"

void main(void);

void main(void)
{
	/*  Test integer codes
	{	int i;
		for (i=-8; i<9; ++i)
		printf("%2d requires %3d bits \n", i, SignedCauchyBitLength(i));
	}
	*/
	
#define N 32
	{	double z[N] =  {5.5, 5.0, 4.5, 4.0, 3.8, 3.6, 3.4, 3.2, 3.0, 2.9, 2.8,
						2.7, 2.6, 2.5, 2.4, 2.3, 2.2, 2.1, 2.0, 1.9, 1.85, 
						1.8, 1.7, 1.6, 1.5, 1.4, 1.3, 1.2, 1.1, 1.0, 0.5, 0.0},
				aicBits[N], bicBits[N], ricBits[N], ebicBits[N],
				totalAicBits, totalBicBits, totalRicBits, totalEbicBits;
		long   aicCodes[N], bicCodes[N], ricCodes[N], ebicCodes[N],
				zLen = N, i,
				sortIndex[32],
				returnBits = 1;
		double M = 32.0;
		
		for (i=0; i<N; ++i)
		{ 	sortIndex[i] = i;
			aicBits[i]  = z[i];		// make "local" copies since overwritten
			bicBits[i]  = z[i];
			ricBits[i]  = z[i];
			ebicBits[i] = z[i];
		}
		 
		AIC ( aicBits, &zLen,  aicCodes, &returnBits, &totalAicBits);
		BIC ( bicBits, &zLen,  bicCodes, &returnBits, &totalBicBits,  &M);
		RIC ( ricBits, &zLen,  ricCodes, &returnBits, &totalRicBits);
		EBIC(ebicBits, &zLen, ebicCodes, &returnBits, &totalEbicBits, sortIndex);
		
		printf("    z               Code/Bits\n");
		printf("            AIC          BIC            RIC            EBIC  \n");		
		for (i=0; i<N; ++i)
			printf(" %7.2f   (%3d   %5.2f)  (%3d   %5.2f)  (%3d   %5.2f)  (%3d   %5.2f)\n", z[i], 
					aicCodes[i], aicBits[i],   bicCodes[i], bicBits[i],
					ricCodes[i], ricBits[i],  ebicCodes[i],ebicBits[i]);
		printf(" Total = %9.2f      %9.2f      %9.2f      %9.2f\n", totalAicBits, totalBicBits, totalRicBits,totalEbicBits);
	}	
}



