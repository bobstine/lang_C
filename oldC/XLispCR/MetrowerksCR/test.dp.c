#include <stdlib.h>#include <stdio.h>#include <math.h>#include <time.h>void main (void);float floatDotProd (float *x, float *y, long *n);double doubleDotProd (double *x, double *y, long *n);float floatDotProd (float *x, float *y, long *n){	float dp, temp;	long i;		dp = 0.0;	i = *n;	for(i=0; i<*n; ++i)	{	dp += x[i] * y[i];	}		return(dp);}double doubleDotProd (double *x, double *y, long *n){	double dp;	long i;		dp = 0.0;	i = *n;	while (i--)	{	dp  += *x++ * *y++;	}		return(dp);}#define vecLength  1000#define nReps     50000void main(void){	clock_t start, secs;	float  floatX[vecLength], floatY[vecLength], floatDP;	double dblX[vecLength], dblY[vecLength], dblDP;	int i,j;	long n = vecLength;	double accum;		for(i = 0; i<vecLength; ++i)	{	floatX[i] = (float) i;		floatY[i] = (float) 1.0;		dblX[i] = (double) i;		dblY[i] = (double) 1.0;	}		start = clock(); // time these calculations		for(j=0; j<nReps; ++j)	{	dblDP = doubleDotProd(dblX,dblY,&n);		accum += dblDP;	}            	                 // check the clock	secs = (clock() - start)/ CLOCKS_PER_SEC;	printf ("That took %lu seconds.\n", secs);	/* printf ("Dot product = %lf \n", floatDP,); */	printf ("Double dot product = %lf \n", dblDP); }