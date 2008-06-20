//  Wavelet code for QMF

#include <stdlib.h>
#include <stdio.h>

#include <time.h>

#include "wavelet.h"

//----------------------------------------------------------------------

void main (void);
void Wait(void);
void PrintVector (double *x, long n, char *str);

void Wait()
{
	char ch;
	printf ("---- Waiting for any key ----\n");
	scanf ("%c", &ch);
}


void PrintVector (double *x, long n, char *str)
{
	long i;
	
	printf ("---------------\nVector %s:\n", str);
	for (i=0; i<n; ++i)
		printf("x[%ld] = %lf \n", i, x[i]);
	Wait();
}

//----------------------------------------------------------------------
	
#define defN 16

void main(void)
{
	clock_t start, secs;							// Timing variables
	
	double x[defN], w[defN], c[6], thresh=5.0;
	long   i, n=defN , k=6, skip=2, meth=0;
	
	for(i = 0; i<defN; ++i)
	{	x[i] = (double) i;
	}
	
	c[0] =  0.33267055295;							// D6
	c[1] =  0.8068915093;
	c[2] =  0.4598775021;
	c[3] = -0.13501102;
	c[4] = -0.08544127388;
	c[5] =  0.0352262918857;

	
	PrintVector(x, n, "Input");
	start = clock(); 								// ___ Time these calculations below
	
	WaveletDecomp(x, &n, c, &k, w);
	ThresholdWavelet (w, &n, &skip, &thresh, &meth);
	WaveletRecon (w, &n, c, &k, x);
	
	secs = (clock() - start)/ CLOCKS_PER_SEC;		// --- End of timing range
	PrintVector(x, n, "Reconstructed");
	
	printf ("That took %lu seconds.\n", secs);		// Print timing info
}
