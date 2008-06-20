/* 				Wavelet code

	13 Aug 95 ... First version reasonably stable.
	
*/

#include "wavelet.h"


//-------  Utility routines  ------------------------------------------


#define min(a,b) (((a) < (b)) ? (a) : (b))

void SetupHiCoefs (double *loCoefs, long *nCoefs);

//-----------------------------------------------------------------------


static double cHi[40];


void SetupHiCoefs (double *loCoefs, long *nCoefs)
{
	int i;
	for (i=0; i<*nCoefs; ++i)
	{	if (i%2 == 0)
			cHi[i] =  loCoefs[i];
		else
			cHi[i] = -loCoefs[i];
	}
}

//-----------------------------------------------------------------------

void ThresholdWavelet (double *wave, long *pn, long *pSkip, double *pLevel, long *pMethod)
{
	long i, skip;
	double  hi, lo;
	
	skip = 0;
	for (i=0; i< *pSkip; ++i)
		skip *= 2;
	
	hi = *pLevel;
	lo = -hi;
	
	if (*pMethod == 0)					// hard
		for (i = skip; i < *pn; ++i)
		{	if (*wave < hi && *wave > lo)
				*wave = 0.0;
			++wave;
		}
	else								// soft
		for (i = skip; i < *pn; ++i)
		{	if (*wave < hi && *wave > lo)
				*wave = 0.0;
			else if (*wave > 0.0) *wave -= hi;
				 else			  *wave += hi;
			++wave;
		}
}


void WaveletDecomp (double *x, long *pn, double *cLo, long *pk, double *wave) 
{
	long i,j, nOver2, n, nCoefs, offset;
	double dpLo, dpHi, *pLo, *pHi;
	
	
	// Build high coef vector
	SetupHiCoefs(cLo, pk);
	
	// Dereference input pointers
	nCoefs = *pk;
	
	// Setup length constants.  n for input length
	n = 2 * *pn;
	nOver2 = n;
	
	// Dyadic recursions start here
	while (nOver2 > 1)
	{	offset = 4 * n;
		n = n/2;
		nOver2 = n/2;
		pLo = wave;							// pLo is pointer for low terms
		pHi = &wave[nOver2];				// pHi is pointer to hi terms
		j = 1;								// j is "data" index
		while (j < min(nCoefs,n) )			// Slower loop
		{	dpLo = dpHi = 0.0;
			for (i=0; i<nCoefs; ++i)
			{ 	dpLo += cLo[i] * x[(offset + j - i) % n];
				dpHi += cHi[i] * x[(j - 1 + i) % n];
			}
			*pLo++ = dpLo;
			*pHi++ = dpHi;
			j += 2;
		}
		while (j < n)						// Faster loop
		{	dpLo = dpHi = 0.0;
			for (i=0; i<nCoefs; ++i)
			{ 	dpLo += cLo[i] * x[ j     - i];
				dpHi += cHi[i] * x[(j - 1 + i) % n];
			}
			*pLo++ = dpLo;
			*pHi++ = dpHi;
			j += 2;
		}
		// Move the low frequency coefficients into x for next pass
		for (i=0; i < nOver2; ++i)
			x[i] = wave[i];
	}
}




void WaveletRecon (double *wave, long *pn, double *cLo, long *pk, double *x)
{
	long i, j, k, wi, nCoefs, n, nOver2, half;
	double odd, even, dp1, dp0, c4[4], *px;

	// Build high coef vector
	SetupHiCoefs(cLo, pk);
	
	// Dereference input pointers
	nCoefs = *pk;
	half = nCoefs/2;
		
	// Do first step manually to avoid some modular arithmetic
	odd = 0.0; even=0.0;
	for (i=0; i<nCoefs; i+=2)					// sum odd and even coefs
	{	even += cLo[i];
		odd  += cLo[i+1];
	}
	x[0] = wave[0] * odd  + wave[1] * even;		// low, then high
	x[1] = wave[0] * even - wave[1] * odd;
	wave[0] = x[0];								// move to wave vector
	wave[1] = x[1];
	
	// Do second step manually as well if nCoefs permits
	if (nCoefs > 2)
	{	for (i=0; i<4; ++i)
			c4[i] = 0.0;
		i = 0;
		while (i < nCoefs)						// calc folded coef sums
		{	c4[0] += cLo[i++];
			c4[1] += cLo[i++];
			if (i < nCoefs)
			{	c4[2] += cLo[i++];
				c4[3] += cLo[i++];
			}
		}
		x[0]  = wave[0] * c4[1] + wave[1] * c4[3];	// low
		x[1]  = wave[0] * c4[0] + wave[1] * c4[2];
		x[2]  = wave[0] * c4[3] + wave[1] * c4[1];
		x[3]  = wave[0] * c4[2] + wave[1] * c4[0];
		x[0] += wave[2] * c4[0] + wave[3] * c4[2];	// high
		x[1] -= wave[2] * c4[1] + wave[3] * c4[3];	 
		x[2] += wave[2] * c4[2] + wave[3] * c4[0];	 
		x[3] -= wave[2] * c4[3] + wave[3] * c4[1];	 
	}
	for (i=0; i<4; ++i)
		wave[i] = x[i];
	
	// Setup length constants.
	half = *pk / 2;
	nOver2 = 4;								// current length of reconstruction
	n = 8;									// length of data to be reconstructed

	// Dyadic recursions start here
	while (nOver2 < *pn)
	{	px = x;								// points into reconstructed series
		for(j=0; j<nOver2; ++j)				// Fill in the low frequency terms
		{	dp1 = cLo[0] * wave[j];			// Do the first two
			dp0 = cLo[1] * wave[j];
			k = 2;
			wi = j+1;
			while (k < nCoefs)
			{	if (wi == nOver2) wi = 0;
				dp1 += cLo[k++] * wave[wi];
				dp0 += cLo[k++] * wave[wi];
				++wi;
			}
			*px++ = dp0;
			*px++ = dp1;
		}
		px = x;
		for(j=0; j<nOver2; ++j)				// Add the high frequency terms
		{	wi = nOver2 + j;
			dp0 = cHi[0] * wave[wi];
			dp1 = cHi[1] * wave[wi];
			k = 2;
			while (k < nCoefs)
			{	-- wi;
				if (wi < nOver2) wi = n-1;	// nOver2 is "zero"
				dp0 += cHi[k++] * wave[wi];
				dp1 += cHi[k++] * wave[wi];
			}
			*px++ += dp0;
			*px++ += dp1;
		}
		for (i = 0; i<n; ++i)				// move reconstruction back
			wave[i] = x[i];
		nOver2 *= 2;
		n *= 2;
	}
}

