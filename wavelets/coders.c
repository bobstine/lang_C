/* 			Threshold coders based on data compression

	 7 Jan 97 ... Initiated.
	15 Jan 97 ... Agrees with lisp.
	17 Jan 97 ... Remove the sorted assumption.
	
	
	Remaining Issues...  


*/


#include <stdlib.h>
#include <math.h>
#ifdef DEBUG
#include <stdio.h>
#endif

#include "coders.h"


//-------  Globals  ------------------------

#define ONEOVER2Log2  0.721347521

//-------  Macros  --------------------------

#define ABS(x)		(((x) < 0) ? (-x) : x)
#define MIN(x,y)	(((x) < (y)) ? (x) : (y))
#define MIN3(x,y,z) MIN((x),MIN((y),(z)))
#define SQR(x)		((x)*(x))
#define LOG2(x)		((log(x))/0.693147)

//-------  internal prototypes  -------------

int IntegerBitLength (int z);
int CauchyBitLength (int z);
int SignedCauchyBitLength (int z);

double DataBits (double z);
int    iRound(double z);
double dRound(double z);

int AICCode (double z, double *codeBits);

int BICCode (double z, long n, double M, double *codeBits);

int RICCode (double z, int indexBits, double *codeBits);

int EBICCode (double z, double prob, double *codeBits);

//-------------------------------------------

int IntegerBitLength(int z)
{
	int j=1, az;
	
	az = ABS(z);
	while (az > 1)
	{	az /= 2;
		++j;
	}
	return(j);
}

int CauchyBitLength (int z)
{
	if (z == 0)
		return(1);
	else
		return(2 * IntegerBitLength(z));
}

int SignedCauchyBitLength (int z)
{
	if (z == 0)
		return(1);
	else
		return(1 + 2 * IntegerBitLength(z));
}



int iRound(double x)
{
	double  flr;
	int ix;
	
	flr = floor(x);
	ix = (int) flr;
	if (x - flr > 0.5)
		ix += 1;
	return(ix);
}
	
double dRound(double x)
{
	double  flr;
	
	flr = floor(x);
	if (x - flr > 0.5)
		flr += 1.0;
	return(flr);
}
	
	
	
double DataBits(double z)
{
	return ( ONEOVER2Log2 * SQR(z) );
}

//////////////////////////////////////////////////////////////////
//     															//
//                    AIC coder									//
//     															//
//////////////////////////////////////////////////////////////////

void AIC  (double *zList, long *n, long *codes, long *returnBits, double *totalBits)
{
	long i;
	double bits;
	
	*totalBits = 0.0;
	for (i=0; i<*n; ++i)
	{	codes[i] = AICCode(zList[i], &bits);
		*totalBits += bits;
		if (*returnBits)
			zList[i] = bits;
		// printf("Coding %f as %ld using %f bits\n", data[i], codes[i], bits[i]);
	}
}

int AICCode (double z, double *minBits)
{
	int    iz;
	
	iz = iRound(z);
	if (iz == 0)
	{	*minBits = 1.0 + DataBits(z);
		return(0);
	}
	else
	{	double az, qBits[3];
		int i, minCode, code, offset[3] = {0, 1, 2};
		
		iz = ABS(iz);
		az = ABS(z);
		*minBits = 1111111.7;
		for (i=0; i<3; ++i)
		{	code = iz - offset[i];
			qBits[i] = SignedCauchyBitLength(code) 
						 + DataBits(((double)code) - az);
			// printf("%5.2f -> %3d -> ( %3d , %6.2f ) \n", z, iz, code, qBits[i]);
			if (qBits[i] < *minBits)
			{	minCode = code;
				*minBits = qBits[i];
			}
		}
		return((z > 0) ? minCode : -minCode);
	}
}



//////////////////////////////////////////////////////////////////
//     															//
//                    BIC coder									//
//     															//
//////////////////////////////////////////////////////////////////


void BIC  (double *zList, long *n, long *codes, long *returnBits, double *totalBits, double *M)
{
	long i;
	double bits;
	
	*totalBits = 0.0;
	for (i=0; i<*n; ++i)
	{	codes[i] = BICCode(zList[i], *n, *M, &bits);
		*totalBits += bits;
		if (*returnBits)
			zList[i] = bits;
		// printf("Coding %f as %ld using %f bits\n", data[i], codes[i], bits[i]);
	}
}

int BICCode (double z, long n, double M, double *codeBits)
{
	double    rz, codeBitsZero;
	
	rz = dRound(z);
	codeBitsZero = DataBits(z);
	
	if (rz == 0.0)
	{	*codeBits = 1.0 + codeBitsZero;
		return(0);
	}
	else
	{	double codeBitsNon;
		
		codeBitsNon = floor(1.0 + 0.5 * LOG2((n-1) * M * M))  +  DataBits(rz-z);
		if (codeBitsNon < codeBitsZero)
		{	*codeBits = codeBitsNon + 1.0;    // add one for hypothesis bit
			return( (int)rz );
		}
		else
		{	*codeBits = codeBitsZero + 1.0;
			return( 0 );
		}
	}
}



//////////////////////////////////////////////////////////////////
//     															//
//                    RIC coder									//
//     															//
//////////////////////////////////////////////////////////////////


void RIC  (double *zList, long *n, long *codes, long *returnBits, double *totalBits)
{
	long i;
	int indexBits = 1 + (int) floor(LOG2(*n-1));
	double bits;
	
	*totalBits = 0.0;
	for (i=0; i<*n; ++i)
	{	codes[i] = RICCode(zList[i], indexBits, &bits);
		*totalBits += bits;
		if (*returnBits) zList[i] = bits;
	}
}

int RICCode (double z, int indexBits, double *codeBits)
{
	double    rz, codeBitsZero;
	
	rz = dRound(z);
	codeBitsZero = DataBits(z);
	
	if (rz == 0.0)
	{	*codeBits = codeBitsZero;    			// Zero bits for the parameter
		return(0);
	}
	else
	{	double aicBits, codeBitsNon;
		int aicCode;
												
		aicCode = AICCode (z, &aicBits);			//  aicBits = parm + databits
		codeBitsNon = aicBits + 1.0 + indexBits;	// Add continuation bit and index
		if (codeBitsNon < codeBitsZero)
		{	*codeBits = codeBitsNon;
			return( aicCode );
		}
		else
		{	*codeBits = codeBitsZero;
			return( 0 );
		}
	}
}


//////////////////////////////////////////////////////////////////
//     															//
//                   EBIC coder									//
//     															//
//////////////////////////////////////////////////////////////////


void EBIC (double *zList, long *n, long *codes, long *returnBits, double *totalBits, long *order)
{
	long i, j, nZero, nNotZero;
	double len = *n, codeBits, prop;
	
	*totalBits = 0.0;
	for (i=0, nZero=0; i<*n; ++i)
	{	j = order[i];
		codes[j] = EBICCode(zList[j], ((double)(1+i))/len , &codeBits);
		if (codes[j] != 0) ++nZero;
		*totalBits += codeBits;
		if (*returnBits) zList[j] = codeBits;
	}
	prop = ((double) nZero)/(double) *n;
	// Now add entropy bits for indicators a_i, first for the a_i=1
	if (nZero)
	{	codeBits = - LOG2( prop );
		*totalBits += nZero * codeBits;
		if (*returnBits)
			for (i=0; i<nZero; ++i)
				zList[ order[i] ] += codeBits;
	}
	// Now add pealty for those coded as zero
	nNotZero = *n - nZero;
	if (nNotZero)
	{	codeBits = - LOG2( 1.0 - prop );
		*totalBits += nNotZero * codeBits;
		if (*returnBits)
			for (i=nZero; i<*n; ++i)
				zList[ order[i] ] += codeBits;
	}
}

int EBICCode (double z, double prob, double *minBits)
{
	double  az, bitsZero, bitsOne, qBits[3];
	int 	iz, minCode, code, start, stop, offset, i;
	int     zero;
	
	if (prob == 1.0)
	{	bitsOne  =  0.0;
		bitsZero = 10.0;				// Enough to make sure that zero is not possible.
	} else
	{	bitsOne  = - LOG2(prob);
		bitsZero = - LOG2(1.0-prob);	// Entropy relative effect on zero code
	}
	az = ABS(z);
	iz = ABS(iRound(z));
	*minBits = 777.7;
	if (0 == iz)
	{	start = -1; stop = 1; }
	else
	{	start = 0; stop = 2; }
	for (offset=start, i=0; offset <= stop; ++offset, ++i)
	{	code = iz - offset;
		qBits[i] =  (double) (SignedCauchyBitLength(code) - 1)
					+ DataBits(((double)code) - az)
					+ ((code == 0) ? bitsZero : bitsOne);
		// printf("%5.2f -> ( %3d , %6.2f ) \n", z,  code, qBits[i]);
		if (qBits[i] < *minBits)
		{	minCode = code;
			*minBits = qBits[i];
			zero = (code == 0) ? 1 : 0;
		}
	}
	if (zero) 					// Return value NOT adjusted for entropy
		*minBits -= bitsZero;
	else 
		*minBits -= bitsOne;
	return((z > 0) ? minCode : -minCode);
}

