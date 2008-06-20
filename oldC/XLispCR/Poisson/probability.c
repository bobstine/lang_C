/*      Probability routines C implementation.          7 Dec 94 ... Cleaned for Unix loading.    4 Dec 94 ... Added double factorial version.   28 Apr 94 ... Removed from HMM code.    */#include <math.h>#include "probability.h"_static_ double TruncatedDiscreteProb(long x, double *table, long n, long lo, long hi){	double min, prob;		if (lo < 0) lo = 0;	if (hi >= n) hi = n-1;#ifdef _debugging_	if (lo > hi ) 	{	printf ("lo %d > hi %d for x=%d \n", lo, hi, x);		Wait();	}#endif	if (lo == hi) return (1.0);	if (lo == 0) min = 0;	else         min = table[lo-1];	if (x == 0) prob = table[0];	else        prob = table[x] - table[x-1];	return (prob / (table[hi]-min));}/*------------------------------------------*//*		  			Poisson					*//*------------------------------------------*/_static_ double PoissonDens(long x, double lambda){	long i;	double pr;		pr = exp(-lambda);	for(i=1; i<=x; ++i)			pr *= lambda / (double) i;	return (pr);}_static_ void CalcPoissonDens(double lambda, long n, double *dens){	long i;		dens[0] = exp(-lambda);	for(i=1; i<n-1; ++i)			dens[i] = dens[i-1] * lambda / (double) i;}_static_ void CalcPoissonCDF (double lambda, long n, double *cdf){	long i;		cdf[0] = exp(-lambda);	for(i=1; i<n-1; ++i)						/* compute dens */		cdf[i] = cdf[i-1] * lambda / (double) i;	for(i=1; i<n-1; ++i)						/* accumulate CDF */		cdf[i] += cdf[i-1];	cdf[n-1] = 1.0;								/* force 1 at end */}/*------------------------------------------*//*		  			Binomial				*//*------------------------------------------*/_static_ double BinomialDens (long x, long n, double p){	long i;	double num, den, pPower,q, qPower;		if (n == 0) return (1.0);	if (n-x < x) 				/* swap so that x is small */	{	p = 1.0 - p;		x = n-x;	}	q = 1.0 - p;		pPower = 1.0;	for (i=0; i<x; ++i) pPower *= p;	qPower = 1.0;	for (i=0; i<n-x; ++i) qPower *= q;		num = 1.0; den = 1.0;	while (x > 0)	{	num *= (double) n; --n;		den *= (double) x; --x;	}	return ( num * pPower * qPower / den );}_static_ void CalcBinomialCDF (long n,double p,double *cdf){	long i, lim;	double num, den, pPower, q, qPower, cum;			pPower = 1.0;	qPower = 1.0;	q = 1.0 - p;	for (i=0; i<n; ++i) qPower *= q;		num = 1.0; den = 1.0;	*cdf = ( num * pPower * qPower / den );	i = 1; lim=n;	while (i <= lim)	{	num *= (double) n; --n;		den *= (double) i; ++i;		pPower *= p;		qPower /= q;		cum = *cdf++ + ( num * pPower * qPower / den );		*cdf = cum;	}}_static_ double BinomialPower (long x,long n,double p){	long i;	double  pPower, q, qPower;		/*  if (n == 0) return (1.0);  */	q = 1.0 - p;		pPower = 1.0;	for (i=0; i<x; ++i) pPower *= p;	qPower = 1.0;	for (i=0; i<n-x; ++i) qPower *= q;		return ( pPower * qPower );}/*------------------------------------------*//*		  			Multinomial				*//*------------------------------------------*/_static_ double MultinomialDens (long *kVec,double *pVec, long nCells){	double result;	long i, n=0;		for (i=0; i<nCells; ++i)		n += kVec[i];	result = dFactorial(n);							/* Calls to dFact if big int */	for (i=0; i<nCells; ++i)		result *= Power(*pVec++,kVec[i]) / dFactorial(kVec[i]);	return(result);}	/*------------------------------------------*//*		  			Utilities				*//*------------------------------------------*/_static_ double ChooseDouble (long k, long n)		/* k from n */{		double num, den;		if (k == 0) return (1.0);	if (n-k < k) k = n-k;		/* swap */		num = 1.0; den = 1.0;	while (k > 0)	{	num *=  (double) n; --n;		den *=  (double) k; --k;	}	return ( num / den );}_static_ long ChooseInt (long k, long n)			/* k from n */{		long num, den;		if (k == 0) return (1);	if (n-k < k) k = n-k;		/* swap */		num = 1; den = 1;	while (k > 0)	{	num *=  n; --n;		den *=  k; --k;	}	return ( num / den );}_static_ double dFactorial(long k)			/* added 4 Dec 94 for handling big longs.	*/{	long i;	double result=1.0;		if (k == 0) return(result);	if (k == 1) return(result);	for (i=2; i<=k; ++i)		result *= (double) i;	return(result);}_static_ long Factorial(long k){	long i;	long result=1;		if (k == 0) return(result);	if (k == 1) return(result);	for (i=2; i<=k; ++i)		result *= i;	return(result);}_static_ double HyperDens (long *kVec, long *nVec, long kSum, long nSum, long nGroups){	double num;		++kVec;				/* shift kVec by one so cell i+1 matches to cell i */		num = ChooseDouble(*kVec++, *nVec++);	-- nGroups;	while (nGroups > 0)	{	if (*kVec > 0)			num *= ChooseDouble(*kVec, *nVec);		/*  10/12  */		++kVec;  ++nVec;		-- nGroups;	}	return ( num / ChooseDouble(kSum, nSum) );}_static_ long IntPower(long x,long k){	long result=1;		while(k>0)	{	result *= x;		--k;	}	return(result);}			_static_ double Power(double x, long k){	double result=1.0;		while(k>0)	{	result *= x;		--k;	}	return(result);}			