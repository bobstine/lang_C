// Include file for xlisp param passing definitions#include "xlsx.h"//	Global data//  Local prototypesvoid AnotherSumVector	(XLSXblock *parms);void AnotherRawSumVector (double *x, long *n, double *sum);void AnotherScaleVector(XLSXblock *parms);//	-------------  Code  -----------------void AnotherRawSumVector (double *x, long *n, double *sum){	long i;	 	*sum = 0.0;	for (i=0; i< *n; ++i)		*sum += x[i];}void AnotherSumVector (XLSXblock *parms){	long i, n;	double *x, *theSum;			// Parse the xlisp parameter block	x		= 	(double *) XLSXargv(parms,0);	n		= * (long *)   XLSXargv(parms,1);	theSum	=   (double *) XLSXargv(parms,2);		// This dyn link to caller failed...MyPrint("Starting to sum...\n");		// Do the work	*theSum = 0.0;	for (i=0; i< n; ++i)		*theSum += x[i];			//  Kills the link when refer to the global variable via conversion.	//  *theSum += (double) gOffset;}void AnotherScaleVector (XLSXblock *parms){	long i, n;	double *x, scalar;		// Parse the xlisp parameter block	x		= 	(double *) XLSXargv(parms,0);	n		= * (long *)   XLSXargv(parms,1);	scalar	= * (double *) XLSXargv(parms,2);		// Do the work	for (i=0; i< n; ++i)		x[i] *= scalar;}