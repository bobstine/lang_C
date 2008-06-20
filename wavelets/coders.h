//	Exported prototypes taking a coef vector which is on a z-score scale.
//  
//		- Each as same first five arguments, with optional arguments following
//		for BIC (parameter space size) and EBIC (indices which sort input). 
//
//		- If the input  returnBits != 0, then overwrites the input zList with 
//		associated bits required for each term.
//

void AIC  (double *zList, long *n, long *codes, long *returnBits, double *totalBits);

void BIC  (double *zList, long *n, long *codes, long *returnBits, double *totalBits, double *M);

void RIC  (double *zList, long *n, long *codes, long *returnBits, double *totalBits);

void EBIC (double *zList, long *n, long *codes, long *returnBits, double *totalBits, long *order);


