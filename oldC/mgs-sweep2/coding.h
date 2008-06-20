/*
  21 Sep 01 ...   Created to encapsulate coding-related functions.
*/

double
log2 (double x);

///////////////////////////////////////////////////////////////////

double
codeIndexLength(double gShare, double gProb, long k);

double
fStatisticThreshold (double gShare, double gProb, long k);

///////////////////////////////////////////////////////////////////
/*
  These are pdf's for discrete, positive density functions.
  The range for k should be restricted to k=1,2,3...
*/

double
geometricPDF (double p, long k);

double
cauchyPDF (long k);
