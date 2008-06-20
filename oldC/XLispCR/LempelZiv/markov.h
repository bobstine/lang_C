/*		Markov.h		25 Jun 96 ... Created.		*/// Functions for handling BINARY markov arrays of order 2^kvoid GenerateMarkovData (double p[], long *pk, long x[], long  *pn, long *pSeed);/*	The first k values of x are expected to be the initialization values for	the data series.  If the seed is zero, then the internal clock is used to init	the random series.*/void CalcMarkovArray(long x[], long *pn, long leaves[], long *pk, long *offset, long *pError);/*	Leaves needs room for 2 * 2^k longs.  Results are concatentated (j,n) pairs of	counts for each leaf in the tree.  Errors are negative if a problem occurs.		Typically, one sets offset == k.  Allow differences for comparison of models of 	different orders fit to the same data.*/void GenerateMarkovArray (double p[], long initial[], long *pK, long *pN, long leaves[], long *pSeed);/*	Does the leaf enumeration in place, combining the actions of GenerateMarkovData and	CalcMarkovArray.  Thus you can have a very, very long series.  All that matters (for	memory use) is the dimension of the markov process leaves (2 * 2^k).		Generates (n-k) more values from the process, extending the "virtual series" to a	full length of n.*/void CalcMarkovArrayFromFile(long leaves[], long *pK, long *pN, long *pError);/*	Calculates a binary markov array using 5 bits from characters found in the file	markov.txt.  Reads up to N characters before halting.  On exit, K is the number	of non-alpha characters that were skipped over in reading N from the file. 		Generates an error if file ends before accumulating N usable chars, in which case N 	is set to the number actually read.  K is still the number skipped.*/