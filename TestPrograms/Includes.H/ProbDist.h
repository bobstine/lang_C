/*		PROBABILITY DISTRIBUTIONS		9 Feb 89 ... Created.		*/#ifndef _PROBDIST_#define _PROBDIST_double GaussCDF (double x);     /* Cumulative gaussian at x */double GaussInv (double prob);     /* Inverts the gaussian cum for the prob value given as argument.*/double TCDF (double t, int df);     /* Computes the prob of being less than "t" with df deg freedom. */double TInv (double prob, int df);     /* Finds the t val which yields the desired cum probability. */double Chi2CDF (double x, int df);     /* Determines the tail of of a chi-squared distribution with df degrees*/	 /*	of freedom.*/double FCDF (double f, int numDF, int denDF);     /* Evaluates the cum F dist at f with the given degrees of freedom. */#endif