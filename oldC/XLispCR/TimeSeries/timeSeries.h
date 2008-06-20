//	Prototypes of visible functionsvoid GenerateAR (double *y, long *pT, double *coefs, long *pOrder);void CalcACS	(double *y, long *pT, double *acs,   long *jLimit, long *method);void EstimateAR1 (double *y, long *pT, double *coefs, long *pOrder, double *RSS);void EstimateLS (double *y, long *pT, double *coefs, long *pOrder, long *center, 					long *start, double *rss);/*     Computes the LS estimator using either raw or centered data and chosen model order.   Returns residual SS computed with response running from start -> T-1 if desired    (indicated by input rss > 0).  Start must be at least p (zero origin for all this!).*/void EstimateBoth (double *y, long *pT, double *ywCoefs, double *lsCoefs, long *pOrder);//  Helpers of some external valuevoid ARCovariances (double phi[], long p, double cov[]);/*     Finds the covariances from the input vector of regression ar coefs. The   resulting covariance vector must be of length at least p+1.*/void ARCovarRoot (double phi[], long p, double **sr);	/*     Lower triagular portion of sr is the square root of the covariance matrix of   an AR process with coefs phi (in regr form).  sr must be at least pxp.*/