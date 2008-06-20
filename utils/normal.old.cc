// $Id: normal.old.cc,v 1.1 2005/06/14 22:36:21 bob Exp $
// From xlispstat

/* Double precision version of routines in ERF from the netlib SPECFUN */
/* library. Translated by f2c and modified. */

#include "normal.h"

#include <math.h>

namespace {

  inline double abs(double x) { return (x >= 0) ? x : -x ; }

  void
  calerf (double arg, double* result, int jint)
  {
  /* ------------------------------------------------------------------ */
  /* This packet evaluates  erf(x),  erfc(x),  and  exp(x*x)*erfc(x) */
  /*   for a real argument  x.  It contains three FUNCTION type */
  /*   subprograms: ERF, ERFC, and ERFCX (or DERF, DERFC, and DERFCX), */
  /*   and one SUBROUTINE type subprogram, CALERF.  The calling */
  /*   statements for the primary entries are: */

  /*                   Y=ERF(X)     (or   Y=DERF(X)), */

  /*                   Y=ERFC(X)    (or   Y=DERFC(X)), */
  /*   and */
  /*                   Y=ERFCX(X)   (or   Y=DERFCX(X)). */

  /*   The routine  CALERF  is intended for internal packet use only, */
  /*   all computations within the packet being concentrated in this */
  /*   routine.  The function subprograms invoke  CALERF  with the */
  /*   statement */

  /*          CALL CALERF(ARG,RESULT,JINT) */

  /*   where the parameter usage is as follows */

  /*      Function                     Parameters for CALERF */
  /*       call              ARG                  Result          JINT */

  /*     ERF(ARG)      ANY REAL ARGUMENT         ERF(ARG)          0 */
  /*     ERFC(ARG)     ABS(ARG) .LT. XBIG        ERFC(ARG)         1 */
  /*     ERFCX(ARG)    XNEG .LT. ARG .LT. XMAX   ERFCX(ARG)        2 */

  /*   The main computation evaluates near-minimax approximations */
  /*   from "Rational Chebyshev approximations for the error function" */
  /*   by W. J. Cody, Math. Comp., 1969, PP. 631-638.  This */
  /*   transportable program uses rational functions that theoretically */
  /*   approximate  erf(x)  and  erfc(x)  to at least 18 significant */
  /*   decimal digits.  The accuracy achieved depends on the arithmetic */
  /*   system, the compiler, the intrinsic functions, and proper */
  /*   selection of the machine-dependent constants. */

  /* ******************************************************************* */
  /* ******************************************************************* */

  /* Explanation of machine-dependent constants */

  /*   XMIN   = the smallest positive floating-point number. */
  /*   XINF   = the largest positive finite floating-point number. */
  /*   XNEG   = the largest negative argument acceptable to ERFCX; */
  /*            the negative of the solution to the equation */
  /*            2*exp(x*x) = XINF. */
  /*   XSMALL = argument below which erf(x) may be represented by */
  /*            2*x/sqrt(pi)  and above which  x*x  will not underflow. */
  /*            A conservative value is the largest machine number X */
  /*            such that   1.0 + X = 1.0   to machine precision. */
  /*   XBIG   = largest argument acceptable to ERFC;  solution to */
  /*            the equation:  W(x) * (1-0.5/x**2) = XMIN,  where */
  /*            W(x) = exp(-x*x)/[x*sqrt(pi)]. */
  /*   XHUGE  = argument above which  1.0 - 1/(2*x*x) = 1.0  to */
  /*            machine precision.  A conservative value is */
  /*            1/[2*sqrt(XSMALL)] */
  /*   XMAX   = largest acceptable argument to ERFCX; the minimum */
  /*            of XINF and 1/[sqrt(pi)*XMIN]. */

  /*   Approximate values for some important machines are: */

  /*                          XMIN       XINF        XNEG     XSMALL */

  /*  IEEE (IBM/XT, */
  /*    SUN, etc.)  (D.P.)  2.23D-308   1.79D+308   -26.628  1.11D-16 */

  /*                          XBIG       XHUGE       XMAX */

  /*  IEEE (IBM/XT, */
  /*    SUN, etc.)  (D.P.)  26.543      6.71D+7     2.53D+307 */


  /* ******************************************************************* */
  /* ******************************************************************* */

  /* Error returns */

  /*  The program returns  ERFC = 0      for  ARG .GE. XBIG; */
  /*                       ERFCX = XINF  for  ARG .LT. XNEG; */
  /*                       ERFCX = 0     for  ARG .GE. XMAX. */


  /*  Author: W. J. Cody */
  /*          Mathematics and Computer Science Division */
  /*          Argonne National Laboratory */
  /*          Argonne, IL 60439 */

  /*  Latest modification: March 19, 1990 */

  /* ------------------------------------------------------------------ */
  double xden, xnum;
  int i;
  double x, y, del, ysq;
  /* ------------------------------------------------------------------ */
  /*  Mathematical constants */
  /* ------------------------------------------------------------------ */
  static double four = 4.;
  static double one = 1.;
  static double half = .5;
  static double two = 2.;
  static double zero = 0.;
  static double sqrpi = .56418958354775628695;
  static double thresh = .46875;
  static double sixten = 16.;

  static double xinf = 1.79e308;
  static double xneg = -26.628;
  static double xsmall = 1.11e-16;
  static double xbig = 26.543;
  static double xhuge = 6.71e7;
  static double xmax = 2.53e307;

  /* ------------------------------------------------------------------ */
  /*  Coefficients for approximation to  erf  in first interval */
  /* ------------------------------------------------------------------ */
  static double a[5] = { 3.1611237438705656,113.864154151050156,
			   377.485237685302021,3209.37758913846947,
			   .185777706184603153 };
  static double b[4] = { 23.6012909523441209,244.024637934444173,
			   1282.61652607737228,2844.23683343917062 };
  /* ------------------------------------------------------------------ */
  /*  Coefficients for approximation to  erfc  in second interval */
  /* ------------------------------------------------------------------ */
  static double c[9] = { .564188496988670089,8.88314979438837594,
			   66.1191906371416295,298.635138197400131,
			   881.95222124176909,1712.04761263407058,
			   2051.07837782607147,1230.33935479799725,
			   2.15311535474403846e-8 };
  static double d[8] = { 15.7449261107098347,117.693950891312499,
			   537.181101862009858,1621.38957456669019,
			   3290.79923573345963,4362.61909014324716,
			   3439.36767414372164,1230.33935480374942 };
  /* ------------------------------------------------------------------ */
  /*  Coefficients for approximation to  erfc  in third interval */
  /* ------------------------------------------------------------------ */
  static double p[6] = { .305326634961232344,.360344899949804439,
			   .125781726111229246,.0160837851487422766,
			   6.58749161529837803e-4,.0163153871373020978 };
  static double q[5] = { 2.56852019228982242,1.87295284992346047,
			   .527905102951428412,.0605183413124413191,
			   .00233520497626869185 };
  /* ------------------------------------------------------------------ */
  x = arg;
  y = abs(x);
  if (y <= thresh) {
    /* ------------------------------------------------------------------ */
    /*  Evaluate  erf  for  |X| <= 0.46875 */
    /* ------------------------------------------------------------------ */
    ysq = zero;
    if (y > xsmall) {
      ysq = y * y;
    }
    xnum = a[4] * ysq;
    xden = ysq;
    for (i = 1; i <= 3; ++i) {
      xnum = (xnum + a[i - 1]) * ysq;
      xden = (xden + b[i - 1]) * ysq;
    }
    *result = x * (xnum + a[3]) / (xden + b[3]);
    if (jint != 0) {
      *result = one - *result;
    }
    if (jint == 2) {
      *result = exp(ysq) * *result;
    }
    return;
    /* ------------------------------------------------------------------ */
    /*  Evaluate  erfc  for 0.46875 <= |X| <= 4.0 */
    /* ------------------------------------------------------------------ */
  }
  else if (y <= four) {
    xnum = c[8] * y;
    xden = y;
    for (i = 1; i <= 7; ++i) {
      xnum = (xnum + c[i - 1]) * y;
      xden = (xden + d[i - 1]) * y;
    }
    *result = (xnum + c[7]) / (xden + d[7]);
    if (jint != 2) {
      ysq = floor(y * sixten) / sixten;
      del = (y - ysq) * (y + ysq);
      *result = exp(-ysq * ysq) * exp(-del) * *result;
    }
    /* ------------------------------------------------------------------ */
    /*  Evaluate  erfc  for |X| > 4.0 */
    /* ------------------------------------------------------------------ */
  }
  else {
    *result = zero;
    if (y >= xbig) {
      if (jint != 2 || y >= xmax) {
	goto L300;
      }
      if (y >= xhuge) {
	*result = sqrpi / y;
	goto L300;
      }
    }
    ysq = one / (y * y);
    xnum = p[5] * ysq;
    xden = ysq;
    for (i = 1; i <= 4; ++i) {
      xnum = (xnum + p[i - 1]) * ysq;
      xden = (xden + q[i - 1]) * ysq;
    }
    *result = ysq * (xnum + p[4]) / (xden + q[4]);
    *result = (sqrpi - *result) / y;
    if (jint != 2) {
      ysq = floor(y * sixten) / sixten;
      del = (y - ysq) * (y + ysq);
      *result = exp(-ysq * ysq) * exp(-del) * *result;
    }
  }
  /* ------------------------------------------------------------------ */
  /*  Fix up for negative argument, erf, etc. */
  /* ------------------------------------------------------------------ */
 L300:
  if (jint == 0) {
    *result = half - *result + half;
    if (x < zero) {
      *result = -(*result);
    }
  }
  else if (jint == 1) {
    if (x < zero) {
      *result = two - *result;
    }
  }
  else {
    if (x < zero) {
      if (x < xneg) {
	*result = xinf;
      }
      else {
	ysq = -floor(-(x * sixten)) / sixten;
	del = (x - ysq) * (x + ysq);
	y = exp(ysq * ysq) * exp(del);
	*result = y + y - *result;
      }
    }
  }
  return;
  }
  

  
  
  
  
  double
  ppnd (double p, int * ifault)
  {
    /* System generated locals */
    double ret_val;

    /* Local variables */
    double q, r;

    /* ALGORITHM AS241  APPL. STATIST. (1988) VOL. 37, NO. 3 */
    
    /* Produces the normal deviate Z corresponding to a given lower */
    /* tail area of P; Z is accurate to about 1 part in 10**16. */
    
    /* The hash sums below are the sums of the mantissas of the */
    /* coefficients.   They are included for use in checking */
    /* transcription. */
    
    /* Coefficients for P close to 0.5 */
    /* HASH SUM AB    55.88319 28806 14901 4439 */

    /* Coefficients for P not close to 0, 0.5 or 1. */
    /* HASH SUM CD    49.33206 50330 16102 89036 */

    /* Coefficients for P near 0 or 1. */
    /* HASH SUM EF    47.52583 31754 92896 71629 */

    *ifault = 0;
    q = p - .5;
    if (abs(q) <= .425) {
      r = .180625 - q * q;
      ret_val = q * (((((((r * 2509.0809287301226727 +
			   33430.575583588128105) * r +
			  67265.770927008700853) * r + 
			 45921.953931549871457) * r +
			13731.693765509461125) * r + 
		       1971.5909503065514427) * r +
		      133.14166789178437745) * r + 
		     3.387132872796366608) /
	(((((((r * 5226.495278852854561 + 
	       28729.085735721942674) * r +
	      39307.89580009271061) * r + 
	     21213.794301586595867) * r +
	    5394.1960214247511077) * r + 
	   687.1870074920579083) * r +
	  42.313330701600911252) * r + 1.);
      return ret_val;
    } else {
      if (q < 0.) {
	r = p;
      } else {
	r = 1. - p;
      }
      if (r <= 0.) {
	*ifault = 1;
	ret_val = 0.;
	return ret_val;
      }
      r = sqrt(-log(r));
      if (r <= 5.) {
	r += -1.6;
	ret_val = (((((((r * 7.7454501427834140764e-4 + 
			 .0227238449892691845833) * r +
			.24178072517745061177) * r 
		       + 1.27045825245236838258) * r +
		      3.64784832476320460504) * 
		     r + 5.7694972214606914055) * r +
		    4.6303378461565452959) * 
		   r + 1.42343711074968357734) /
	  (((((((r * 1.05075007164441684324e-9 +
		 5.475938084995344946e-4) * r 
		+ .0151986665636164571966) * r +
	       .14810397642748007459) * 
	      r + .68976733498510000455) * r +
	     1.6763848301838038494) * r +
	    2.05319162663775882187) * r + 1.);
      } else {
	r += -5.;
	ret_val = (((((((r * 2.01033439929228813265e-7 + 
			 2.71155556874348757815e-5) * r +
			.0012426609473880784386) 
		       * r + .026532189526576123093) * r +
		      .29656057182850489123) * r +
		     1.7848265399172913358) * r + 5.4637849111641143699)
		   * r + 6.6579046435011037772) /
	  (((((((r * 2.04426310338993978564e-15 +
		 1.4215117583164458887e-7) * 
		r + 1.8463183175100546818e-5) * r + 
	       7.868691311456132591e-4) * r +
	      .0148753612908506148525) * r +
	     .13692988092273580531) * r + .59983220655588793769) * 
	   r + 1.);
      }
      if (q < 0.) {
	ret_val = -ret_val;
      }
      return ret_val;
    }
  }
  
  double SQRT2   (1.414213562373095049);

  double SQRT2PI (2.5066282746310005);
}
  //////////////////////////////////////////////////////////////
  
double
erf (double x)
{
  int jint;
  double result;

  jint = 0;
  calerf(x, &result, jint);
  return result;
}

double
erfc (double x)
{
  int jint;
  double result;

  jint = 1;
  calerf(x, &result, jint);
  return result;
}

double
erfcx (double x)
{
  int jint;
  double result;

  jint = 2;
  calerf(x, &result, jint);
  return result;
}


double
normal_density(double x)
{
  return exp(- x * x / 2.0)/SQRT2PI;
}

double
normal_cdf (double x)
{
  double y, phi;

  y = x / SQRT2;
  if (y < 0) {
    calerf(-y, &phi, 1);
    return 0.5 * phi;
  }
  else {
    calerf(y, &phi, 1);
    return  1.0 - 0.5 * phi;
  }
}

double
normal_quantile (double p)
{
  int ret_code;
  
  return ppnd(p, &ret_code);
}
