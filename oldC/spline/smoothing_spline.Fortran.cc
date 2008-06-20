// $Id: smoothing_spline.Fortran.cc,v 1.2 2004/08/16 17:07:35 bob Exp $

// ftoc version of smoothing spline code

//  Evil macros

#define min(a,b) (((a) < (b)) ? (a) : (b))
#define max(a,b) (((a) > (b)) ? (a) : (b))
#define abs(a) (((a) >= 0) ? (a) : (-(a)))

double bvalue_(double *t, double *bcoef, int *n, int *k, double *x, int *jderiv);
int    interv_(double *xt, int *lxt, double *x, int *left, int *mflag);
void   sknot(double *x, int *n, double *knot, int *k, int *allk);
void   qsbart(double *dblParms, int *lngParms, double *xs, double *ys, double *ws,
	      double *knot, double *coef, double *sz, double *lev, int *iparms, double *scrtch);

int    bsplvd_(double *t, int *k, double *x, int *left, double *a, double *dbiatx, int *nderiv);
int    bsplvb_(double *t, int *jhigh, int *index, double *x, int *left, double *biatx);

int    dpbfa_(double *abd, int *lda, int *n, int *m, int *info);
int    dpbsl_(double *abd, int *lda, int * n, int *m, double *b);

int    sbart_(double *penalt, double *dofoff, double *xs, double *ys, double *ws, int *n,
	      double *knot, int *nk, double *coef, double *sz,
	      double *lev, double *crit, int *icrit, double *lambda, int *ispar, int *isetup,
	      double *xwy, int *ld4, int *ldnk, int *ier);
int    sgram_(double *sg0, double *sg1, double *sg2, double *sg3, double *tb, int *nb);
int    sinerp_(double *abd, int *ld4, int *nk, double *p1ip, double *p2ip, int *ldnk, int *flag_);
int    sslvrg_(double *penalt, double *dofoff, double *x, double *y, double *w, int *n,
	       double *knot, int *nk, double *coef, double *sz, 
	       double *lev, double *crit, int *icrit, double *lambda, double *xwy, double *hBlock[], double *sBlock[],
	       double *abd, double *p1ip, double *p2ip, int *ld4, int *ldnk, int *info);
int    stxwx_(double *x, double *z, double *w, int *k, double *xknot,
	      int  *n, double *y, double *hs0, double *hs1, double *hs2, double *hs3);


double  bvalue_(double *t, double *bcoef, int *n, int *k, double *x, int *jderiv)
{
  /* System generated locals */
  int  i_1, i_2;
  double ret_val;
  
  /* Local variables */
  double fkmj;
  int  i, j, mflag, jcmin, jcmax, jdrvp1;
  double aj[20];
  int  jc;
  double dm[20];
  int  jj;
  double dp[20];
  int  km1, imk, kmj, ilo, nmi;

  /*
  std::cout << "\nBVALUE: k=" << *k << " with x=" << *x << " and order " << *jderiv << " (" << *n+*k << " knots, " << *n << " coefs)\n";
  for (int i=0; i<*n+*k; ++i)
    std::cout << t[i] << " ";
  std::cout << std::endl;
  for (int i=0; i<*n; ++i)
    std::cout << bcoef[i] << " ";
  std::cout << std::endl;
  */
      
  /* Calls  interv */
  
  /* Calculates value at  x  of  jderiv-th derivative of spline from b-repr. 	*/
  /*  the spline is taken to be continuous from the right. 			*/
  
  /* ******  i n p u t ****** */
  /*  t, bcoef, n, k......forms the b-representation of the spline  f  to 	*/
  /*        be evaluated. specifically, 					*/
  /*  t.....    knot sequence, of length  n+k, assumed nondecreasing. 		*/
  /*  bcoef.....b-coefficient sequence, of length  n . 			        */
  /*  n.....    length of  bcoef  and dimension of s(k,t),		        */
  /*        a s s u m e d  positive . 					        */
  /*  k.....order of the spline . 						*/
  
  /*  w a r n i n g . . .   the restriction  k .le. kmax (=20)  is imposed 	*/
  /*        arbitrarily by the dimension statement for  aj, dm, dm  below, 	*/
  /*        but is  n o w h e r e  c h e c k e d. 				*/
  
  /*  x.....the point at which to evaluate . 					*/
  /*  jderiv.....int  giving the order of the derivative to be evaluated 	*/
  /*        a s s u m e d  to be zero or positive. 				*/
  
  /* ******  o u t p u t  ****** */
  /*  bvalue.....the value of the (jderiv)-th derivative of  f  at  x .         */
  
  /* ******  m e t h o d  ****** */
  /*     the nontrivial knot interval  (t(i),t(i+1))  containing  x  is lo-     */
  /*  cated with the aid of  interv . the  k  b-coeffs of  f  relevant for      */
  /*  this interval are then obtained from  bcoef (or taken to be zero if       */
  /*  not explicitly available) and are then differenced  jderiv  times to      */
  /*  obtain the b-coeffs of  (d**jderiv)f  relevant for that interval.         */
  /*  precisely, with  j = jderiv, we have from x.(12) of the text that         */
    
  /*     (d**j)f  =  sum ( bcoef(.,j)*b(.,k-j,t) ) */
  
  /*  where */
  /*                   / bcoef(.),                     ,  j .eq. 0            */
  /*                   /                                                      */
  /*    bcoef(.,j)  =  / bcoef(.,j-1) - bcoef(.-1,j-1)                        */
  /*                   / ----------------------------- ,  j .gt. 0            */
  /*                   /    (t(.+k-j) - t(.))/(k-j)                           */
  
  /*     then, we use repeatedly the fact that                                */
  
  /*    sum ( a(.)*b(.,m,t)(x) )  =  sum ( a(.,x)*b(.,m-1,t)(x) )             */
  /*  with 								        */
  /*                 (x - t(.))*a(.) + (t(.+m-1) - x)*a(.-1)                  */
  /*    a(.,x)  =    ---------------------------------------                  */
  /*                 (x - t(.))      + (t(.+m-1) - x)                         */
  
  /*  to write  (d**j)f(x)  eventually as a linear combination of b-splines   */
  /*  of order  1 , and the coefficient for  b(i,1,t)(x)  must then           */
  /*  be the desired number  (d**j)f(x). (see x.(17)-(19) of text).           */
  
  /*     dimension t(n+k) */
  /* Current fortran standard makes it impossible to specify the length of    */
  
  /*  t  precisely without the introduction of otherwise superfluous */
  /*  additional arguments. */
  /* Parameter adjustments */
  --bcoef;
  --t;
  
  /* Function Body */
  ret_val = 0.0;
  if (*jderiv >= *k) {
    goto L99;
  }
  
  /*  *** find  i  s.t.  1 .le. i .lt. n+k  and  t(i) .lt. t(i+1) and */
  /*      t(i) .le. x .lt. t(i+1) . if no such i can be found,  x  lies */
  /*      outside the support of  the spline  f  and  bvalue = 0. */
  /*      (the asymmetry in this choice of  i  makes  f  rightcontinuous) */
  
  if (*x != t[*n + 1] || t[*n + 1] != t[*n + *k]) {goto L700;}
  i = *n;
  goto L701;
 L700:
  i_1 = *n + *k;
  interv_(&t[1], &i_1, x, &i, &mflag);
  if (mflag != 0) {
    goto L99;
  }
 L701:
  /*  *** if k = 1 (and jderiv = 0), bvalue = bcoef(i). */
  km1 = *k - 1;
  if (km1 > 0) {
    goto L1;
  }
  ret_val = bcoef[i];
  goto L99;
  
  /*  *** store the k b-spline coefficients relevant for the knot interval */
  /*     (t(i),t(i+1)) in aj(1),...,aj(k) and compute dm(j) = x - t(i+1-j), */
  /*     dp(j) = t(i+j) - x, j=1,...,k-1 . set any of the aj not obtainable */
  /*     from input to zero. set any t.s not obtainable equal to t(1) or */
  /*     to t(n+k) appropriately. */
 L1:
  jcmin = 1;
  imk = i - *k;
  if (imk >= 0) {
    goto L8;
  }
  jcmin = 1 - imk;
  i_1 = i;
  for (j = 1; j <= i_1; ++j) {
    /* L5: */
    dm[j - 1] = *x - t[i + 1 - j];
  }
  i_1 = km1;
  for (j = i; j <= i_1; ++j) {
    aj[*k - j - 1] = (double)0.;
    /* L6: */
    dm[j - 1] = dm[i - 1];
  }
  goto L10;
 L8:
  i_1 = km1;
  for (j = 1; j <= i_1; ++j) {
    /* L9: */
    dm[j - 1] = *x - t[i + 1 - j];
  }
  
 L10:
  jcmax = *k;
  nmi = *n - i;
  if (nmi >= 0) {
    goto L18;
  }
  jcmax = *k + nmi;
  i_1 = jcmax;
  for (j = 1; j <= i_1; ++j) {
    dp[j - 1] = t[i + j] - *x;
  }
  i_1 = km1;
  for (j = jcmax; j <= i_1; ++j) {
    aj[j] = (double)0.;
    dp[j - 1] = dp[jcmax - 1];
  }
  goto L20;
 L18:
  i_1 = km1;
  for (j = 1; j <= i_1; ++j) {
    dp[j - 1] = t[i + j] - *x;
  }
  
 L20:
  i_1 = jcmax;
  for (jc = jcmin; jc <= i_1; ++jc) {
    aj[jc - 1] = bcoef[imk + jc];
  }
  
  /*               *** difference the coefficients  jderiv  times. */
  if (*jderiv == 0) {goto L30; }
  i_1 = *jderiv;
  for (j = 1; j <= i_1; ++j) {
    kmj = *k - j;
    fkmj = (double) kmj;
    ilo = kmj;
    i_2 = kmj;
    for (jj = 1; jj <= i_2; ++jj) {
      aj[jj - 1] = (aj[jj] - aj[jj - 1]) / (dm[ilo - 1] + dp[jj - 1]) * fkmj;
      --ilo;
    }
  }
  
  /*  *** compute value at  x  in (t(i),t(i+1)) of jderiv-th derivative, */
  /*     given its relevant b-spline coeffs in aj(1),...,aj(k-jderiv). */
  
 L30:
  if (*jderiv == km1) {goto L39;}
  jdrvp1 = *jderiv + 1;
  i_2 = km1;
  for (j = jdrvp1; j <= i_2; ++j) {
    kmj = *k - j;
    ilo = kmj;
    i_1 = kmj;
    for (jj = 1; jj <= i_1; ++jj) {
      aj[jj - 1] = (aj[jj] * dm[ilo - 1] + aj[jj - 1] * dp[jj - 1]) / (dm[ilo - 1] + dp[jj - 1]);
      --ilo;
    }
  }
 L39:
  ret_val = aj[0];
  
 L99:
  return ret_val;
} /* bvalue_ */



  int interv_(double *xt, int *lxt, double *x, int *left, int *mflag)
  {
    int  ilo = 1;
    int  istep, middle, ihi; // c__4 = 4;
    
    /* Computes  left = max( i ; 1 <= i <= lxt  &&  xt(i) <= x )  . */
    /* ******  i n p u t  ****** */
    /*  xt.....a double precision sequence, of length  lxt , assumed to be nondecreasing*/
    /*  lxt....number of terms in the sequence  xt . */
    /*  x..... the point whose location with respect to the sequence  xt  is */
    /*         to be determined. */
    /* ******  o u t p u t  ****** */
    /*  left, mflag.....both integers, whose values are */
    /*   1     -1      if             x <  xt(1) */
    /*   i      0      if   xt(i)  <= x < xt(i+1) */
    /*  lxt     1      if  xt(lxt) <= x */
    /*        in particular,  mflag = 0 is the 'usual' case.  mflag .ne. 0 */
    /*        indicates that  x  lies outside the halfopen interval */
    /*        xt(1) .le. y .lt. xt(lxt) . the asymmetric treatment of the */
    /*        interval is due to the decision to make all pp functions cont- */
    /*        inuous from the right. */
    
    /* ******  m e t h o d  ****** */
    /*  the program is designed to be efficient in the common situation that 	*/
    /*  it is called repeatedly, with  x  taken from an increasing or decrea- 	*/
    /*  sing sequence. this will happen, e.g., when a pp function is to be 	*/
    /*  graphed. the first guess for  left  is therefore taken to be the val- 	*/
    /*  ue returned at the previous call and stored in the  l o c a l  varia- 	*/
    /*  ble  ilo . a first check ascertains that  ilo .lt. lxt (this is nec- 	*/
    /*  essary since the present call may have nothing to do with the previ- 	*/
    /*  ous call). then, if  xt(ilo) .le. x .lt. xt(ilo+1), we set  left = 	*/
    /*  ilo  and are done after just three comparisons. 			*/
    /*     otherwise, we repeatedly double the difference  istep = ihi - ilo 	*/
    /*  while also moving  ilo  and  ihi  in the direction of  x , until 	*/
    /*                      xt(ilo) .le. x .lt. xt(ihi) , 			*/
    /*  after which we use bisection to get, in addition, ilo+1 = ihi . 	*/
    /*  left = ilo  is then returned. 						*/
    
    /* Parameter adjustments */
    --xt;
    
    /* Function Body */
    /*     save ilo  (a valid fortran statement in the new 1977 standard) */
    ihi = ilo + 1;
    if (ihi < *lxt) {
      goto L20;
    }
    if (*x >= xt[*lxt]) {
      goto L110;
    }
    if (*lxt <= 1) {
      goto L90;
    }
    ilo = *lxt - 1;
    ihi = *lxt;
    
  L20:
    if (*x >= xt[ihi]) {
      goto L40;
    }
    if (*x >= xt[ilo]) {
      goto L100;
    }
    
    /*              **** now x .lt. xt(ilo) . decrease  ilo  to capture  x . 
     */
    /* L30: */
    istep = 1;
  L31:
    ihi = ilo;
    ilo = ihi - istep;
    if (ilo <= 1) {
      goto L35;
    }
    if (*x >= xt[ilo]) {
      goto L50;
    }
    istep <<= 1;
    goto L31;
  L35:
    ilo = 1;
    if (*x < xt[1]) {
      goto L90;
    }
    goto L50;
    /*              **** now x .ge. xt(ihi) . increase  ihi  to capture  x . 
     */
  L40:
    istep = 1;
  L41:
    ilo = ihi;
    ihi = ilo + istep;
    if (ihi >= *lxt) {
      goto L45;
    }
    if (*x < xt[ihi]) {
      goto L50;
    }
    istep <<= 1;
    goto L41;
  L45:
    if (*x >= xt[*lxt]) {
      goto L110;
    }
    ihi = *lxt;
    
    /*           **** now xt(ilo) .le. x .lt. xt(ihi) . narrow the interval. 
     */
  L50:
    middle = (ilo + ihi) / 2;
    if (middle == ilo) {
      goto L100;
    }
    /*     note. it is assumed that middle = ilo in case ihi = ilo+1 . */
    if (*x < xt[middle]) {
      goto L53;
    }
    ilo = middle;
    goto L50;
  L53:
    ihi = middle;
    goto L50;
    /* **** set output and return. */
  L90:
    *mflag = -1;
    *left = 1;
    return 0;
  L100:
    *mflag = 0;
    *left = ilo;
    return 0;
  L110:
    *mflag = 1;
    *left = *lxt;
    return 0;
  } /* interv_ */

  
int
bsplvd_(double *t, int *k, double *x, int *left, double *a, double *dbiatx, int *nderiv)
{
  /* System generated locals */
  int  a_dim1, a_offset, dbiatx_dim1, dbiatx_offset, i_1, i_2, i_3;
  int c__1 = 1, c__2 = 2;
  
  /* Local variables */
  int  jlow, kp1mm, i, j, m, mhigh, jp1mid;
  double fkp1mm;
  int  il;
  double factor;
  int  ideriv;
  int  ldummy, kp1;
  double sum;

/* calls bsplvb */
/* calculates value and deriv.s of all b-splines which do not vanish at x */

/* ******  i n p u t  ****** */
/*  t     the knot array, of length left+k (at least) */
/*  k     the order of the b-splines to be evaluated */
/*  x     the point at which these values are sought */
/*  left  an int  indicating the left endpoint of the interval of */
/*        interest. the  k  b-splines whose support contains the interval */
/*               (t(left), t(left+1)) */
/*        are to be considered. */
/*  a s s u m p t i o n  - - -  it is assumed that */
/*               t(left) .lt. t(left+1) */
/*        division by zero will result otherwise (in  b s p l v b ). */
/*        also, the output is as advertised only if */
/*               t(left) .le. x .le. t(left+1) . */
/*  nderiv   an int  indicating that values of b-splines and their */
/*        derivatives up to but not including the  nderiv-th  are asked */

/*        for. ( nderiv  is replaced internally by the int  in (1,k) */

/*        closest to it.) */

/* ******  w o r k   a r e a  ****** */
/*  a     an array of order (k,k), to contain b-coeff.s of the derivat- */

/*        ives of a certain order of the  k  b-splines of interest. */

/* ******  o u t p u t  ****** */
/*  dbiatx   an array of order (k,nderiv). its entry  (i,m)  contains */
/*        value of  (m-1)st  derivative of  (left-k+i)-th  b-spline of */
/*        order  k  for knot sequence  t , i=m,...,k; m=1,...,nderiv. */

/* ******  m e t h o d  ****** */
/*  values at  x  of all the relevant b-splines of order k,k-1,..., */
/*  k+1-nderiv  are generated via  bsplvb  and stored temporarily */
/*  in  dbiatx .  then, the b-coeffs of the required derivatives of the */

/*  b-splines of interest are generated by differencing, each from the */
/*  preceding one of lower order, and combined with the values of b- */
/*  splines of corresponding order in  dbiatx  to produce the desired */
/*  values. */

    /* Parameter adjustments */
    dbiatx_dim1 = *k;
    dbiatx_offset = dbiatx_dim1 + 1;
    dbiatx -= dbiatx_offset;
    a_dim1 = *k;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --t;

    /* Function Body */
	/* Computing MAX */
    i_1 = min(*nderiv,*k);
    mhigh = max(i_1,1);
	/*     mhigh is usually equal to nderiv. */
    kp1 = *k + 1;
    i_1 = kp1 - mhigh;
    bsplvb_(&t[1], &i_1, &c__1, x, left, &dbiatx[dbiatx_offset]);
    if (mhigh == 1) goto L99;
/*     the first column of  dbiatx  always contains the b-spline values */
/*     for the current order. these are stored in column k+1-current */
/*     order  before  bsplvb  is called to put values for the next */
/*     higher order on top of it. */
    ideriv = mhigh;
    i_1 = mhigh;
    for (m = 2; m <= i_1; ++m) {
		jp1mid = 1;
		i_2 = *k;
		for (j = ideriv; j <= i_2; ++j) {
	    	dbiatx[j + ideriv * dbiatx_dim1] = dbiatx[jp1mid + dbiatx_dim1];
	   		++jp1mid;
		}
		--ideriv;
		i_2 = kp1 - ideriv;
		bsplvb_(&t[1], &i_2, &c__2, x, left, &dbiatx[dbiatx_offset]);
    }

/*     at this point,  b(left-k+i, k+1-j)(x) is in  dbiatx(i,j) for */
/*     i=j,...,k and j=1,...,mhigh ('=' nderiv). in particular, the */
/*     first column of  dbiatx  is already in final form. to obtain cor- */
/*     responding derivatives of b-splines in subsequent columns, gene- */
/*     rate their b-repr. by differencing, then evaluate at  x. */

    jlow = 1;
    i_1 = *k;
    for (i = 1; i <= i_1; ++i) {
		i_2 = *k;
		for (j = jlow; j <= i_2; ++j) {
	   		a[j + i * a_dim1] = 0.;
		}
		jlow = i;
		a[i + i * a_dim1] = 1.;
    }
/*     at this point, a(.,j) contains the b-coeffs for the j-th of the */
/*     k  b-splines of interest here. */

    i_1 = mhigh;
    for (m = 2; m <= i_1; ++m) {
		kp1mm = kp1 - m;
		fkp1mm = (double) kp1mm;
		il = *left;
		i = *k;

/*        for j=1,...,k, construct b-coeffs of  (m-1)st  derivative of */
/*        b-splines from those for preceding derivative by differencing */
/*        and store again in  a(.,j) . the fact that  a(i,j) = 0  for */
/*        i .lt. j  is used.sed. */
/*           the assumption that t(left).lt.t(left+1) makes denominator */
/*           in  factor  nonzero. */
		i_2 = kp1mm;
		for (ldummy = 1; ldummy <= i_2; ++ldummy) {
	 		factor = fkp1mm / (t[il + kp1mm] - t[il]);
	    	i_3 = i;
	    	for (j = 1; j <= i_3; ++j) {
			a[i + j * a_dim1] = (a[i + j * a_dim1] - a[i - 1 + j * a_dim1]) * factor;
	    }
	    --il;
	    --i;
	}

/*        for i=1,...,k, combine b-coeffs a(.,i) with b-spline values */
/*        stored in dbiatx(.,m) to get value of  (m-1)st  derivative of */
/*        i-th b-spline (of interest here) at  x , and store in */
/*        dbiatx(i,m). storage of this value over the value of a b-spline */
/*        of order m there is safe since the remaining b-spline derivat- */
/*        ive of the same order do not use this value due to the fact */
/*        that  a(j,i) = 0  for j .lt. i . */

	i_2 = *k;
	for (i = 1; i <= i_2; ++i) {
	    sum = (double)0.;
	    jlow = max(i,m);
	    i_3 = *k;
	    for (j = jlow; j <= i_3; ++j) {
			sum = a[j + i * a_dim1] * dbiatx[j + m * dbiatx_dim1] + sum;
	    }
	    dbiatx[i + m * dbiatx_dim1] = sum;
	}
    }
L99:
    return 0;
} /* bsplvd_ */

int
bsplvb_(double *t, int *jhigh, int *index, double *x, int *left, double *biatx)
{
    /* Initialized data */
    /* static */ int  j = 1;

    /* System generated locals */
    int  i_1;

    /* Local variables */
    double term;
    int  i;
    /* static */ double saved, deltal[20], deltar[20];
    int  jp1;

/* Calculates the value of all possibly nonzero b-splines at  x  of order */
/*               jout  =  dmax( jhigh , (j+1)*(index-1) ) */
/*  with knot sequence  t . */

/* ******  i n p u t  ****** */
/*  t.....knot sequence, of length  left + jout  , assumed to be nonde- */
/*        creasing.  a s s u m p t i o n . . . . */
/*                       t(left)  .lt.  t(left + 1)   . */
/*   d i v i s i o n  b y  z e r o  will result if  t(left) = t(left+1) */
/*  jhigh, */
/*  index.....integers which determine the order  jout = max(jhigh, 		*/
/*        (j+1)*(index-1))  of the b-splines whose values at  x  are to 	*/
/*        be returned.  index  is used to avoid recalculations when seve- 	*/
/*        ral columns of the triangular array of b-spline values are nee- 	*/
/*        ded (e.g., in  bvalue  or in  bsplvd ). precisely, 				*/
/*                     if  index = 1 , */
/*        the calculation starts from scratch and the entire triangular 	*/
/*        array of b-spline values of orders 1,2,...,jhigh  is generated 	*/
/*        order by order , i.e., column by column . 						*/
/*                     if  index = 2 , */
/*        only the b-spline values of order  j+1, j+2, ..., jout  are ge- 	*/
/*        nerated, the assumption being that  biatx , j , deltal , deltar 	*/
/*        are, on entry, as they were on exit at the previous call. 		*/
/*           in particular, if  jhigh = 0, then  jout = j+1, i.e., just 	*/
/*        the next column of b-spline values is generated. 					*/

/*  w a r n i n g . . .  the restriction   jout .le. jmax (= 20)  is im- 	*/
/*        posed arbitrarily by the dimension statement for  deltal  and 	*/
/*        deltar  below, but is  n o w h e r e  c h e c k e d  for . 		*/

/*  x.....the point at which the b-splines are to be evaluated. */
/*  left.....an int  chosen (usually) so that */
/*                  t(left) .le. x .le. t(left+1)  . */

/* ******  o u t p u t  ****** */
/*  biatx.....array of length  jout , with  biatx(i)  containing the val- */
/*        ue at  x  of the polynomial of order  jout  which agrees with */

/*        the b-spline  b(left-jout+i,jout,t)  on the interval (t(left), */
/*        t(left+1)) . */

/* ******  m e t h o d  ****** */
/*  the recurrence relation */

/*                       x - t(i)              t(i+j+1) - x */
/*     b(i,j+1)(x)  =  -----------b(i,j)(x) + ---------------b(i+1,j)(x) */
/*                     t(i+j)-t(i)            t(i+j+1)-t(i+1) */

/*  is used (repeatedly) to generate the (j+1)-vector  b(left-j,j+1)(x), */
/*  ...,b(left,j+1)(x)  from the j-vector  b(left-j+1,j)(x),..., */
/*  b(left,j)(x), storing the new values in  biatx  over the old. the */
/*  facts that */
/*            b(i,1) = 1  if  t(i) .le. x .lt. t(i+1) */
/*  and that */
/*            b(i,j)(x) = 0  unless  t(i) .le. x .lt. t(i+j) */
/*  are used. the particular organization of the calculations follows al- */
/*  gorithm  (8)  in chapter x of the text. */
/*     dimension biatx(jout), t(left+jout) */
/* urrent fortran standard makes it impossible to specify the length of */
/*  t  and of  biatx  precisely without the introduction of otherwise */
/*  superfluous additional arguments. */
    /* Parameter adjustments */
    --biatx;
    --t;

    /* Function Body */
	/*     save j,deltal,deltar (valid in fortran 77) */
	
    switch ((int)*index) {
	case 1:  goto L10;
	case 2:  goto L20;
    }
L10:
    j = 1;
    biatx[1] = 1.;
    if (j >= *jhigh) {
	goto L99;
    }

L20:
    jp1 = j + 1;
    deltar[j - 1] = t[*left + j] - *x;
    deltal[j - 1] = *x - t[*left + 1 - j];
    saved = 0.;
    i_1 = j;
    for (i = 1; i <= i_1; ++i)
	{	term = biatx[i] / (deltar[i - 1] + deltal[jp1 - i - 1]);
		biatx[i] = saved + deltar[i - 1] * term;
		saved = deltal[jp1 - i - 1] * term;
    }
    biatx[jp1] = saved;
    j = jp1;
    if (j < *jhigh) {
	goto L20;
    }

L99:
    return 0;
} /* bsplvb_ */



int
dpbfa_(double *abd, int *lda, int *n, int *m, int *info)
{
    /* System generated locals */
    int  abd_dim1, abd_offset, i_1, i_2, i_3;

    /* Local variables */
    //  MW  extern double ddot();
    int  j, k;
    double s, t;
    int  ik, jk, mu;


/*     dpbfa factors a double precision symmetric positive definite */
/*     matrix stored in band form. */

/*     dpbfa is usually called by dpbco, but it can be called */
/*     directly with a saving in time if  rcond  is not needed. */

/*     on entry */

/*        abd     double precision(lda, n) */
/*                the matrix to be factored.  the columns of the upper */
/*                triangle are stored in the columns of abd and the */
/*                diagonals of the upper triangle are stored in the */
/*                rows of abd .  see the comments below for details. */

/*        lda     int  */
/*                the leading dimension of the array  abd . */
/*                lda must be .ge. m + 1 . */

/*        n       int  */
/*                the order of the matrix  a . */

/*        m       int  */
/*                the number of diagonals above the main diagonal. */
/*                0 .le. m .lt. n . */

/*     on return */

/*        abd     an upper triangular matrix  r , stored in band */
/*                form, so that  a = trans(r)*r . */

/*        info    int  */
/*                = 0  for normal return. */
/*                = k  if the leading minor of order  k  is not */
/*                     positive definite. */

/*     band storage */

/*           if  a  is a symmetric positive definite band matrix, */
/*           the following program segment will set up the input. */

/*                   m = (band width above diagonal) */
/*                   do 20 j = 1, n */
/*                      i1 = max0(1, j-m) */
/*                      do 10 i = i1, j */
/*                         k = i-j+m+1 */
/*                         abd(k,j) = a(i,j) */
/*                10    continue */
/*                20 continue */

/*     linpack.  this version dated 08/14/78 . */
/*     cleve moler, university of new mexico, argonne national lab. */

/*     subroutines and functions */

/*     blas ddot */
/*     fortran max0,dsqrt */

/*     internal variables */

/*     begin block with ...exits to 40 */


    /* Parameter adjustments */
    abd_dim1 = *lda;
    abd_offset = abd_dim1 + 1;
    abd -= abd_offset;

    /* Function Body */
    i_1 = *n;
    for (j = 1; j <= i_1; ++j) {
	*info = j;
	s = 0.;
	ik = *m + 1;
	/* Computing MAX */
	i_2 = j - *m;
	jk = max(i_2,1);
	/* Computing MAX */
	i_2 = *m + 2 - j;
	mu = max(i_2,1);
	if (*m < mu) { goto L20;}
	i_2 = *m;
	for (k = mu; k <= i_2; ++k) {
	    i_3 = k - mu;
	    t = abd[k + j * abd_dim1] - ddot(i_3, &abd[ik + jk * abd_dim1], 
		    &abd[mu + j * abd_dim1]);
	    t /= abd[*m + 1 + jk * abd_dim1];
	    abd[k + j * abd_dim1] = t;
	    s += t * t;
	    --ik;
	    ++jk;
	}
L20:
	s = abd[*m + 1 + j * abd_dim1] - s;
/*     ......exit */
	if (s <= 0.) { goto L40; }
	abd[*m + 1 + j * abd_dim1] = sqrt(s);
    }
    *info = 0;
L40:
    return 0;
} /* dpbfa_ */

int
dpbsl_(double *abd, int *lda, int * n, int *m, double *b)
{
    /* System generated locals */
    int  abd_dim1, abd_offset, i_1, i_2;

    /* Local variables */
    // MW extern double ddot();
    int  k;
    double t;
    // MW extern void daxpy();
    int  kb, la, lb, lm;


/*     dpbsl solves the double precision symmetric positive definite */
/*     band system  a*x = b */
/*     using the factors computed by dpbco or dpbfa. */

/*     on entry */

/*        abd     double precision(lda, n) */
/*                the output from dpbco or dpbfa. */

/*        lda     int  */
/*                the leading dimension of the array  abd . */

/*        n       int  */
/*                the order of the matrix  a . */

/*        m       int  */
/*                the number of diagonals above the main diagonal. */

/*        b       double precision(n) */
/*                the right hand side vector. */

/*     on return */

/*        b       the solution vector  x . */

/*     error condition */

/*        a division by zero will occur if the input factor contains */
/*        a zero on the diagonal.  technically this indicates */
/*        singularity but it is usually caused by improper subroutine */
/*        arguments.  it will not occur if the subroutines are called */
/*        correctly and  info .eq. 0 . */

/*     to compute  inverse(a) * c  where  c  is a matrix */
/*     with  p  columns */
/*           call dpbco(abd,lda,n,rcond,z,info) */
/*           if (rcond is too small .or. info .ne. 0) go to ... */
/*           do 10 j = 1, p */
/*              call dpbsl(abd,lda,n,c(1,j)) */
/*        10 continue */

/*     linpack.  this version dated 08/14/78 . */
/*     cleve moler, university of new mexico, argonne national lab. */

/*     subroutines and functions */

/*     blas daxpy,ddot */
/*     fortran min0 */

/*     internal variables */


/*     solve trans(r)*y = b */

    /* Parameter adjustments */
    --b;
    abd_dim1 = *lda;
    abd_offset = abd_dim1 + 1;
    abd -= abd_offset;

    /* Function Body */
    i_1 = *n;
    for (k = 1; k <= i_1; ++k) {
/* Computing MIN */
	i_2 = k - 1;
	lm = min(i_2,*m);
	la = *m + 1 - lm;
	lb = k - lm;
	t = ddot(lm, &abd[la + k * abd_dim1], &b[lb]);
	b[k] = (b[k] - t) / abd[*m + 1 + k * abd_dim1];
/* L10: */
    }

/*     solve r*x = y */

    i_1 = *n;
    for (kb = 1; kb <= i_1; ++kb) {
	k = *n + 1 - kb;
/* Computing MIN */
	i_2 = k - 1;
	lm = min(i_2,*m);
	la = *m + 1 - lm;
	lb = k - lm;
	b[k] /= abd[*m + 1 + k * abd_dim1];
	t = -b[k];
	daxpy(lm, &t, &abd[la + k * abd_dim1], &b[lb]);
/* L20: */
    }
    return 0;
} /* dpbsl_ */



void
qsbart(double *dblParms, int *lngParms, double *xs, double *ys, double *ws,
       double *knot, double *coef, double *sz, double *lev,
       int *iparms, double *scrtch)
{
  /* Former parameters */
  double *penalt, *dofoff,  *crit, *lambda;
  int  *n, *nk, *isetup, *ld4, *ldnk, *ier;
  
  /* Uncode bundled parameters */
  penalt = &dblParms[0];
  dofoff = &dblParms[1];
  crit   = &dblParms[2];
  lambda = &dblParms[3];
  
  n      = &lngParms[0];
  nk     = &lngParms[1];
  isetup = &lngParms[2];
  ld4    = &lngParms[3];
  ldnk   = &lngParms[4];
  ier    = &lngParms[5];
  
  /* Parameter adjustments */
  --scrtch;
  --iparms;
  --lev;
  --sz;
  --coef;
  --knot;
  --ws;
  --ys;
  --xs;

    /* Former Function Call (with 35 args!!!)
    sbart_(penalt, dofoff, &xs[1], &ys[1], &ws[1], n, &knot[1], nk, &coef[1], 
	    &sz[1], &lev[1], crit, &iparms[1], lambda, &iparms[2], &parms[1], 
	    &parms[2], &parms[3], isetup, &scrtch[1], &scrtch[*nk + 1], &
	    scrtch[(*nk << 1) + 1], &scrtch[*nk * 3 + 1], &scrtch[(*nk << 2) 
	    + 1], &scrtch[*nk * 5 + 1], &scrtch[*nk * 6 + 1], &scrtch[*nk * 7 
	    + 1], &scrtch[(*nk << 3) + 1], &scrtch[*nk * 9 + 1], &scrtch[*nk *
	     9 + *ld4 * *nk + 1], &scrtch[*nk * 9 + (*ld4 << 1) * *nk + 1], 
	    ld4, ldnk, ier);
	 */

  sbart_(penalt, dofoff, &xs[1], &ys[1], &ws[1], n, &knot[1], nk, &coef[1], 
	    &sz[1], &lev[1], crit, &iparms[1], lambda, &iparms[2], isetup, &scrtch[1],
	    ld4, ldnk,ier);
	 
} /* qsbart_ */

int
sbart_(double *penalt, double *dofoff, double *xs, double *ys, double *ws, int *n,
       double *knot, int *nk, double *coef, double *sz,
       double *lev, double *crit, int *icrit, double *lambda, int *ispar, int *isetup,
       double *xwy, int *ld4, int *ldnk, int *ier)
{
    /* System generated locals */
    int  abd_dim1, abd_offset, p1ip_dim1, p1ip_offset, p2ip_dim1,  p2ip_offset, i_1;
    double d_1, c_b80 = 16.0;
    
    /* RAS set parms manually for finding lambda */
    double *lspar, *uspar, *tol, lo=0.0,up= 1.5,t=1e-8;    // May need to lower the tolerance t in order to get desired leverage
    double *hs0, *hs1, *hs2, *hs3, *sg0, *sg1, *sg2, *sg3, *abd, *p1ip, *p2ip;
    double *sBlock[4], *hBlock[4];
	    
    /* Local variables */
    double a, b, c, d, e;
    int  i;
    double p, q, r, u (0.0), v, w, x;
    double ratio (0.0), t1 (0.0), t2 (0.0);
    double ax, fu, fv, fw, fx, bx, xm;
    double eps, tol1, tol2;

    /* RAS values for former parameters */
    lspar = &lo;
    uspar = &up;
    tol   = &t;
 	hBlock[0] = hs0 = &xwy[*nk     + 1]; 
	hBlock[1] = hs1 = &xwy[*nk * 2 + 1];
	hBlock[2] = hs2 = &xwy[*nk * 3 + 1];
	hBlock[3] = hs3 = &xwy[*nk * 4 + 1];
	sBlock[0] = sg0 = &xwy[*nk * 5 + 1];
	sBlock[1] = sg1 = &xwy[*nk * 6 + 1];
	sBlock[2] = sg2 = &xwy[*nk * 7 + 1];
	sBlock[3] = sg3 = &xwy[*nk * 8 + 1];
	            abd = &xwy[*nk * 9 + 1];
	            p1ip= &xwy[*nk * 9 + *ld4       * *nk + 1];
	            p2ip= &xwy[*nk * 9 + (*ld4 * 2) * *nk + 1]; 
    
    /* Parameter adjustments */    
    p2ip_dim1 = *ldnk;
    p2ip_offset = p2ip_dim1 + 1;
    p2ip -= p2ip_offset;
    p1ip_dim1 = *ld4;
    p1ip_offset = p1ip_dim1 + 1;
    p1ip -= p1ip_offset;
    abd_dim1 = *ld4;
    abd_offset = abd_dim1 + 1;
    abd -= abd_offset;
    --sg3;
    --sg2;
    --sg1;
    --sg0;
    --hs3;
    --hs2;
    --hs1;
    --hs0;
    --xwy;
    --lev;
    --sz;
    --coef;
    --knot;
    --ws;
    --ys;
    --xs;

    /* Function Body */
    i = 1;
L23000:
    if (! (i <= *n)) {goto L23002;}
    if (! (ws[i] > 0.)) {goto L23003;}
    ws[i] = sqrt(ws[i]);
L23003:
    ++i;
    goto L23000;
L23002:
    if (! (*isetup == 0)) {goto L23005;}
    sgram_(&sg0[1], &sg1[1], &sg2[1], &sg3[1], &knot[1], nk);
    stxwx_(&xs[1], &ys[1], &ws[1], n, &knot[1], nk, &xwy[1], &hs0[1], &hs1[1],
	     &hs2[1], &hs3[1]);
    t1 = (double)0.;
    t2 = (double)0.;
    i_1 = *nk - 3;
    for (i = 3; i <= i_1; ++i)
		t1 += hs0[i];
    i_1 = *nk - 3;
    for (i = 3; i <= i_1; ++i)
		t2 += sg0[i];
 	ratio = t1 / t2;
    *isetup = 1;
L23005:
    if (! (*ispar == 1)) { goto L23011; }
    sslvrg_(penalt, dofoff, &xs[1], &ys[1], &ws[1], n, &knot[1], nk, &coef[1],
	     &sz[1], &lev[1], crit, icrit, lambda, &xwy[1], hBlock, sBlock, 
	     &abd[abd_offset], &p1ip[p1ip_offset], &p2ip[p2ip_offset], ld4, ldnk, 
	    ier);
    return 0;
L23011:
    ax = *lspar;
    bx = *uspar;
    c = ((double)3. - sqrt(5.)) * (double).5;
    eps = 1.;
L10:
    eps /= 2.;
    tol1 = eps + 1.;
    if (! (tol1 > 1.)) {goto L23013;}
    goto L10;
L23013:
    eps = sqrt(eps);
    a = ax;
    b = bx;
    v = a + c * (b - a);
    w = v;
    x = v;
    e = (double)0.;
    d_1 = x * (double)6. - (double)2.;
    *lambda = ratio * pow_dd(&c_b80, &d_1);
    sslvrg_(penalt, dofoff, &xs[1], &ys[1], &ws[1], n, &knot[1], nk, &coef[1],
	     &sz[1], &lev[1], crit, icrit, lambda, &xwy[1], hBlock, sBlock, &abd[
	    abd_offset], &p1ip[p1ip_offset], &p2ip[p2ip_offset], ld4, ldnk, 
	    ier);
    fx = *crit;
    fv = fx;
    fw = fx;
L20:
    xm = (a + b) * (double).5;
    tol1 = eps * abs(x) + *tol / 3.;
    tol2 = tol1 * 2.;
    /* if (! ((d_1 = x - xm, abs(d_1)) <= tol2 - (b - a) * (double).5)) { */
    d_1 = x - xm;
    if (abs(d_1) > tol2 - (b - a) * (double).5) { goto L23015; }
    goto L90;
L23015:
    if (! (abs(e) <= tol1)) {
	goto L23017;
    }
    goto L40;
L23017:
    r = (x - w) * (fx - fv);
    q = (x - v) * (fx - fw);
    p = (x - v) * q - (x - w) * r;
    q = (q - r) * (double)2.;
    if (! (q > 0.)) {
	goto L23019;
    }
    p = -p;
L23019:
    q = abs(q);
    r = e;
    e = d;
    if (! (abs(p) >= (d_1 = q * (double).5 * r, abs(d_1)))) {
	goto L23021;
    }
    goto L40;
L23021:
    if (! (p <= q * (a - x))) {
	goto L23023;
    }
    goto L40;
L23023:
    if (! (p >= q * (b - x))) {
	goto L23025;
    }
    goto L40;
L23025:
    d = p / q;
    u = x + d;
    if (! (u - a < tol2)) {
	goto L23027;
    }
    d_1 = xm - x;
    d = d_sign(&tol1, &d_1);
L23027:
    if (! (b - u < tol2)) {
	goto L23029;
    }
    d_1 = xm - x;
    d = d_sign(&tol1, &d_1);
L23029:
    goto L50;
L40:
    if (! (x >= xm)) {
	goto L23031;
    }
    e = a - x;
L23031:
    if (! (x < xm)) {
	goto L23033;
    }
    e = b - x;
L23033:
    d = c * e;
L50:
    if (! (abs(d) >= tol1)) {
	goto L23035;
    }
    u = x + d;
L23035:
    if (! (abs(d) < tol1)) {
	goto L23037;
    }
    u = x + d_sign(&tol1, &d);
L23037:
    d_1 = u * (double)6. - (double)2.;
    *lambda = ratio * pow_dd(&c_b80, &d_1);
    sslvrg_(penalt, dofoff, &xs[1], &ys[1], &ws[1], n, &knot[1], nk, &coef[1],
	     &sz[1], &lev[1], crit, icrit, lambda, &xwy[1], hBlock, sBlock, &abd[
	    abd_offset], &p1ip[p1ip_offset], &p2ip[p2ip_offset], ld4, ldnk, 
	    ier);
    fu = *crit;
    if (! (fu > fx)) {
	goto L23039;
    }
    goto L60;
L23039:
    if (! (u >= x)) {
	goto L23041;
    }
    a = x;
L23041:
    if (! (u < x)) {
	goto L23043;
    }
    b = x;
L23043:
    v = w;
    fv = fw;
    w = x;
    fw = fx;
    x = u;
    fx = fu;
    goto L20;
L60:
    if (! (u < x)) {
	goto L23045;
    }
    a = u;
L23045:
    if (! (u >= x)) {
	goto L23047;
    }
    b = u;
L23047:
    if (! (fu <= fw)) {
	goto L23049;
    }
    goto L70;
L23049:
    if (! (w == x)) {
	goto L23051;
    }
    goto L70;
L23051:
    if (! (fu <= fv)) {
	goto L23053;
    }
    goto L80;
L23053:
    if (! (v == x)) {
	goto L23055;
    }
    goto L80;
L23055:
    if (! (v == w)) {
	goto L23057;
    }
    goto L80;
L23057:
    goto L20;
L70:
    v = w;
    fv = fw;
    w = u;
    fw = fu;
    goto L20;
L80:
    v = u;
    fv = fu;
    goto L20;
L90:
    d_1 = x * (double)6. - (double)2.;
    *lambda = ratio * pow_dd(&c_b80, &d_1);
    *crit = fx;
    return 0;
} /* sbart_ */

int
sgram_(double *sg0, double *sg1, double *sg2, double *sg3, double *tb, int *nb)
{
    /* System generated locals */
    int  i_1, i_2, c__4 = 4,c__3 = 3;

    /* Local variables */
    double work[16];
    int  i, mflag, ileft;
    double vnikx[12]	/* was [4][3] */;
    int  ii, jj;
    double yw1[4], yw2[4];
    int  ilo;
    double wpt;

    /* Parameter adjustments */
    --tb;
    --sg3;
    --sg2;
    --sg1;
    --sg0;

    /* Function Body */
    i_1 = *nb;
    for (i = 1; i <= i_1; ++i) {
		sg0[i] = (double)0.;
		sg1[i] = (double)0.;
		sg2[i] = (double)0.;
		sg3[i] = (double)0.;
    }
    ilo = 1;
    i_1 = *nb;
    for (i = 1; i <= i_1; ++i) {
		i_2 = *nb + 1;
		interv_(&tb[1], &i_2, &tb[i], &ileft, &mflag);
		bsplvd_(&tb[1], &c__4, &tb[i], &ileft, work, vnikx, &c__3);
		for (ii = 1; ii <= 4; ++ii) {
	   		yw1[ii - 1] = vnikx[ii + 7];
		}
		bsplvd_(&tb[1], &c__4, &tb[i + 1], &ileft, work, vnikx, &c__3);
		for (ii = 1; ii <= 4; ++ii) {
	    	yw2[ii - 1] = vnikx[ii + 7] - yw1[ii - 1];
		}
		wpt = tb[i + 1] - tb[i];
		if (! (ileft >= 4)) {  goto L23008; }
		for (ii = 1; ii <= 4; ++ii) {
	   		jj = ii;
	    	sg0[ileft - 4 + ii] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (yw2[ii 
		 		- 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1]) * (double)
		  		.5 + yw2[ii - 1] * yw2[jj - 1] * (double).333);
	    	jj = ii + 1;
	    	if (! (jj <= 4)) { goto L23012; }
	    	sg1[ileft + ii - 4] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (yw2[ii 
		    	- 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1]) * (double)
		    	.5 + yw2[ii - 1] * yw2[jj - 1] * (double).333);
L23012:
	    	jj = ii + 2;
	    	if (! (jj <= 4)) { goto L23014;}
	    	sg2[ileft + ii - 4] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (yw2[ii 
		    	- 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1]) * (double)
		    	.5 + yw2[ii - 1] * yw2[jj - 1] * (double).333);
L23014:
	    	jj = ii + 3;
	    	if (! (jj <= 4)) {goto L23016;}
	    	sg3[ileft + ii - 4] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (yw2[ii 
		    	- 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1]) * (double)
		    	.5 + yw2[ii - 1] * yw2[jj - 1] * (double).333);
L23016:		;
		}
	goto L23009;
L23008:
		if (! (ileft == 3)) {goto L23018;}
		for (ii = 1; ii <= 3; ++ii) {
	    	jj = ii;
	    	sg0[ileft - 3 + ii] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (yw2[ii 
		   		- 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1]) * (double)
		    	.5 + yw2[ii - 1] * yw2[jj - 1] * (double).333);
	    	jj = ii + 1;
	    	if (! (jj <= 3)) goto L23022;
	    	sg1[ileft + ii - 3] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (yw2[ii 
		    	- 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1]) * (double)
		    	.5 + yw2[ii - 1] * yw2[jj - 1] * (double).333);
L23022:
	    	jj = ii + 2;
	    	if (! (jj <= 3)) goto L23024;
	    	sg2[ileft + ii - 3] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (yw2[ii 
		    	- 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1]) * (double)
		    	.5 + yw2[ii - 1] * yw2[jj - 1] * (double).333);
L23024: ;
		}
		goto L23019;
L23018:
		if (! (ileft == 2)) goto L23026;
		for (ii = 1; ii <= 2; ++ii) {
	    	jj = ii;
	    	sg0[ileft - 2 + ii] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (yw2[ii 
		    	- 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1]) * (double)
		    	.5 + yw2[ii - 1] * yw2[jj - 1] * (double).333);
	    	jj = ii + 1;
	    	if (! (jj <= 2)) goto L23030;
	    	sg1[ileft + ii - 2] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (yw2[ii 
		    - 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1]) * (double)
		    .5 + yw2[ii - 1] * yw2[jj - 1] * (double).333);
L23030:	;
		}
		goto L23027;
L23026:
		if (! (ileft == 1)) goto L23032;
		for (ii = 1; ii <= 1; ++ii) {
	    	jj = ii;
	    	sg0[ileft - 1 + ii] += wpt * (yw1[ii - 1] * yw1[jj - 1] + (yw2[ii 
		    	- 1] * yw1[jj - 1] + yw2[jj - 1] * yw1[ii - 1]) * (double)
		    	.5 + yw2[ii - 1] * yw2[jj - 1] * (double).333);
		}
L23032:
L23027:
L23019:
L23009: ;
    }
    return 0;
} /* sgram_ */

int
sinerp_(double *abd, int *ld4, int *nk, double *p1ip, double *p2ip, int *ldnk, int *flag_)
{
    /* System generated locals */
    int  abd_dim1, abd_offset, p1ip_dim1, p1ip_offset, p2ip_dim1, 
	    p2ip_offset, i_1;
    double d_1, d_2, d_3, d_4;

    /* Local variables */
    int  i, j, k;
    double c0 (0.0), c1 (0.0), c2 (0.0), c3 (0.0), wjm1[1], wjm2[2], wjm3[3];

    /* Parameter adjustments */
    p2ip_dim1 = *ldnk;
    p2ip_offset = p2ip_dim1 + 1;
    p2ip -= p2ip_offset;
    p1ip_dim1 = *ld4;
    p1ip_offset = p1ip_dim1 + 1;
    p1ip -= p1ip_offset;
    abd_dim1 = *ld4;
    abd_offset = abd_dim1 + 1;
    abd -= abd_offset;

    /* Function Body */
    wjm3[0] = 0.;
    wjm3[1] = 0.;
    wjm3[0] = 0.;
    wjm2[0] = 0.;
    wjm2[1] = 0.;
    wjm1[0] = 0.;
    i_1 = *nk;
    for (i = 1; i <= i_1; ++i) {
	j = *nk - i + 1;
	c0 = 1. / abd[j * abd_dim1 + 4];
	if (! (j <= *nk - 3)) {
	    goto L23002;
	}
	c1 = abd[(j + 3) * abd_dim1 + 1] * c0;
	c2 = abd[(j + 2) * abd_dim1 + 2] * c0;
	c3 = abd[(j + 1) * abd_dim1 + 3] * c0;
	goto L23003;
L23002:
	if (! (j == *nk - 2)) {
	    goto L23004;
	}
	c1 = 0.;
	c2 = abd[(j + 2) * abd_dim1 + 2] * c0;
	c3 = abd[(j + 1) * abd_dim1 + 3] * c0;
	goto L23005;
L23004:
	if (! (j == *nk - 1)) {
	    goto L23006;
	}
	c1 = 0.;
	c2 = 0.;
	c3 = abd[(j + 1) * abd_dim1 + 3] * c0;
	goto L23007;
L23006:
	if (! (j == *nk)) {
	    goto L23008;
	}
	c1 = 0.;
	c2 = 0.;
	c3 = 0.;
L23008:
L23007:
L23005:
L23003:
	p1ip[j * p1ip_dim1 + 1] = 0. - (c1 * wjm3[0] + c2 * wjm3[1] + c3 * 
		wjm3[2]);
	p1ip[j * p1ip_dim1 + 2] = 0. - (c1 * wjm3[1] + c2 * wjm2[0] + c3 * 
		wjm2[1]);
	p1ip[j * p1ip_dim1 + 3] = 0. - (c1 * wjm3[2] + c2 * wjm2[1] + c3 * 
		wjm1[0]);
/* Computing 2nd power */
	d_1 = c0;
/* Computing 2nd power */
	d_2 = c1;
/* Computing 2nd power */
	d_3 = c2;
/* Computing 2nd power */
	d_4 = c3;
	p1ip[j * p1ip_dim1 + 4] = d_1 * d_1 + d_2 * d_2 * wjm3[0] + c1 * (
		float)2. * c2 * wjm3[1] + c1 * (double)2. * c3 * wjm3[2] + d_3 
		* d_3 * wjm2[0] + c2 * (double)2. * c3 * wjm2[1] + d_4 * d_4 * 
		wjm1[0];
	wjm3[0] = wjm2[0];
	wjm3[1] = wjm2[1];
	wjm3[2] = p1ip[j * p1ip_dim1 + 2];
	wjm2[0] = wjm1[0];
	wjm2[1] = p1ip[j * p1ip_dim1 + 3];
	wjm1[0] = p1ip[j * p1ip_dim1 + 4];
/* L23000: */
    }
    if (! (*flag_ == 0)) {
	goto L23010;
    }
    return 0;
L23010:
    i_1 = *nk;
    for (i = 1; i <= i_1; ++i) {
	j = *nk - i + 1;
	k = 1;
L23014:
	if (! (k <= 4 && j + k - 1 <= *nk)) {
	    goto L23016;
	}
	p2ip[j + (j + k - 1) * p2ip_dim1] = p1ip[5 - k + j * p1ip_dim1];
	++k;
	goto L23014;
L23016:
	;
    }
    i_1 = *nk;
    for (i = 1; i <= i_1; ++i) {
	j = *nk - i + 1;
	k = j - 4;
L23019:
	if (! (k >= 1)) {
	    goto L23021;
	}
	c0 = (double)1. / abd[k * abd_dim1 + 4];
	c1 = abd[(k + 3) * abd_dim1 + 1] * c0;
	c2 = abd[(k + 2) * abd_dim1 + 2] * c0;
	c3 = abd[(k + 1) * abd_dim1 + 3] * c0;
	p2ip[k + j * p2ip_dim1] = 0. - (c1 * p2ip[k + 3 + j * p2ip_dim1] + c2 
		* p2ip[k + 2 + j * p2ip_dim1] + c3 * p2ip[k + 1 + j * 
		p2ip_dim1]);
	--k;
	goto L23019;
L23021:
/* L23017: */
	;
    }
    return 0;
/* L23011: */
} /* sinerp_ */


void
sknot(double *x, int *n, double *knot, int *k, int *allk)
{
    /* System generated locals */
    int  i_1;
    double d_1, c_b161 = 2.0, c_b170 = 0.2;

    /* Local variables */
    int  j (0), ndk (0);
    double a1, a2, a3, a4;

    /* Parameter adjustments */
    --knot;
    --x;

    /* Function Body */
    a1 = log(50.) / log(2.);
    a2 = log(100.) / log(2.);
    a3 = log(140.) / log(2.);
    a4 = log(200.) / log(2.);
    if (! (*n < 50 || *allk)) {
	goto L23000;
    }
    ndk = *n;
    goto L23001;
L23000:
    if (! (*n >= 50 && *n < 200)) {
	goto L23002;
    }
    d_1 = a1 + (a2 - a1) * (*n - (double)50.) / (double)150.;
    ndk = (int) pow_dd(&c_b161, &d_1);
    goto L23003;
L23002:
    if (! (*n >= 200 && *n < 800)) {
	goto L23004;
    }
    d_1 = a2 + (a3 - a2) * (*n - (double)200.) / (double)600.;
    ndk = (int) pow_dd(&c_b161, &d_1);
    goto L23005;
L23004:
    if (! (*n >= 800 && *n < 3200)) {
	goto L23006;
    }
    d_1 = a3 + (a4 - a3) * (*n - (double)800.) / (double)2400.;
    ndk = (int) pow_dd(&c_b161, &d_1);
    goto L23007;
L23006:
    if (! (*n >= 3200)) {
	goto L23008;
    }
    d_1 = (double) (*n - 3200);
    ndk = (int) (pow_dd(&d_1, &c_b170) + (double)200.);
L23008:
L23007:
L23005:
L23003:
L23001:
    *k = ndk + 6;
    for (j = 1; j <= 3; ++j)
      knot[j] = x[1];
    i_1 = ndk;
    for (j = 1; j <= i_1; ++j)
      knot[j + 3] = x[(j - 1) * (*n - 1) / (ndk - 1) + 1];
    for (j = 1; j <= 3; ++j)
      knot[ndk + 3 + j] = x[*n];
} /* sknotl_ */

int
sslvrg_(double *penalt, double *dofoff, double *x, double *y, double *w, int *n,
	double *knot, int *nk, double *coef, double *sz, 
	double *lev, double *crit, int *icrit, double *lambda, double *xwy, double *hBlock[], double *sBlock[],
	double *abd, double *p1ip, double *p2ip, int *ld4, int *ldnk, int *info)
{
    /* System generated locals */
    int  abd_dim1, abd_offset, p1ip_dim1, p1ip_offset, p2ip_dim1, 
	    p2ip_offset, i_1, i_2;
    double d_1, d_2, d_3, d_4, d_5;
	int c__0 = 0, c__1 = 1, c__4 = 4, c__3 = 3;
	double *hs0, *hs1, *hs2, *hs3, *sg0, *sg1, *sg2, *sg3;
	
    /* Local variables */
    double work[16], sumw;
    int  i, j;
    int  icoef, mflag, ileft;
    double b0, b1, b2, b3, vnikx[4], df, xv;
    int  ilo;
    double eps, rss;

    /* Parameter adjustments */
    p2ip_dim1 = *ldnk;
    p2ip_offset = p2ip_dim1 + 1;
    p2ip -= p2ip_offset;
    p1ip_dim1 = *ld4;
    p1ip_offset = p1ip_dim1 + 1;
    p1ip -= p1ip_offset;
    abd_dim1 = *ld4;
    abd_offset = abd_dim1 + 1;
    abd -= abd_offset;
    
    hs0 = hBlock[0]; /* RAS */
    hs1 = hBlock[1];
    hs2 = hBlock[2];
    hs3 = hBlock[3];
    sg0 = sBlock[0];
    sg1 = sBlock[1];
    sg2 = sBlock[2];
    sg3 = sBlock[3];
   
    --sg3;
    --sg2;
    --sg1;
    --sg0;
    --hs3;
    --hs2;
    --hs1;
    --hs0;
    --xwy;
    --lev;
    --sz;
    --coef;
    --knot;
    --w;
    --y;
    --x;

    /* Function Body */
    ilo = 1;
    eps = 1e-11;
    i_1 = *nk;
    for (i = 1; i <= i_1; ++i)
		coef[i] = xwy[i];
    i_1 = *nk;
    for (i = 1; i <= i_1; ++i)
		abd[i * abd_dim1 + 4] = hs0[i] + *lambda * sg0[i];
    i_1 = *nk - 1;
    for (i = 1; i <= i_1; ++i)
		abd[(i + 1) * abd_dim1 + 3] = hs1[i] + *lambda * sg1[i];
    i_1 = *nk - 2;
    for (i = 1; i <= i_1; ++i)
		abd[(i + 2) * abd_dim1 + 2] = hs2[i] + *lambda * sg2[i];
    i_1 = *nk - 3;
    for (i = 1; i <= i_1; ++i)
		abd[(i + 3) * abd_dim1 + 1] = hs3[i] + *lambda * sg3[i];
    dpbfa_(&abd[abd_offset], ld4, nk, &c__3, info);
    if (*info == 0) goto L23010;  /* RAS , from ! != */
    return 0;
L23010:
    dpbsl_(&abd[abd_offset], ld4, nk, &c__3, &coef[1]);
    icoef = 1;
    i_1 = *n;
    for (i = 1; i <= i_1; ++i) {
		xv = x[i];
		sz[i] = bvalue_(&knot[1], &coef[1], nk, &c__4, &xv, &c__0);
    }
    if (! (*icrit == 0))	goto L23014;
    return 0;
L23014:
    sinerp_(&abd[abd_offset], ld4, nk, &p1ip[p1ip_offset], &p2ip[p2ip_offset],
	     ldnk, &c__0);
    i_1 = *n;
    for (i = 1; i <= i_1; ++i) {
		xv = x[i];
		i_2 = *nk + 1;
		interv_(&knot[1], &i_2, &xv, &ileft, &mflag);
		if (! (mflag == -1)) goto L23018;
		ileft = 4;
		xv = knot[4] + eps;
L23018:
		if (! (mflag == 1))  goto L23020;
		ileft = *nk;
		xv = knot[*nk + 1] - eps;
L23020:
		j = ileft - 3;
		bsplvd_(&knot[1], &c__4, &xv, &ileft, work, vnikx, &c__1);
		b0 = vnikx[0];
		b1 = vnikx[1];
		b2 = vnikx[2];
		b3 = vnikx[3];
		/* Computing 2nd power */
		d_1 = b0;
		/* Computing 2nd power */
		d_2 = b1;
		/* Computing 2nd power */
		d_3 = b2;
		/* Computing 2nd power */
		d_4 = b3;
		/* Computing 2nd power */
		d_5 = w[i];
		lev[i] = (p1ip[j * p1ip_dim1 + 4] * (d_1 * d_1) + p1ip[j * p1ip_dim1 
			+ 3] * (double)2. * b0 * b1 + p1ip[j * p1ip_dim1 + 2] * (double)
			2. * b0 * b2 + p1ip[j * p1ip_dim1 + 1] * (double)2. * b0 * b3 
			+ p1ip[(j + 1) * p1ip_dim1 + 4] * (d_2 * d_2) + p1ip[(j + 1) *
			 p1ip_dim1 + 3] * (double)2. * b1 * b2 + p1ip[(j + 1) * 
			p1ip_dim1 + 2] * (double)2. * b1 * b3 + p1ip[(j + 2) * 
			p1ip_dim1 + 4] * (d_3 * d_3) + p1ip[(j + 2) * p1ip_dim1 + 3] *
			 (double)2. * b2 * b3 + p1ip[(j + 3) * p1ip_dim1 + 4] * (d_4 * 
			d_4)) * (d_5 * d_5);
    }
    if (! (*icrit == 1)) goto L23022;
    rss = 0.;
    df = 0.;
    sumw = 0.;
    i_1 = *n;
    for (i = 1; i <= i_1; ++i) {
		/* Computing 2nd power */
		d_1 = (y[i] - sz[i]) * w[i];
		rss += d_1 * d_1;
    }
    i_1 = *n;
    for (i = 1; i <= i_1; ++i)
		df += lev[i];
	/* Computing 2nd power */
    d_1 = 1. - (*dofoff + *penalt * df) / *n;
    *crit = rss / *n / (d_1 * d_1);
    goto L23023;
L23022:
    if (! (*icrit == 2))	goto L23028;
    *crit = 0.;
    i_1 = *n;
    for (i = 1; i <= i_1; ++i) {  /* Computing 2nd power */
		d_1 = (y[i] - sz[i]) * w[i] / (1 - lev[i]);
		*crit += d_1 * d_1;
    }
    *crit /= *n;
    goto L23029;
L23028:
    *crit = 0.;
    i_1 = *n;
    for (i = 1; i <= i_1; ++i)
		*crit += lev[i];
	/* Computing 2nd power */
    d_1 = *dofoff - *crit;
    *crit = d_1 * d_1 + 3;
L23029:
L23023:
    return 0;
} /* sslvrg_ */

int
stxwx_(double *x, double *z, double *w, int *k, double *xknot,
	     int  *n, double *y, double *hs0, double *hs1, double *hs2, double *hs3)
{
    /* System generated locals */
    int  i_1, i_2, c__4=4, c__1=1;
    double d_1, d_2;

    /* Local variables */
    double work[16];
    int  i, j, mflag, ileft;
    double vnikx[4]	/* was [4][1] */;
    int  ilo;
    double eps;

    /* Parameter adjustments */
    --hs3;
    --hs2;
    --hs1;
    --hs0;
    --y;
    --xknot;
    --w;
    --z;
    --x;
    
    /* Function Body */
    i_1 = *n;
    for (i = 1; i <= i_1; ++i)
	{	y[i] = 0.;
		hs0[i] = 0.;
		hs1[i] = 0.;
		hs2[i] = 0.;
		hs3[i] = 0.;
    }
    ilo = 1;
    eps = 1e-10;
    i_1 = *k;
    for (i = 1; i <= i_1; ++i) {
		i_2 = *n + 1;
		interv_(&xknot[1], &i_2, &x[i], &ileft, &mflag);
		if (! (mflag == 1)) {goto L23004;}
		if (! (x[i] <= xknot[ileft] + eps)) { return 0; }
		--ileft;
L23004:
		bsplvd_(&xknot[1], &c__4, &x[i], &ileft, work, vnikx, &c__1);
		j = ileft - 3;
			/* Computing 2nd power */
		d_1 = w[i];
		y[j] += d_1 * d_1 * z[i] * vnikx[0];
			/* Computing 2nd power */
		d_1 = w[i];
			/* Computing 2nd power */
		d_2 = vnikx[0];
		hs0[j] += d_1 * d_1 * (d_2 * d_2);
			/* Computing 2nd power */
		d_1 = w[i];
		hs1[j] += d_1 * d_1 * vnikx[0] * vnikx[1];
			/* Computing 2nd power */
		d_1 = w[i];
		hs2[j] += d_1 * d_1 * vnikx[0] * vnikx[2];
			/* Computing 2nd power */
		d_1 = w[i];
		hs3[j] += d_1 * d_1 * vnikx[0] * vnikx[3];
		j = ileft - 2;
			/* Computing 2nd power */
		d_1 = w[i];
		y[j] += d_1 * d_1 * z[i] * vnikx[1];
			/* Computing 2nd power */
		d_1 = w[i];
			/* Computing 2nd power */
		d_2 = vnikx[1];
		hs0[j] += d_1 * d_1 * (d_2 * d_2);
			/* Computing 2nd power */
		d_1 = w[i];
		hs1[j] += d_1 * d_1 * vnikx[1] * vnikx[2];
			/* Computing 2nd power */
		d_1 = w[i];
		hs2[j] += d_1 * d_1 * vnikx[1] * vnikx[3];
		j = ileft - 1;
			/* Computing 2nd power */
		d_1 = w[i];
		y[j] += d_1 * d_1 * z[i] * vnikx[2];
			/* Computing 2nd power */
		d_1 = w[i];
			/* Computing 2nd power */
		d_2 = vnikx[2];
		hs0[j] += d_1 * d_1 * (d_2 * d_2);
			/* Computing 2nd power */
		d_1 = w[i];
		hs1[j] += d_1 * d_1 * vnikx[2] * vnikx[3];
		j = ileft;
			/* Computing 2nd power */
		d_1 = w[i];
		y[j] += d_1 * d_1 * z[i] * vnikx[3];
			/* Computing 2nd power */
		d_1 = w[i];
			/* Computing 2nd power */
		d_2 = vnikx[3];
		hs0[j] += d_1 * d_1 * (d_2 * d_2);
    }
    return 0;
} /* stxwx_ */

