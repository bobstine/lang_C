/*
   6 Oct 99 ... Use a list of pointers, rather than one big space.
   4 Oct 99 ... Add trace function.
  27 Sep 99 ... Implement as list of column pointers.
  13 Nov 98 ... Complete implementation of sweep operations.
  11 Nov 98 ... Creation started.
	
*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "matrix.h"
#include "memory.h"

/********************************************************************************/

#define ExitOnNull(ptr,str,result) if(NULL==(ptr)){fprintf(stderr,"\nMATRIX: %s\n",(str));return (result);}

#define ExitOn(c, str, val) if (c){fprintf(stderr, "\nMATRIX: %s\n", (str)); return (val);}

#define CHECKDIM(m,i,j) (((i) < (m)->nRows) && ((j) < (m)->nCols)) ? 1 : 0

#define ABS(x) ((x) < 0) ? (-x) : (x)

#define TINY 1.0e-20


/*********************** Internal functions *************************************/ 

Matrix NewMatrix (long nRows, long nCols, char *creator)
{
  Matrix m;
  long j;
  
  m = (Matrix) Allocate ( sizeof(aMatrix), creator, "MATr" );
  ExitOnNull(m, "could not allocate", NULL);
  m->nRows = nRows;
  m->nCols = nCols;
  m->pCols = (double **) Allocate(nCols * sizeof (double *), creator, "MATp");
  ExitOnNull(m->pCols, "could not allocate column pointers.", NULL);
  for (j=0; j<nCols; ++j)
  {	m->pCols[j] =  (double *) Allocate (nRows * sizeof(double), creator, "MATc");
	ExitOnNull(m->pCols[j], "MATRIX: could not allocate column", NULL);
  }
  return m;
}

Matrix IdentityMatrix (long n, char *creator)
{
  Matrix iden;
  long i;
  
  ExitOnNull(iden = NewMatrix (n, n, creator), "Identity fails", NULL);
  InitializeMatrix(iden, 0.0);
  for (i=0; i<n; ++i)
    AssignMatrixElement(iden, i, i, 1.0);
  return (iden);
}

void   DeleteMatrix (Matrix m)
{
  long j;
  
  for (j=0; j < m->nCols; ++j)
    Release(m->pCols[j]);
  Release (m->pCols);		// points to array of col pointers
  Release (m);
}



Matrix
RestoreMatrixFromFile (FILE *file, int nPad, char *creator)
{
  Matrix m;
  double x;
  long nRows, nCols;
  int nRead;
  
  nRead = fscanf (file, "matrix %ld %ld\n", &nRows, &nCols);
  if (2 != nRead) printf ("MATRIX: Error reading matrix; got %d dims\n", nRead);
  printf ("Reading matrix with dim [%ld, %ld] from file...\n", nRows, nCols);
  m = NewMatrix(nRows, nCols+nPad, creator);
  for (long i=0; i<nRows; ++i)
  { for (long j=0; j<nCols; ++j)
    { fscanf(file, "%lf", &x);
      AssignMatrixElement(m, i,j,x);
    }
    for (long j=nCols; j<nCols+nPad; ++j)  // zero padded area
      AssignMatrixElement(m,i,j,0.0);
  }
  for (long i=nRows; i<nRows+nPad; ++i)
    for (long j=0; j<nCols+nPad; ++j)
      AssignMatrixElement(m,i,j,0.0);
  return m;
}
	
  
long
SaveMatrixToFile (Matrix m, FILE *file)
{
  fprintf (file, "matrix %ld %ld\n", m->nRows, m->nCols);
  for (long i=0; i<m->nRows; ++i)
  { for (long j=0; j<m->nCols; ++j)
      fprintf(file,"%f ", MatrixElement(m,i,j));
    fprintf(file,"\n");
  }
  return (m->nRows * m->nCols);
}



double	MatrixElement (Matrix m, long row, long col)
{
  if (CHECKDIM(m,row,col))
    return m->pCols[col][row];
  else
  { fprintf (stderr, "MATRIX: index error for row = %ld and col = %ld\n",
	     row, col);
    return 0.0;
  }
}

double * MatrixColumn (Matrix m, long col)
{
  ExitOn ((col >= m->nCols), "Column access error", NULL);
  return(m->pCols[col]);
}

void MatrixDimensions (Matrix m, long *nRows, long *nCols)
{
  *nRows = m->nRows;
  *nCols = m->nCols;
}


int InitializeMatrix	(Matrix m, double initialValue)
{
  long i,j;
  double *x;
  
  if (m)
  {
    for (j=0; j<m->nCols; ++j)
    {	x = m->pCols[j];
	for (i=0; i<m->nRows; ++i)
	  *x++ = initialValue;
    }
    return 1;
  }
  else return 0;
}


int AssignMatrixElement		(Matrix m, long row, long col, double x)
{
  if (m && CHECKDIM(m,row,col))
  { m->pCols[col][row] = x;
    return 1;
  } else
  { fprintf(stderr, 
	    "MATRIX: index error in assigning element at (%ld, %ld)\n", row, col);
    fprintf(stderr,
	    "      : dimension of array is [%ld, %ld]\n", m->nRows, m->nCols);
    return 0;
  }
}

int
InsertMatrix (Matrix src, Matrix dest)
{
  for (long i=0; i<src->nRows; ++i)
    for (long j=0; j<src->nCols; ++j)
      AssignMatrixElement(dest,i,j, MatrixElement(src,i,j));
  return 0;
}


int IncrementMatrixElement	(Matrix m, long row, long col, double x)
{
  if (m && CHECKDIM(m,row,col))
  { m->pCols[col][row] += x;
    return 1;
  } else
  { fprintf(stderr, 
	    "MATRIX: index error in incrementing element at (%ld, %ld)\n", row, col);
    return 0;
  }
}

int DecrementMatrixElement  (Matrix m, long row, long col, double x)
{
  if (m && CHECKDIM(m,row,col))
  { m->pCols[col][row] -= x;
    return 1;
  } else
  { fprintf(stderr, 
	    "MATRIX: index error in decrementing element at (%ld, %ld)\n", row, col);
    return 0;
  }
}


int DivideMatrixElement (Matrix m, long row, long col, double divisor)
{
  if (m && CHECKDIM(m,row,col) && divisor != 0.0)
  { m->pCols[col][row] /= divisor;
    return 1;
  } else
  { fprintf(stderr, 
	    "MATRIX: index error in dividing element at (%ld, %ld)\n", row, col);
    return 0;
  }
}

int MultiplyMatrixElement (Matrix m, long row, long col, double factor)
{
  if (m && CHECKDIM(m,row,col))
  { m->pCols[col][row] *= factor;
    return 1;
  } else
  { fprintf(stderr, 
	    "MATRIX: index error in multiplying element at (%ld, %ld)\n", row, col);
    return 0;
  }
}

int
MultiplyMatrixByConstant (Matrix m, double x)
{
  long i,j;
  for (i=0; i<m->nRows; ++i)
    for (j=0; j<m->nCols; ++j)
      m->pCols[j][i] *=  x;
  return 1;
}

  
// --------------------  Matrix Operations  ----------------------------------------


double	TraceOfMatrix (Matrix m)
{
  double trace;
  long i, limit;
  
  if (m->nRows == m->nCols)
    limit = m->nRows;
  else
  { fprintf (stderr, "MATRIX: trace of non-square matrix.\n");
    limit = (m->nRows<m->nCols) ? m->nRows : m->nCols;
  }
  for (i = 0, trace=0.0; i< limit; ++i)
    trace += MatrixElement(m,i,i);
  return(trace);
}

Matrix
MatrixProduct (Matrix a, Matrix b, char *creator)
{
  Matrix product;
  long i,j,k;
  double accum;
  
  if (a->nCols != b->nRows)
  {	fprintf (stderr, "MATRIX: not conformable for multiplication.\n");
	return NULL;
  }
  ExitOnNull(product = NewMatrix (a->nRows, b->nCols, creator), "cannot allocate product", NULL);
  for (i = 0; i<product->nRows; ++i)
    for (j=0; j < product->nCols; ++j)
    {	accum = 0.0;
	for (k=0; k < a->nCols; ++k)
	  accum += MatrixElement(a, i, k) * MatrixElement(b, k, j) ;
	AssignMatrixElement(product, i, j, accum);
    }
  return (product);
}

double
QuadraticForm (Matrix m, double *x)
{
  long dim = m->nRows;
  
  ExitOn((dim != m->nCols), "quadratic form needs sqr matrix", 0.0);
  double qf = 0.0;
  double **pM = m->pCols;
  for (long i=0; i<dim; ++i)
  { qf += x[i] * x[i] * pM[i][i];
    for (long j=i+1; j<dim; ++j)
      qf += 2.0 * x[i] * x[j] * pM[j][i];
  }
  return qf;
}

Matrix
TriangularQuadraticForm (Matrix u, Matrix m) // u m u', u up triagular
{
  Matrix prod;
  long dim = u->nRows;

  ExitOnNull(prod = NewMatrix(dim, dim, "TriagularQuad"),
	     "Could not allocate quadratic product", NULL);
  // form u m
  for (long row=0; row<dim; ++row)
  { for (long col=0; col<dim; ++col)
    { double sum = 0.0;
      for (long i=row; i<dim; ++i)
	sum += MatrixElement(u, row, i) * MatrixElement(m, i, col);
      AssignMatrixElement(prod,row,col,sum);
    }
  }
  // form (um) u'
  for (long row=0; row<dim; ++row)
  { for (long col=0; col<dim; ++col)
    { double sum = 0.0;
      for (long i=col; i<dim; ++i)
	sum += MatrixElement(prod, row, i) * MatrixElement(u, col, i);
      AssignMatrixElement(prod,row,col,sum);
    }
  }
  return prod;
}
  

Matrix
LUFactor (Matrix mat, long *permutation, double *parity, char *creator)
{
  Matrix a;
  long i,imax,j,k,n;
  double big,dum,sum,temp, *vv;
  
  ExitOn((mat->nRows != mat->nCols), "LU requires square matrix.", NULL);
  ExitOnNull(a = CopyMatrix(mat, creator), "could not allocate LU matrix", NULL);
  n = a->nRows;
  ExitOnNull(vv=new double[n],"could not alloc temp space", NULL);
  *parity=1.0;
  imax = 0;
  for (i=0; i<n; i++) 
  {	big=0.0;
	for (j=0; j<n; j++)
	  if ((temp=ABS(MatrixElement(a,i,j))) > big) big=temp;
	ExitOn((big == 0.0), "Singular matrix in LUfactor", NULL);
	vv[i] = 1.0/big;
  }
  for (j=0; j<n; j++)
  {	for (i=0; i<j; i++)
  {	sum=MatrixElement(a,i,j);
	for (k=0; k<i; k++)
	  sum -= MatrixElement(a,i,k) * MatrixElement(a,k,j);
	AssignMatrixElement(a,i,j,sum);
  }
	big=0.0;
	for (i=j; i<n; i++)  // fill other half of LU decomp
	{	sum=MatrixElement(a,i,j);
		for (k=0; k<j; k++)
		  sum -= MatrixElement(a,i,k) * MatrixElement(a,k,j);
		AssignMatrixElement(a, i, j, sum);
		if ( (dum=vv[i] * ABS(sum)) >= big) 
		{	big=dum;
			imax=i;
		}
	}
	if (j != imax) 
	{	for (k=0; k<n; k++)
	{	dum = MatrixElement(a, imax, k);
		AssignMatrixElement(a, imax, k, MatrixElement(a, j, k));
		AssignMatrixElement(a,j,k,dum);
	}
		*parity = -(*parity);
		vv[imax]=vv[j];
	}
	permutation[j] = imax;
	if (0.0 == MatrixElement(a,j,j)) 
	  AssignMatrixElement(a, j, j, TINY);
	if (j < n-1) 
	{	dum=1.0/MatrixElement(a, j, j);
		for (i=j+1; i < n; i++) 
		  MultiplyMatrixElement(a, i, j, dum);
	}
  }
  delete(vv);
  return (a);
}

void
LUSolve (Matrix a, long *permutation, double b[])
{
  long i,ii=-1,ip,j;
  double sum;
  
  for (i=0; i<a->nRows; i++) 
  {	ip=permutation[i];
	sum=b[ip];
	b[ip]=b[i];
	if (ii>=0)
	  for (j=ii; j<=i-1; j++) sum -= MatrixElement(a,i,j)*b[j];
	else if (sum) ii=i;
	b[i]=sum;
  }
  for (i=a->nRows-1; i>=0; i--) 
  {	sum=b[i];
	for (j=i+1; j<a->nRows; j++) 
	  sum -= MatrixElement(a, i, j)*b[j];
	b[i]=sum/MatrixElement(a,i,i);
  }
}

Matrix
InverseMatrix (Matrix m, char *creator)
{
  Matrix inverse, factor;
  long *permute, col;
  double parity;
  
  // set up space
  ExitOn( (m->nCols != m->nRows), "matrix is not square", NULL);
  ExitOnNull(permute = new long[m->nRows], "could not get index space", NULL);
  // factor input, then solve columns
  factor = LUFactor(m, permute, &parity, "InverseMatrix");
  inverse = IdentityMatrix (m->nRows, creator);
  for (col = 0; col < m->nCols; ++col)
    LUSolve(factor, permute, MatrixColumn(inverse, col));
  DeleteMatrix(factor);
  delete(permute);
  return (inverse);
}



// --------------------  Stepwise  ----------------------------------------

int ForwardStepwise (Matrix XpX, long nSteps, long *cols)
{
  double *partialCorr, maxCorr, absCorr;
  long  dim, i, bestX=0;
  int *haveNotSwept, ok;
  
  dim = XpX->nRows;
  partialCorr = new double[dim];
  haveNotSwept = new int[dim];
  for (i=1; i<dim; ++i)
    haveNotSwept[i] = 1;
  PrintMatrix(XpX, "Matrix: XpX prior to sweeping");
  while (nSteps--)
  {	maxCorr = 0.0;
	for (i=1; i<dim; ++i)
	{
	  if (haveNotSwept[i])
	  {	// dont div by sd of Y since in each, so partCorr may be > 1
	    partialCorr[i] = MatrixElement(XpX,0,i) / sqrt(MatrixElement(XpX,i,i));
	    absCorr = ABS(partialCorr[i]);
	    if (absCorr > maxCorr)
	    {	maxCorr = absCorr;
		bestX = i;
	    }
	  }
	}
	// printf ("\nSweeping on %ld\n", bestX);
	*cols++ = bestX;
	haveNotSwept[bestX] = 0;
	ok = ForwardSweepMatrix(XpX, bestX);		
	if ( !ok ) return -nSteps;
  }
  PrintMatrix(XpX, "Matrix: XpX after sweeping");
  delete(partialCorr);
  delete(haveNotSwept);
  return nSteps;
}


int
ReverseStepwise (Matrix XpX, long nSteps, long *cols)
{
  double *tStat, minTStat, absTStat;
  long  dim, i, worstX=0;
  int *haveSwept, ok;
  
  dim = XpX->nRows;
  tStat = new double[dim];
  // Assumes that all have been used in sweeping, now want to reverse
  haveSwept = new int[dim];
  for (i=1; i<dim; ++i)
    haveSwept[i] = 1;
  while (nSteps--)
  {	minTStat = 1.4E300;
	for (i=1; i<dim; ++i)
	{
	  if (haveSwept[i])
	  {	// dont need sd of resids since in each
	    tStat[i] = MatrixElement(XpX,0,i) / sqrt(-MatrixElement(XpX,i,i));
	    absTStat = ABS(tStat[i]);
	    if (absTStat < minTStat)
	    {	minTStat = absTStat;
		worstX = i;
	    }
	  }
	}
	printf ("\nMA: Reverse sweep on %ld\n", worstX);
	*cols++ = worstX;
	haveSwept[worstX] = 0;
	ok = ReverseSweepMatrix(XpX, worstX);
	if ( !ok ) return -nSteps;
  }
  PrintMatrix(XpX, "Matrix: XpX after reverse sweep");
  delete(tStat);
  delete(haveSwept);
  return nSteps;
}


int
SweepMatrix(Matrix m, long col, double pivot);  // negative pivot means reverse

#define PIVOT_TOLERANCE 1.0E-40

int ForwardSweepMatrix (Matrix m, long k)
{
  double pivot;
  
  pivot = MatrixElement(m, k, k);
  if (pivot < PIVOT_TOLERANCE)
  {	fprintf (stderr, 
		 "MATRIX: forward sweep pivot %e too close to zero.\n", pivot);
	return 0;
  } else
    return SweepMatrix(m, k, pivot);
}

int ReverseSweepMatrix (Matrix m, long k)
{
  double pivot;
  
  pivot = MatrixElement(m, k, k);
  if (-pivot < PIVOT_TOLERANCE)
  {	fprintf (stderr, 
		 "MATRIX: reverse sweep pivot %e too close to zero.\n", pivot);
	return 0;
  } else
    return SweepMatrix(m, k, pivot);
}


int
SweepMatrix(Matrix m, long k, double pivot)
{
  double factor;
  long dim, i,j;
  
  dim = m->nRows;
  for (j=0; j<dim; ++j)
    if (j != k)
    {	factor = MatrixElement(m,k,j)/pivot;
	for (i=0; i<dim; ++i)
	  if (i != k)
	    DecrementMatrixElement(m,i,j, factor * MatrixElement(m,i,k));
    }
  if (pivot < 0.0)  // reverse sign for reverse sweep
    factor = -pivot;
  else
    factor = pivot;
  for (j=0; j<dim; ++j)
  {	DivideMatrixElement(m,k,j, factor);
	DivideMatrixElement(m,j,k, factor);
  }
  AssignMatrixElement(m,k,k, -1.0/pivot);
  return 1;
}


Matrix
CopyMatrix (Matrix m, char *creator)
{
  long nRows, nCols, i, j;
  double *src, *dest;
  Matrix copy;
  
  MatrixDimensions(m, &nRows, &nCols);
  ExitOnNull(copy = NewMatrix(nRows, nCols, creator),
	     "MATRIX: unable to allocate copy.", NULL);
  for (j=0; j<m->nCols; ++j)
  {	src = m->pCols[j];
	dest = copy->pCols[j];
	for (i=0; i<m->nRows; ++i)
	  *dest++ = *src++;
  }
  return copy;
}


void
PrintMatrix(Matrix m, char *msg)
{
  long i,j;
  
  if (m)
  {	printf ("\n --- %s [%ld,%ld] ---\n", msg, m->nRows, m->nCols);
	for (i=0; i<m->nRows; ++i)
	{	printf("[%3ld] ", i);
		for (j=0; j<m->nCols; ++j)
		  printf("%8.4f ", MatrixElement(m,i,j));
		printf("\n");
	}
  } else
    printf("\nMATRIX:  cannot print null matrix.\n");
}



#undef TINY
#undef ExitOn
#undef ExitOnNull
