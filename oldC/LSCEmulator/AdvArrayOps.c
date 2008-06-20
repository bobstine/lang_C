/*					AdvArrayOps.c		19 Jul 89 ... Alter to nonprinting error codes.	14 Oct 87		*/#include "Globals.h"#include "Utils.h"#include "ArrayDef.h"#include "ElemArrayOps.h"#include "AdvArrayOps.h"#include <math.h>#include <string.h>#define ABS(X) ((X)>0.0?(X):-(X))#define MIN(A,B) ((A)<(B)?(A):(B))#define MORETHANZERO  1.0E-20void BackSub (r, dim, cb)double *r[];int dim;double *cb;{	double total;	int row, col, p;	p = dim - 1;	for (col=0; col< dim; ++col)	{	if (IsNearZero(r[col][col]))		{	SetErrorCode(2, "AAO: Diag near zero in backSub; reset.");			r[col][col] = MORETHANZERO;		}	}	cb[p] = cb[p] / r[p][p];	for (row = p-1; row>=0; --row)	{	total = cb[row];		for (col=row+1; col<dim; ++col)		{	total -= cb[col] * r[col][row];			cb[row] = total / r[row][row];		}	}}void FactorMatrix (mat, factor)Matrix *mat, *factor;{	if (mat->nRows != mat->nCols)		SetErrorCode (2, "AAO: Matrix to factor not square; using min");	if (mat->nRows > factor->nRows)		SetErrorCode (-1, "AAO: Factor destination too small");	else		FactorMat (mat->pntr, MIN(mat->nRows, mat->nCols), factor->pntr);}void FactorMat (mat, dim, factor)double *mat[], *factor[];int dim;{	int row, col,i;	double diag, total;	for (row=0; row<dim; ++row)	{	diag = mat[row][row];		for (i=0; i<row; ++i)			diag -= factor[row][i]*factor[row][i];		if (IsNegative(diag))		{	SetErrorCode(2,"AAO: Neg diag in factoring matrix; reset");			diag = MORETHANZERO;		} else			diag = sqrt(diag);		factor[row][row] = diag;		for (col=row+1; col<dim; ++col)		{   /*fill in rest of row*/			total = mat[col][row];			for (i=0; i<row; ++i)				total -= factor[col][i] * factor[row][i];			factor[col][row] = total / diag;		};	}}  /*  FactorMat   */void InvertTriMatrix (mat, inv)Matrix *mat, *inv;{	if ((mat->nRows != inv->nRows) ||		(mat->nCols != inv->nCols) ||		(mat->nRows != mat->nCols))		SetErrorCode (-4, "AAO: Dimension error in tri inverse");	else		InvertTriMat (mat->pntr, mat->nRows,inv->pntr);}void InvertTriMat (mat, dim, inv)double *mat[], *inv[];int dim;{ 	int row, col, k;	double diag, tot;	for (row=dim-1; row>-1; --row)	{ 	diag = mat[row][row];		if (IsNearZero(diag)) 		{	SetErrorCode (2,"AAO: Near zero in Invert Tri");			diag = MORETHANZERO;		}		inv[row][row] = 1.0 / diag;		for (col=row+1; col<dim; ++col)		{	tot = 0.0;			for (k=row; k<col; ++k)				tot -= inv[k][row] * mat[col][k];			inv[col][row] = tot / mat[col][col];		}	}}void GramSchmidt (x,nRows, nCols, findCoef, findRes, y,c,r,res)double *x[], *y,*c, *r[], *res;int nRows, nCols, findCoef, findRes;{	double	root;	int i, j, k;	if (findRes&&findCoef) 		CopyVec(y, res, nRows);	for (j=0; j<nCols; ++j)	{	root = sqrt (DotProdVec (x[j], x[j],nRows));		if (IsNearZero(root))		{	SetErrorCode(2, "AAO: Singularity in Gram Schmidt");			root = MORETHANZERO;		}		r[j][j] = root;		for (i=0; i<nRows; ++i)			x[j][i] /= root; 				/*norm col*/		for (k=j+1; k<nCols; ++k)		{	/*sweep from remaining cols*/			root = DotProdVec (x[j], x[k], nRows);			r[k][j] = root;			for (i=0; i<nRows; ++i)				x[k][i] -= x[j][i] * root;		}		if (findCoef)		{			c[j] = DotProdVec(x[j], y, nRows);			if (findRes)			{	root = c[j];				for (i=0; i<nRows; ++i)					res[i] -= root * x[j][i];			}		}	}	/* for j */}void SolveLinearSystem (x,y,b)Matrix *x;Vector *y,*b;{	SolveSys (x->pntr,  x->nRows, x->nCols, y->pntr, b->pntr);}void SolveSys (x,nRows,nCols, y,b)double *x[], *y,*b;int nRows, nCols;{	Matrix r, copyX;	Vector	res;	double *dummy;		/* dont calc resids in GS	*/	AllocMatrix(nCols, nCols, &r);	AllocMatrix(nRows, nCols, &copyX);	CopyMat (x, copyX.pntr, nRows, nCols);	GramSchmidt(copyX.pntr, nRows, nCols, 1, 0, y, b, r.pntr, dummy);	BackSub(r.pntr, r.nRows, b);	DisposeMatrix(&r);	DisposeMatrix(&copyX);}		/*  nuSolveSystem   */void SolveMultiSystem (x,y,b)Matrix *x,*y,*b;{	SolveMSys (x->pntr, x->nRows, x->nCols, y->pntr, y->nCols, b->pntr);}void SolveMSys (x,nRows,nCols, y,nY, b)double *x[], *y[], *b[];int nRows, nCols, nY;{#define		maxDim 20	Matrix	tableau;							/* tableau for holding system */	int pivot[maxDim+1];	int i, j, maxRow, useRow, lim, col, row;	double max, ratio, tot;		AllocMatrix (nRows, nCols + nY , &tableau);	CopyMat (x, tableau.pntr, nRows, nCols);			/* preserve original */	for (col=0; col<nY; ++col)							/* place right vectors in tableau */		memcpy((char *)tableau.pntr[col + nCols], (char *)y[col], nRows * sizeof(double));	lim = nCols - 1;	for (row=0; row<nCols; ++row)	{	/* init pivot array and scaling values*/		pivot[row] = row;		max = ABS(x[0][row]);		for (col=1; col<nCols; ++col)			if (ABS(x[col][row]) > max)				max = ABS(x[col][row]);	}	for (col=0; col<nCols-1; ++col)	{	/*  elimination loop  */		/*	PrintMatrix(&tableau, "Tableau at top of elim loop"); */		maxRow = col;		max = ABS(tableau.pntr[col][pivot[col]]);		for (row=col+1; row<nCols; ++row)		{	tot = ABS(tableau.pntr[col][pivot[row]]);				if (tot > max)				{	max = tot;					maxRow = row;				}		}		/* now exchange */		i = pivot[col];		pivot[col] = pivot[maxRow];		pivot[maxRow] = i;		if (IsNearZero(max))		{	SetErrorCode (2,"AAO: Error in Solving multiple system; reset.");			max = MORETHANZERO;		}		maxRow = pivot[col];		for (row=col+1; row<nCols; ++row)		{	/* sweep out elements "below" this row */			i = pivot[row];			ratio = tableau.pntr[col][i] / tableau.pntr[col][maxRow];			for (j=col+1; j<tableau.nCols; ++j)				tableau.pntr[j][i] -= ratio * tableau.pntr[j][maxRow];		}	}	/* now backsub for the cols of the inverse */	/* PrintMatrix(&tableau, "Post elim"); 		*/	maxRow = pivot[lim];	for (col=lim+1; col<tableau.nCols; ++col)	{	/* first handle divide by diagonal */		max = tableau.pntr[lim][maxRow];		if (IsNearZero(max))		{	SetErrorCode (3,"AAO: Diag near zero in solving system.");			max = MORETHANZERO;		}		tableau.pntr[col][maxRow] /=  max;		for (row=lim-1; row>-1; --row)		{	/* now rest of rows  */			useRow = pivot[row];			tot = tableau.pntr[col][useRow];			for (j=row+1; j<nCols; ++j)				tot -= tableau.pntr[col][pivot[j]] * tableau.pntr[j][useRow];			tableau.pntr[col][useRow] = tot / tableau.pntr[row][useRow];		}	}	for (row=0; row<nCols; ++row)  /* place solution into b in correct order */		for (col=0; col<nY; ++col)			b[col][row] = tableau.pntr[nCols + col][pivot[row]];	DisposeMatrix(&tableau);} 	/*  SolveMultiSystem  */void InvertMatrix (mat, inv)Matrix *mat, *inv;{	if ((mat->nRows != inv->nRows) ||		(mat->nCols != inv->nCols) ||		(mat->nRows != mat->nCols))		SetErrorCode (-1, "AAO: Dimension error in inverse");	else		InvertMat (mat->pntr, mat->nRows, inv->pntr);}void InvertMat (mat, dim, inv)double *mat[], *inv[];int dim;{	Matrix iden;	int row, col;			AllocMatrix (dim, dim, &iden);	for (col=0; col<dim; ++col)		for (row=0; row<dim; ++row)			if (row == col)				iden.pntr[col][row] = 1.0;			else				iden.pntr[col][row] = 0.0;	SolveMSys (mat, dim, dim,  iden.pntr, dim,  inv);	DisposeMatrix(&iden);}	 /*  InvertMatrix  */void MultMatrix (a,b, aTrans, prod)Matrix *a, *b, *prod;int aTrans;{	Boolean ok;		if (aTrans)		ok = (a->nRows EQ b->nRows) && (a->nCols EQ prod->nRows);	else		ok = (a->nCols EQ b->nRows) && (a->nRows EQ prod->nRows);	if (!ok)		SetErrorCode (-1, "AAO: Incompatible dims in mat mult");	else		MultMat (a->pntr, b->pntr, a->nRows, a->nCols, b->nCols, aTrans, prod->pntr);}void MultMat (a,b,rowsA, colsA, colsB, aTrans, prod)double *a[], *b[], *prod[];int rowsA, colsA, colsB, aTrans;{	int r, c, i;	double tot;	if (aTrans)			/* use dot prod to calc sums */		for (c=0; c<colsB; ++c)			for (r = 0; r<colsA; ++r)				prod[c][r] = DotProdVec (a[r], b[c], rowsA); 	else		for (r=0; r<rowsA; ++r)			for (c=0; c<colsB; ++c)			{	tot = 0.0;				for (i = 0; i<colsA; ++i)					tot += a[i][r] * b[c][i];				prod[c][r] = tot;			}}	/*  MultMat  */void TransMatrix (mat)Matrix *mat;{	if (mat->nRows != mat->nCols)		SetErrorCode (1, "AAO: Transpose rows # cols; using min.");	TransMat (mat->pntr, MIN(mat->nRows, mat->nCols));}void TransMat (mat, dim)double *mat[];int dim;{	int row, col;	double hold;	for (row = 0; row<dim; ++row)		for (col = row + 1; col<dim; ++col)		{	hold = mat[col][row];			mat[col][row] = mat[row][col];			mat[row][col] = hold;		}}   /*nuTransposeMatrix*/