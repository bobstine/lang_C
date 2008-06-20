// $Id: raggedArray.h,v 1.4 2001/07/31 08:21:26 bob Exp $

/*
  22 Jul 01 ... Add inverse operation.
  27 Jun 01 ... Created for mgs sweeper 2.
*/

#ifndef _RAGGEDARRAY_
#define _RAGGEDARRAY_  

#include "matrix.h"

////////////////////////////////////////////////////////////////////////////////

typedef struct aRaggedArray
{
  long maxSize, size;
  long   *pLen;
  double **pArray;
} aRaggedArray;

typedef struct aRaggedArray *RaggedArray;

////////////////////////////////////////////////////////////////////////////////

RaggedArray
NewRaggedArray (long maxSize, char *creator);

void
DeleteRaggedArray (RaggedArray ra);

void
PrintRaggedArray (RaggedArray ra, char *msg);

void
WriteRaggedArrayToFile (RaggedArray ra, FILE *fp);

RaggedArray
ReadRaggedArrayFromFile (FILE *fp);

//  access to matrix elements, columns

long
SizeOfRaggedArray (RaggedArray ra);

double
IndexRaggedArray (RaggedArray ra, long i, long j);

double *
ElementOfRaggedArray (RaggedArray ra, long j);

long
AppendToRaggedArray (RaggedArray ra, double *x, long len);


// Operations

Matrix
MatrixFromRaggedArray (RaggedArray ra);

Matrix
InverseTriangularRaggedArray (RaggedArray ra);  // if triangular input

#endif

