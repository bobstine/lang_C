// $Id: raggedArray.c,v 1.5 2001/08/21 21:42:38 bob Exp $

/*
  27 Jun 01 ... Created for mgs sweeper 2.
*/

#include <stdlib.h>
#include <stdio.h>

#include "raggedArray.h"
#include "memory.h"

/////////////////////////

#define ExitOnNull(ptr,str,result) if(NULL==(ptr)){fprintf(stderr,"\nRA: %s\n",(str));return (result);}

#define ExitOn(c, str, val) if (c){fprintf(stderr, "\nRA: %s\n", (str)); return (val);}

//////////////////////////

RaggedArray
NewRaggedArray (long maxSize, char *creator)
{
  RaggedArray ra;
  
  ra = (RaggedArray) Allocate ( sizeof(aRaggedArray), creator, "RAGn" );
  ExitOnNull(ra, "could not allocate ragged array structure", NULL);
  ra->maxSize = maxSize;
  ra->size = 0;
  ra->pLen   = (long *) Allocate(maxSize * sizeof(long), creator, "RAGl");
  ExitOnNull(ra->pLen, "could not allocate length vector.", NULL);
  ra->pArray = (double **) Allocate(maxSize * sizeof(double *), creator, "RAGp");
  ExitOnNull(ra->pArray, "could not allocate column pointers.", NULL);
  return ra;
}

void
DeleteRaggedArray (RaggedArray ra)
{
  for (long j=0; j < ra->size; ++j)
    Release(ra->pArray[j]);
  Release (ra->pArray);		// points to array of col pointers
  Release (ra->pLen);
  Release (ra);
}
#define MIN(a,b) (((a)<(b))? (a) : (b))

void
PrintRaggedArray (RaggedArray ra, char *msg)
{
  long maxRow=0, printRows=0;

  for (long j=0; j<ra->size; ++j)
    if (maxRow < ra->pLen[j])
      maxRow = ra->pLen[j];
  printRows = (maxRow < 8) ? maxRow : 8;  // dont go on for too long
  printf("\nRagged Array [%ld/%ld] (%s)\n", ra->size, ra->maxSize, msg);
  for (long j=0; j<ra->size; ++j)
    printf("[%3ld(%3ld)] ", j, ra->pLen[j]);
  printf("\n");
  for (long i=0; i<printRows; ++i)
  { for (long j=0; j<ra->size; ++j)
    { if (i < ra->pLen[j])
        printf("%9.4f ", ra->pArray[j][i]);
      else
	printf("          ");
    }
    printf("\n");
  }
  if (printRows < maxRow)
  { for(long j=0; j<ra->size; ++j)
      if (ra->pLen[j]>=8)
	printf("   ...    ");
      else
	printf("          ");
    printf("\n");
  }
  printf(" ----- \n");
}

//  file write, read

void
WriteRaggedArrayToFile (RaggedArray ra, FILE *fp)
{
  // write a header with number of elements
  fprintf(fp, "RaggedArray %ld %ld\n",ra->size,ra->maxSize);
  // write each element with length prefix
  for (long j=0; j<ra->size; ++j)
  { fprintf(fp, "%ld  ", ra->pLen[j]);
    for(long i=0; i < ra->pLen[j]; ++i)
      fprintf(fp, "%f ", ra->pArray[j][i]);
    fprintf(fp, "\n");
  }
}

RaggedArray
ReadRaggedArrayFromFile (FILE *fp)
{
  double *ptr;
  long q, maxq, len, pos;
  int count;
  RaggedArray ra;
  
  // get number of elements and max size from file
  // Ignores the specific leading string
  pos = ftell(fp);
  count = fscanf(fp, "%*s %ld %ld", &q, &maxq);
  if (2 != count)
  { char str[80];
    fscanf(fp,"%s", str);
    printf("RA: Did not get two dimensions; read %d items @ %ld.\n", count, pos);
    printf("  : Next string holds \"%s\"\n", str);
    return NULL;
  }
  printf("RA: Reading array of size %ld(%ld) from file @ %ld\n", q, maxq, pos);
  ra = NewRaggedArray(maxq, "ReadRagged");
  ExitOnNull(ra, "Could not allocate ra for reading", NULL);
  // read elements from file and append them
  for (long j=0; j<q; ++j)
  { fscanf(fp, "%ld", &len);
    ptr = (double *) Allocate( len * sizeof(double), "ReadRagged", "doub");
    ExitOnNull (ptr, "could not get element for ragged array", NULL);
    for (long i=0; i<len; ++i)
      fscanf(fp, "%lf", &ptr[i]);
    AppendToRaggedArray(ra, ptr, len);
  }
  return ra;
}


//  access to matrix elements, columns

long
SizeOfRaggedArray (RaggedArray ra)
{
  return ra->size;
}

double
IndexRaggedArray (RaggedArray ra, long i, long j)
{
  if ((j < ra->size) && (i < ra->pLen[j]))
    return ElementOfRaggedArray(ra,j)[i];
  else
  { fprintf(stderr, "RA: [%ld,%ld] out of bounds; sizes are [%ld,%ld]\n",
	    i,j, ra->pLen[j], ra->size);
    return 0.0;
  } 
}

double *
ElementOfRaggedArray (RaggedArray ra, long j)
{
  if (j < ra->size)
    return ra->pArray[j];
  else
  { fprintf(stderr, "RA: Attempt to access pointer %ld; size is %ld\n",
	    j, ra->size);
    return NULL;
  }
}

long
AppendToRaggedArray (RaggedArray ra, double *x, long len)
{
  if (ra->size < ra->maxSize)
  { ra->pLen[ra->size] = len;
    ra->pArray[ra->size] = x;
    ++ ra->size;
    return (ra->size);
  }
  else
  { fprintf(stderr, "RA: size limit exceeded\n");
    return(0);
  }
}


//  Matrix operations

Matrix
MatrixFromRaggedArray (RaggedArray ra)
{
  Matrix result = NULL;
  long maxRow = 0;
  
  for (long i=0; i<ra->size; ++i)
    if (ra->pLen[i]>maxRow)
      maxRow = ra->pLen[i];
  result = NewMatrix (maxRow, ra->size, "MatrixRA");
  // initialize to zero
  InitializeMatrix(result, 0.0);
  // copy over the non-empty rows
  for (long col=0; col<ra->size; ++col)
  { double *y=ra->pArray[col];
    for (long row=0; row<ra->pLen[col]; ++row)
      AssignMatrixElement(result,row,col,y[row]);
  }
  return result;
}


Matrix
InverseTriangularRaggedArray (RaggedArray ra)
{
  Matrix inverse = NULL;

  // check first that array is in fact triangular
  for (long i=0; i<ra->size; ++i)
    if ( (1+i) != ra->pLen[i] )
    { fprintf(stderr, "RA: input array is not triagular.\n");
      return NULL;
    }

  // allocate the destination matrix
  inverse = NewMatrix(ra->size, ra->size, "InvTriRA");
  InitializeMatrix (inverse,0.0);
  
  // backsubstitution recursion
  for (long k=0; k<ra->size;++k)     // k is the current column of inverse
  { double diag = ra->pArray[k][k];
    ExitOn ((diag <= 0.0), "Diagonal outside bounds", NULL);
    AssignMatrixElement(inverse, k, k, 1.0 / diag);
    for (long j=1; j<=k; ++j)
    { double num = 0.0;
      for (long i=1; i<=j; ++i)
	num += MatrixElement(inverse, k-j+i, k) * ra->pArray[k-j+i][k-j];
      AssignMatrixElement(inverse, k-j, k, - num/diag);
    }
  }
  return inverse;
}
