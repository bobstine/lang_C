// $Id: dataMatrix.c,v 1.1.1.1 2001/06/29 12:48:15 bob Exp $

/*
  21 Jun 01 ... Row-major form for mgs version of sweeper.
  15 Oct 98 ... Created in original form for sweeper.
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "dataMatrix.h"
#include "memory.h"

//////////////////////////////////////////////////////
//
// typedef struct aDataArray
// {
//  float *pData;
//  long nRows, nCols;
// } aDataArray;
//
// typedef struct aDataArray *DataArray;
//
/////////////////////////////////////////////////////

#define ExitOnNull(ptr,str,result) if(NULL==(ptr)){fprintf(stderr,"\nDA: %s\n",(str));return (result);}

#define ExitOn(c, str, val) if (c){fprintf(stderr, "\nDA: %s\n", (str)); return (val);}


//  --------------------  New, delete, copy  ---------------------

DataArray
NewDataArray (long nRows, long nCols, char *creator)
{
  DataArray m;
  
  m = (DataArray) Allocate ( sizeof(aDataArray), creator, "DAnw" );
  ExitOnNull(m, "could not allocate structure", NULL);
  m->nRows = nRows;
  m->nCols = nCols;
  m->pData = (float *) Allocate(nRows * nCols * sizeof(float), creator, "DAdt");
  ExitOnNull(m->pData, "DM: could not allocate data", NULL);
  return m;
}

DataArray
CopyDataArray (DataArray da, char *creator)
{
  DataArray copy;
  float *src, *dest;
  long nRows, nCols, i;
  
  DimensionsDataArray(da, &nRows, &nCols);
  copy = NewDataArray(nRows, nCols, creator);
  ExitOnNull(copy, "DA: unable to allocate copy.", NULL);
  src = da->pData;
  dest = copy->pData;
  for (i=0; i <nRows*nCols; ++i)
  	  *dest++ = *src++;
  return copy;
}

void
DeleteDataArray (DataArray da)
{
  Release (da->pData);
  Release (da);
}


// --------------  Read, write  ---------------------------------

DataArray
ReadDataArray (char *fileName)
{
  DataArray da;
  float *dest;
  long nR, nC;
  int nRead;
  FILE *input;

  // Open input file as read only
  input = fopen(fileName, "r");
  if (input == NULL)
  {	fprintf(stderr, "DA: could not open input file %s.\n", fileName);
	return NULL;	
  };
  // Parse nRows, nCols from first line of input file, allocate
  nRead = fscanf(input,"%ld %ld\n", &nR, &nC);
  if (2 != nRead)
  { fprintf(stderr, "DA: data matrix dimensions not found; got %d.", nRead);
    return NULL;
  }
  printf("DA: scanned dimensions [%ld, %ld] from %s\n", nR, nC, fileName);
  da = NewDataArray(nR, nC, "ReadDataArray");
  // Read nRows lines into array
  dest = da->pData;
  for (long j = 0; j < nR*nC; ++j)
  { fscanf(input, "%f", dest++);
  }
  // Close files, and return
  fclose(input);
  return da;
}

int
WriteDataArray (DataArray da, char *fileName)
{
  float *src;
  FILE *output;

  // Open output text file for writing
  output = fopen(fileName, "w");
  if (output == NULL)
  {	fprintf(stderr, "DM: could not open output file %s.\n", fileName);
	return 0;	
  };
  // Write the dimensions as first row of output
  fprintf(output, "%ld %ld\n", da->nRows, da->nCols);
  // Write nRows lines into array, padding with line feeds
  src = m->pData;
  for(long i=0; i<da->nRows; ++i)
  { for (long j = 0; j < da->nCols; ++j)
      fprintf(output, "%f ", *src++);
    fprintf(output,"\n");
  } 
  // Close files, and return
  fclose(output);
  return 1;
}

//  Access to matrix elements, columns

void
PrintDataArray (DataArray da, char *msg, long nRows, long nCols)
{
  float *src;
  long i,j;
  
  printf ("\nData matrix [%ld,%ld]  (%s)", da->nRows, da->nCols, msg);
  src = da->pData;
  for(i=0; i<nRows; ++i)
  { printf("\n[%4ld] ", i);
    for(j=0; j<nCols; ++j)
      printf("%6.2f ", *src++);
  }
  printf("\n ------ \n\n");
}


void
DimensionsDataArray (DataArray da, long *nRows, long *nCols)
{
  *nRows = da->nRows;
  *nCols = da->nCols;
}


float
ElementDataArray (DataArray da, long row, long col)
{
  return( *da->pData+col+row*da->nCols );
}

float *
DataArrayRowPtr (DataArray da, long row)
{
  return( da->pData+row*da->nCols );
}


