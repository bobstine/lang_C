// $Id: dataArray.c,v 1.6 2001/11/21 13:30:56 bob Exp $

/*
   9 Jul 01 ... Use the new type of operators
  21 Jun 01 ... Row-major form for mgs version of sweeper.
  15 Oct 98 ... Created in original form for sweeper.
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

#include "dataArray.h"
#include "operator.h"
#include "memory.h"


#define ExitOnNull(ptr,str,result) if(NULL==(ptr)){fprintf(stderr,"\nDA: %s\n",(str));return (result);}

#define ExitOn(c, str, val) if (c){fprintf(stderr, "\nDA: %s\n", (str)); return (val);}


//  --------------------  New, delete, copy  ---------------------

DataArray
NewDataArray (long nRows, long nCols, int nPad, char *creator)
{
  DataArray m;
  long offset;
  float *base;

  printf("DA: allocating data array of dimension [%ld,%ld(+%d)]\n",
	 nRows, nCols, nPad);
  m = (DataArray) Allocate ( sizeof(aDataArray), creator, "DAnw" );
  ExitOnNull(m, "could not allocate structure", NULL);
  m->nRows = nRows;
  m->nCols = nCols;
  m->nPad = nPad;
  m->pData = (float *) Allocate(nRows * (nCols+nPad) * sizeof(float), creator, "DAdt");
  ExitOnNull(m->pData, "DM: could not allocate data", NULL);
  m->pRows = (float **) Allocate(nRows * sizeof(float *), creator, "DArp");
  ExitOnNull(m->pRows, "DM: could not allocate row ptr", NULL);
  offset = nCols+nPad;
  base = m->pData;
  for (long i=0; i<nRows; ++i)
  { m->pRows[i] = base;
    // this next code zeros out the array
    // for (long j=0; j<nCols+nPad; ++j)
    //   base[j] = 0.0;
    base += offset;  // only offset by number of float, *not* count of bytes
  }
  return m;
}

DataArray
CopyDataArray (DataArray da, char *creator)
{
  DataArray copy;
  float *src, *dest;
  long nRows, nCols, nPad, i;
  
  DimensionsDataArray(da, &nRows, &nCols, &nPad);
  copy = NewDataArray(nRows, nCols, nPad, creator);
  ExitOnNull(copy, "DA: unable to allocate copy.", NULL);
  src = da->pData;
  dest = copy->pData;
  for (i=0; i <nRows*(nCols+nPad); ++i)
    *dest++ = *src++;
  return copy;
}

void
DeleteDataArray (DataArray da)
{
  Release (da->pData);
  Release (da->pRows);
  Release (da);
}


// --------------  Read, write  ---------------------------------

DataArray
ReadDataArray (char *fileName, long nPad)
{
  DataArray da;
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
  da = NewDataArray(nR, nC, nPad, "ReadDataArray");
  // Read nRows lines into array
  for (long i = 0; i < nR; ++i)
  { float *row = da->pRows[i];
    for (long j=0; j < nC; ++j)
      fscanf(input, "%f", row++);
  }
  // Close files, and return
  fclose(input);
  return da;
}

int
WriteDataArray (DataArray da, char *fileName)
{
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
  for(long i=0; i<da->nRows; ++i)
  { float *src = da->pRows[i];
    for (long j = 0; j < da->nCols; ++j)
      fprintf(output, "%f ", *src++);
    fprintf(output,"\n");
  } 
  // Close files, and return
  fclose(output);
  return 1;
}

// --- Access to matrix elements, columns

void
PrintDataArray (DataArray da, char *msg, long maxRows, long maxCols)
{
  long nRows = da->nRows;
  long nCols = da->nCols;
  long nPrintRows = (nRows < maxRows) ? nRows : maxRows;
  long nPrintCols = (nCols < maxCols) ? nCols : maxCols;
  
  printf ("Data matrix [%ld,%ld(%d)]  (%s)", nRows, nCols, da->nPad, msg);
  for(long i=0; i<nPrintRows; ++i)
  { float *pRow = da->pRows[i];
    printf("\n[%4ld] ", i);
    for(long j=0; j<nPrintCols; ++j)
      printf("%6.2f ", pRow[j]);
    if (maxCols<nCols)
      printf("... %6.2f ", pRow[nCols-1]);
  }
  if (maxRows<nRows)
  { float *pRow = da->pRows[nRows-1];
    printf("\n...\n[%4ld] ", nRows);
    for(long j=0; j<nPrintCols; ++j)
      printf("%6.2f ", pRow[j]);
    if (maxCols<nCols)
      printf("... %6.2f ", pRow[nCols-1]);
  }
  printf("\n ------ \n");
}


void
DimensionsDataArray (DataArray da, long *nRows, long *nCols, long *nPad)
{
  *nRows = da->nRows;
  *nCols = da->nCols;
  *nPad = da->nPad;
}


long
NRowsOfDataArray (DataArray da)
{
  return da->nRows;
}

long
NColsOfDataArray (DataArray da)
{
  return da->nCols;
}

float
ElementDataArray (DataArray da, long row, long col)
{
  float *rowPtr = da->pRows[row];

  return( rowPtr[col] );
}

float *
RowPtrOfDataArray (DataArray da, long row)
{
  return( da->pRows[row] );
}


float **
BasePtrOfDataArray (DataArray da)
{
  return( da->pRows );
}
     
//  --- Manipulate columns

long
UsePaddingOfDataArray (DataArray da)
{
  ExitOn ((0 == da->nPad), "No padding columns are available", 0);
  -- da->nPad;
  ++ da->nCols;
  return da->nCols-1;
}


//  --- Operators ---

void
ApplyOperatorToDataArray (Operator f, DataArray src, int useWeights,
			  DataArray dst, long dstColumn)
{
  // Check bounds
  if (dstColumn >= dst->nCols)
  { printf("Destination data array has %ld cols; lacks column %ld\n",
	   dst->nCols, dstColumn);
    return;
  }
  assert(src->nRows == dst->nRows); // Data arrays have unequal number of rows
  // Watch for weighted calculation
  if (useWeights)
  { for (long i=0; i<src->nRows; ++i)
    { float *pInRow  = src->pRows[i];
      float *pOutRow = dst->pRows[i];
      pOutRow[dstColumn] = EvalOperator(f, pInRow+1); // +1 to skip wt
    }
  }
  else
  { for (long i=0; i<src->nRows; ++i)
    { float *pInRow  = src->pRows[i];
      float *pOutRow = dst->pRows[i];
      // sum +=
      pOutRow[dstColumn] = EvalOperator(f, pInRow);
    }
  }
}
