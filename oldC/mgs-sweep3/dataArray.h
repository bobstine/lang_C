// $Id: dataArray.h,v 1.1.1.1 2001/12/04 20:32:34 bob Exp $

/*
  23 Aug 01 ... Alter how operators are applied to data arrays.
  7  Jul 01 ... Import operators from distinct file,then revised.
  28 Jun 01 ... Operator in place.
  26 Jun 01 ... Row-major form for mgs version of sweeper.
  15 Oct 98 ... Created in original form for sweeper.
*/

#ifndef _DATAARRAY_
#define _DATAARRAY_  

#include "operator.h"

////////////////////////////////////////////////////////////////////////////////

typedef struct aDataArray
{
  float *pData;
  float **pRows;
  long nRows, nCols;
  int nPad;              // number of unused elements at the end of each row
} aDataArray;

typedef struct aDataArray *DataArray;

////////////////////////////////////////////////////////////////////////////////

//
// New, delete, copy
//

DataArray
NewDataArray (long nRows, long nCols, int nPad, char *creator);

DataArray
CopyDataArray (DataArray da, char *creator);

void
DeleteDataArray (DataArray m);

// Read, write to file
//       Format:  first line nRows nCols
//                rest  data11 data12 .... data1p             

DataArray
ReadDataArray (char *fileName, long nPad);
/*
  Padding the data array leaves nPad extra columns of space
  at the end of each row for subsequent use.
*/

int
WriteDataArray (DataArray da, char *fileName);
/*
  Does not include any padding.
*/

void
PrintDataArray (DataArray da, char *msg, long maxRows, long maxCols);

//
//  Access to matrix elements
//

void
DimensionsDataArray (DataArray da, long *nRows, long *nCols, long *nPad);

long
NRowsOfDataArray (DataArray da);
long
NColsOfDataArray (DataArray da);

float
ElementDataArray (DataArray da, long row, long col);

float *
RowPtrOfDataArray (DataArray da, long row);

float **
BasePtrOfDataArray (DataArray da);

//
//  Interaction with Operators
//

long
UsePaddingOfDataArray (DataArray da);
/*
  Allows writing into the padding column; returns the added column index.
  Returns zero if no padding is available.
*/

void
ApplyOperatorToDataArray (Operator f, DataArray src, int useWeights,
			  DataArray dst, long dstColumn);
/*
  Applies the function row-by-row, inserting the results in column dstColumn
  of the dst data array.  Input data array is *not* altered.
*/

#endif
