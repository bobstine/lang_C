// $Id: dataMatrix.h,v 1.1.1.1 2001/06/29 12:48:15 bob Exp $

/*
  21 Jun 01 ... Row-major form for mgs version of sweeper.
  15 Oct 98 ... Created in original form for sweeper.
*/

#ifndef _DATAARRAY_
#define _DATAARRAY_  

////////////////////////////////////////////////////////////////////////////////

typedef struct aDataArray
{
  float *pData;
  long nRows, nCols;
} aDataArray;

typedef struct aDataArray *DataArray;

////////////////////////////////////////////////////////////////////////////////

// New, delete, copy

DataArray
NewDataArray (long nRows, long nCols, char *creator);

DataArray
CopyDataArray (DataArray da, char *creator);

void
DeleteDataArray (DataArray m);

// Read, write to file
//       Format:  first line nRows nCols
//                rest  data11 data12 .... data1p             

DataArray
ReadDataArray (char *fileName);

int
WriteDataArray (DataArray da, char *fileName);

//  Access to matrix elements, columns

void
PrintDataArray (DataArray da, char *msg, long nRows, long nCols);

void
DimensionsDataArray (DataArray da, long *nRows, long *nCols);

float
ElementDataArray (DataArray da, long row, long col);

float *
DataArrayRowPtr (DataArray da, long row);

#endif

