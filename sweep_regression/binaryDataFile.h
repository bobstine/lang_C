// $Id: binaryDataFile.h,v 1.1 2005/06/13 20:47:51 bob Exp $

/*
	30 Sep 99 ... Make the weights a vector as well.
	13 Sep 99 ... Built from the old fileOps.
*/
#ifndef header_BDF
#define header_BDF

#include "vector.h"
#include "matrix.h"

#define MAX_BDF_NAME    50

////////////////////////////////////////////////////////////////////////////

typedef struct aBDF
{
  char fileNamePrefix[MAX_BDF_NAME];	// prefix for .bin and .bdf files
  char workingFileName[MAX_BDF_NAME];	// string for opening files
  long nFileColumns;                    // not including the initial weight column
  float *dataBuffer;			// holds data moving in and out
  long nFileLines;			// lines in the file
  double *pWeights;			// pointer to vector of sample weights
  double sumWeights;			// accum sum of weights (found when mean found)
  int useInteractions;			// boolean for interactions of predictors
  Vector meanVector;			// avgs of file columns, assumes wts defined.
} aBinaryDataFile;

typedef aBinaryDataFile *BinaryDataFile;

///////////////////////////////////////////////////////////////////////////////

BinaryDataFile
MakeBinaryDataFile (char *path, char *bdfPrefix,  int hasWeights, int useInteractions);
/* 
   Reads data from input text/ascii file, and saves converted data as binary file.
   Name for binary file appends .bin to prefix of input data.  hasWeights indicates
   if the first column of the input data is to be treated as a column of obs weights.
	
   Stores weights as a separate vector rather than in file.
   Binary data file has only data.	

   The first column of the data (column 0) will always be treated as the "response"
   column and not incorporated into interaction calculations.
*/

void
DeleteBinaryDataFile (BinaryDataFile bdf);

BinaryDataFile	OpenBinaryDataFile (char *fileNamePrefix);
void 			SaveBinaryDataFile (BinaryDataFile bdf);
/*
	Read the BDF from a file using input file name prefix.
	Close the file and leave it on disk so that it may be opened.
*/

BinaryDataFile CopyBinaryDataFile (BinaryDataFile bdf);
/*
	Copies it all, allowing you to change the weights from those
	used in the original, without changing the mean vector. It does not
	refer back to the original text file, and points at the same
	binary file.
*/

long
nFileLinesBDF (BinaryDataFile bdf);

long
nFileColumnsBDF (BinaryDataFile bdf);

double
SumWeightsBDF (BinaryDataFile bdf);
/*
	Effectively the number of observations in the data file.  Reports the value
	zero unless the BDF has been initialized with some calculations.
*/

Vector
WeightVectorBDF(BinaryDataFile bdf);

void
SetWeightVectorBDF(BinaryDataFile bdf, Vector newWeights);
/*
	Changing the weight vector will flush the stored mean vector
	and require this to be recomputed, prior to further covariance computation.
*/

void
PrintSummaryBDF (BinaryDataFile bdf);
/*
	Prints very short summary of the internal structure.
*/


char*
DataFileNameBDF (BinaryDataFile bdf);

char*
FileNameBDF (BinaryDataFile bdf);
/*
	Return the names used to store the encoded data and the structure of the 
	bdf file itself.
*/


/********************  Statistical Functions *************************************/

long  NumberOfPredictorsBDF(BinaryDataFile bdf);
long  NumberOfVariablesBDF(BinaryDataFile bdf);
/*
    Number of columns for output covariances as function of number of
    input file columns incorporating any interaction terms.
*/


Vector MeanVectorBDF(BinaryDataFile bdf);
Vector SSVectorBDF(BinaryDataFile bdf);
Vector CPVectorBDF(BinaryDataFile bdf, long column);
int    CPArrayBDF(BinaryDataFile bdf, long *pCols, int nCols, Vector vecArray[]);
Matrix CPMatrixBDF(BinaryDataFile bdf, long *pCols, int nCols, Vector wts);
/*
   Vector of the column means, CPs, or SS, using the underlying
   weights if they exist. The summary vectors include the interactions
   if they are being used.  The mean will be computed as needed for SS
   and covariances.  Lengths are one plus the number of predictors.

   wts in CPMatrix allow this routine to use alternative weights, as in
   the rWWr matrix needed by stepwise with survey weights. If wts is null,
   then will default to the internal weights.
*/

void
PredictorsUsedInColumnBDF(BinaryDataFile bdf, long column, long *p1, long *p2);

Vector
ColumnBDF(BinaryDataFile bdf, long columnIndex);

int
WriteColumnsToFileBDF(BinaryDataFile bdf,
		      long *outputCols, int nCols, char *outFileName);
/*
   Extracts indicated column or feature of a column from the binary
   data file, with interactions allowed. No mean or weight adjustment,
   so get the raw product for interaction terms.
*/

double
SSOfLinearCombinationBDF(BinaryDataFile bdf,
			 Vector coefs, long *coefIndex, double offset);

Vector
LinearCombinationBDF(BinaryDataFile bdf,
			     Vector coefs, long *coefIndex, double offset);

int
FillResidualSSBDF(BinaryDataFile bdf, int k, IndexedVector sweep[], int *skip,
		  Vector fit, Vector obsVar,
		  Vector a, Vector b2);  // max term and variance z'W(obsvar)Wz

/*
   Computes the linear combination of the columns defined by the coefs
   vector with the indexing implied by the cols pointer, and
   accumulates the sum of the squared values over the observations in
   the indicated binary file.  The value of the offset is added to
   each linear combination prior to squaring.  Automatically computes
   the interaction terms as needed based on coefIndex values.
	
   The SS is weighted, but the linear combination is returned in "raw" form.
	
	Examples:

	To get the residual SS, set coefs[0] to -1 and
	coefs[1..k] to the regression slopes, with offset as
	-intercept.

	To get the fitted values, set coefs[0]=0.0 and rest to regr
	slopes, with offset as intercept.
*/

#endif
