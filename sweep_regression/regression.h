// $Id: regression.h,v 1.1 2005/06/13 20:47:51 bob Exp $

/*
  12 Jul 00 ... Path based file tracking.
  30 Jun 00 ... Revise variance calculations.
   4 Oct 99 ... Save, free and restore sweep matrix.
  20 Sep 99 ... Start to incorporate the BDF functions.
   2 Aug 99 ... Add log file, open as file with suffix on data file name.
   5 Nov 98 ... Add access to coefs and summary stats.
  30 Oct 98 ... Better tracking of number vars left to use.
  26 Oct 98 ... Move to all pointer based vectors in sweep matrix.
  15 Oct 98 ... Created.
*/

#ifndef _SWEEP_
#define _SWEEP_

#include <map.h>

#include "vector.h"
#include "matrix.h"
#include "binaryDataFile.h"

#define SWEEP_MAX_COLUMNS 		500
#define SWEEP_NEAR_ZERO			1.0E-20
#define SWEEP_NAME_LEN			100

//////////////////////////////////////////////////////////////////////////////////

typedef struct sweepMatrix
{
  BinaryDataFile bdf;			 // Binary data file of input data
  BinaryDataFile validationBDF;          //                validation data
  char path[SWEEP_NAME_LEN];             // Place files in this subdirectory
  char **varNames;                       // Names for Y, X1, ... X_m (no interactions)
  Vector rawWeights;		         // Weight from initial data, eg sampling wts
  Vector rawSS;				 // SS about mean in columns
  Vector residualSS;                     // Updated residual SS about mean
  long nSkip;                            // Number of skipped cols
  int *skipVector;			 // Columns to skip in further calculation
  IndexedVector incrSweepMat[SWEEP_MAX_COLUMNS]; // incremental sweep matrix
  IndexedVector fullSweepMat[SWEEP_MAX_COLUMNS]; // fully swept matrix
  map<long,Vector> CPCache;              // Precomputed covariance queue
  int nextRow;				 // Row in sweep matrix that holds next covar
  int qBase;                             // Used in adaptive thresholding
} aSweepMatrix;

typedef aSweepMatrix *SweepMatrix;


////////////////////////////////////////////////////////////////////////////////

typedef struct regrCoef
{	long variable;
	double coef;
	double se;
} aRegrCoef;

typedef aRegrCoef *RegrCoef;

///////////////////////////////////////////////////////////////////////////////


/////////////////////  Building a sweep matrix  /////////////////////////////

SweepMatrix
NewSweepMatrix(char *path, BinaryDataFile bdf, BinaryDataFile validationBDF);

void
DeleteSweepMatrix (SweepMatrix sm);
/*
  Initializes a sweep matrix object from the contents of the found BINARY file.
  Negative results signal an error.  Interaction presence defined in bdf. Uses the
  data in the validation file to validate fit only.  Leave NULL if none.
*/

SweepMatrix
ReweightedSweepMatrix (SweepMatrix sm, Vector weights);
/*
  Clones the basic features of the input sm, but modifies data BDF using the input
  weights, and rebuilds the covariance queue using the previously chosen predictors.
*/

SweepMatrix
RestoreSweepMatrixFromFile (char *fileName);

int
SaveSweepMatrixToFile (SweepMatrix sm, char *fileName);
/*
  This pair save and restore a sweep matrix from a disk file.
*/


/////////////////////  Grow sweep matrix by adding X  ///////////////////////


int
UpdateCache(SweepMatrix sm, long *columns, int nCols, int addAll);


int
ExpandStepwiseModel(SweepMatrix sm, int nToAdd);

int
AddPredictorsToModel(SweepMatrix sm, long *cols, int nToAdd, int forced);
/* forced implies that one knew the predictors a priori rather than found
   them through the use of a variable selection search.  If forced, the
   qBase slot is incremented by nToAdd.
*/

int
TrimStepwiseModel(SweepMatrix sm, int nToRemove);


//////////////////  Read - Write status data to file  ///////////////////////

void
WriteRegressionDataToFile (SweepMatrix sm);  // file sweep.dat in path

int
ReadVariableNamesFromFile (SweepMatrix sm);  // file names in path

long
WriteCoefficientsToFile (SweepMatrix sm);    // model.q in path

void
WriteCurrentFit (SweepMatrix sm);			

void
WriteCurrentFitSummary (SweepMatrix sm);
/*
  Both write to log file (suffix .regrlog).  WriteCurrentFit also
  writes the summary to the file as well.
*/

void
PrintSweepMatrix (SweepMatrix sm);		// Prints summary to std out


//////////////////  Features of regression model  ///////////////////////

long	NumberOfObservations (SweepMatrix sm);
long    NumberOfPredictors (SweepMatrix sm);
long    NumberOfPossibleBasePredictors(SweepMatrix sm); // excluding interactions
long    NumberOfPossiblePredictors (SweepMatrix sm);

long	NumberOfCoefficients (SweepMatrix sm);
double	NumberOfWeightedObservations (SweepMatrix sm);
double  DegreesOfFreedom (SweepMatrix sm);

long *	ColumnsUsedInModel (SweepMatrix sm);

RegrCoef EstimatedCoefficients (SweepMatrix sm);
/*
  Pointer to array of regression coefficients with mean as leading
  value.  Length matches that given by call to number of coefficients
  functions given above.  Number of coefficients *includes* the
  constant term.
*/

double	RSquared (SweepMatrix sm);
double	SSquared (SweepMatrix sm);
double  ValidationSS (SweepMatrix sm);

double 	MDL (SweepMatrix sm);        	//  J Rissanen (1999) MDL denoising, eqn 28.

Vector	FitVector (SweepMatrix sm);
Vector	ResidualVector (SweepMatrix sm);

double 	Intercept (SweepMatrix sm);
Vector	CoefficientVector (SweepMatrix sm);


Matrix
RawCovMatOfEstimates (SweepMatrix sm, int code);  // s^2 (X'X)-1 (0 direct, 1 sweep)

Matrix
HomoCovMatOfEstimates (SweepMatrix sm);  // s^2 (X'WX)-1 (X'WWX) (X'WX)-1

Matrix
BinCovMatOfEstimates (SweepMatrix sm);   // v^2 (X'X)-1 (X'WX) (X'X)-1

Matrix
HeteroCovMatOfEstimates (SweepMatrix sm);  // (X'X)-1 (X'e^2X) (X'X)-1

Matrix	CrossProductMatrix(SweepMatrix sm, int excludeY);
/*
  First group offer various forms to estimate the  covariance matrix
  of the fitted coefs.
  
  Second extracts the kxk or k+1 x k+1 cross product matrix associated with the
  current state of the sweep operation, with leading Y component unless excluded.
  excludeY is 0 if you want to start with y (col 0) or 1 if you start with X's.
*/

////////////////  Weight vector for regression model  ///////////////////////


Vector
BinomialVector (SweepMatrix sm, double *sumWeights);
/*
  Computes binomial weights assuming response is 0/1 data.
*/

void
CheckWeightVector(SweepMatrix sm, double *maxDiff, double *avgDiff, double *corr);
/*
  Uses binomial wts as computed

  Change in weights may be large, but this may only reflect a rescaling of the
  problem, making the correlation a useful summary as well.
*/



#endif
