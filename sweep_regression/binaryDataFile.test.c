// $Id:

#include <stdio.h>
#include <time.h>

#include "sweepMatrix.h"
#include "binaryDataFile.h"
#include "fileOps.h"


#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

static clock_t startTime, stopTime;

void Wait (void);
void Wait (void)
{
  char ch;
  ch = (char) getchar();
}

void PrintDuration(char *, clock_t start, clock_t finish);
void PrintDuration(char *message, clock_t start, clock_t finish)
{
  printf ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");
  printf ("  %s required %5.1f sec. \n",
	  message, ((double) (finish - start)) / CLOCKS_PER_SEC);
  printf ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");
  // Wait();
}


// --------------------------------------------------------------------------

#define NADD        2
#define NREMOVE     1
#define N_CP_COLS   5
#define USE_WTS	 TRUE
#define INTERACT TRUE
#define N_LIN_COMB  3

int main(void)
{
  
  // Test variables for binary data file.
  char *txtFileName = "test.txt";
  char *filePrefix  = "test";
  BinaryDataFile bdf, copyBDF;
  Vector vec, wts, cpArray[N_CP_COLS];
  Matrix covMat;
  long columnList[N_CP_COLS] = {0, 1, 2, 3, 4};
  
  Vector coefs, linComb;
  long lcIndex[N_LIN_COMB];
  double ssLinComb;
  
  int i, j, status;
  long nDataCols;     	
  int nGenCols  =  5;
  
  
  int seed;
  int nGenLines = 50;
  printf ("\n -------------  Generate test file  ----------- \n");
  startTime = clock();
  seed = WriteRandomFile(txtFileName, nGenLines, nGenCols, 26);
  stopTime = clock();
  printf ("Wrote %d lines with %d columns to %s\n", nGenLines, nGenCols, txtFileName);
  PrintDuration ("Generating the test file", startTime, stopTime);
  if (seed < 0) return 0;

  printf ("\n -------------  Check number of data fields  ----------- \n");
  startTime = clock();
  nDataCols = CountFields(txtFileName, 1) - 1;
  stopTime = clock();
  printf ("Found %ld fields\n", nDataCols);
  
  printf ("\n -------------  Compute means from text file  ----------- \n");
  vec = MeansFromTextFile(txtFileName, nGenCols);
  PrintVector (vec, "means from text file");
  
  printf ("\n -------------  Test of underlying BDF functions  ----------- \n");
  bdf = MakeBinaryDataFile (txtFileName, filePrefix,  USE_WTS, INTERACT);
  vec = MeanVectorBDF(bdf);
  PrintSummaryBDF(bdf);
  
  vec = SSVectorBDF(bdf);
  PrintVector (vec, "SS vector");
  
  vec = CPVectorBDF(bdf, 4);
  PrintVector (vec, "CP vector");
  
  status = CPArrayBDF(bdf, columnList, N_CP_COLS, cpArray);
  for (i=0; i<NumberOfVariablesBDF(bdf); ++i)
  {	printf ("[%3d] ", i);
	for (j=0; j < N_CP_COLS; ++j)
	  printf (" %7.2f", VectorElement(cpArray[j], i));
	printf("\n");
  }
  
  coefs = NewVector(3, "Test BDF");
  lcIndex[0] = 0; lcIndex[1]=3; lcIndex[2]=12;
  AssignVectorElement(coefs, 0, 7.2);
  AssignVectorElement(coefs, 1, -4.2);
  AssignVectorElement(coefs, 2, 43.1);
  linComb = LinearCombinationBDF(bdf,coefs, lcIndex, -20.0);
  CompactPrintVector(linComb, "Linear combination");
  ssLinComb = SSOfLinearCombinationBDF(bdf, coefs, lcIndex, -20.0);
  printf("SS of linear comb = %7.2f\n", ssLinComb);
  
  // Test of copy functions
  copyBDF = CopyBinaryDataFile(bdf);
  PrintSummaryBDF(copyBDF);
  
  vec = SSVectorBDF(copyBDF);
  PrintVector (vec, "SS vector from copy");
  
  // Reset the weight vector and compare -- should be same
  wts = WeightVectorBDF (copyBDF);
  CompactPrintVector(wts, "Wts from copy");
  SetWeightVectorBDF(copyBDF, wts);
  vec = SSVectorBDF(copyBDF);
  PrintVector (vec, "SS vector from copy");	
  
  // Compare covariance matrices with *new* weights -- should differ
  wts = NewVector (copyBDF->nFileLines, "Test BDF, copy");
  InitializeVector(wts, 1.0);
  SetWeightVectorBDF(copyBDF, wts);
  
  covMat = CPMatrixBDF(bdf, lcIndex,  3);
  PrintMatrix (covMat, "covariance matrix");
  covMat = CPMatrixBDF(copyBDF, lcIndex,  3);
  PrintMatrix (covMat, "covariance matrix of copy");
  
  
  /*
    printf ("\n -------------  Convert both files to binary  ----------- \n");
    startTime = clock();
    nFoundLinesEst = ConvertToBinaryFile(estFileName, nCols, binEstFileName);
    nFoundLinesVal = ConvertToBinaryFile(valFileName, nCols, binValFileName);
    stopTime = clock();
    printf ("Converted %d and %d lines from text file\n",
              nFoundLinesEst, nFoundLinesVal);
    PrintDuration ("Conversion to binary", startTime, stopTime);
    
    printf ("\n -------------  Build initial sweep matrix  ----------- \n");
    startTime = clock();
    sm = NewSweepMatrix(binEstFileName, nCols, INTERACT, binValFileName);
    ReadVariableNamesFromFile(sm, "processed.names");
    stopTime = clock();
    if (sm)
    PrintSweepMatrix (sm);
    else return 0;
    PrintDuration ("Initialization of sweep", startTime, stopTime);
    
    printf ("\n -------------  Initial regression calc  ----------- \n");
    startTime = clock();
    status = ExpandStepwiseModel(sm, NADD);
    stopTime = clock();
    printf ("Result returned from intial stepwise is %d\n", status);
    if (status <= 0) return 0;
    PrintDuration ("Expanding model", startTime, stopTime);
    
    printf ("\n -------------  Next regression calc  ----------- \n");
    startTime = clock();
    status = ExpandStepwiseModel(sm, NADD);
    stopTime = clock();
    printf ("Result returned from expanding stepwise is %d\n", status);
    if (status <= 0) return 0;
    PrintDuration ("Second expansion", startTime, stopTime);
    
    printf ("\n -------------  Reverse stepwise  ----------- \n");
    startTime = clock();
    status = TrimStepwiseModel(sm, NADD);
    stopTime = clock();
    printf ("Result returned from trimming stepwise is %d\n", status);
    if (status <= 0) return 0;
    PrintDuration ("Trimming model", startTime, stopTime);
    
    printf ("\n -------------  Forward again  ----------- \n");
    startTime = clock();
    status = ExpandStepwiseModel(sm, NADD);
    stopTime = clock();
    printf ("Result returned from expanding stepwise again is %d\n", status);
    if (status <= 0) return 0;
    PrintDuration ("Expansion again", startTime, stopTime);
    
    WriteRegressionDataToFile(sm, "stepwise.dat");
  */
  
  printf ("\nFINISHED...\n");
  return 1;
}
