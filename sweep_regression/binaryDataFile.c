// $Id: binaryDataFile.c,v 1.1 2005/06/13 20:47:50 bob Exp $

/*
  4 Oct 99 ... Killed ++line bug in CPmatrix that caused early termination.
 13 Sep 99 ... Cut from the old fileOps.
*/

#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

#include "binaryDataFile.h"
#include "fileOps.h"

#ifndef TRUE
#define FALSE 0
#define TRUE 1
#endif

#ifndef MAX
#define MAX(a,b) (((a)>=(b)) ? (a) : (b))
#endif

#ifndef ABS
#define ABS(x) ((x)>0 ? (x) : (-(x)))
#endif

/********************************************************************************/

#define ExitOnNull(ptr,str,result) if(NULL==(ptr)){fprintf(stderr,"\nBDF: %s\n",str);return (result);}

/*********************** Internal functions *************************************/ 

static int AllocateBDF (BinaryDataFile bdf);
static inline float DataBufferElementBDF (BinaryDataFile bdf, long index);


/********************** Allocation Utilities    ************************************/

static int AllocateBDF (BinaryDataFile bdf)
{
  if (bdf->nFileLines <= 0)
  {	fprintf(stderr, "FileOps: line count zero or negative.\n");
	return 0;
  };
  if (bdf->nFileColumns <= 0)
  {	fprintf (stderr, "FilesOps: number of columns zero or negative.\n");
	return 0;
  }
  // allocate room for weights, data buffer
  bdf->pWeights = new double[bdf->nFileLines]; 
  ExitOnNull(bdf->pWeights, "could not allocate weight vector", 0);
  // buffer needs room for leading double and rest which are floats
  bdf->dataBuffer = new float[bdf->nFileColumns+2];
  ExitOnNull(bdf->dataBuffer, "could not allocate data buffer", 0);
  // set vector to null
  bdf->meanVector = NULL;
  return 1;
}

void
DeleteBinaryDataFile (BinaryDataFile bdf)
{
  delete(bdf->pWeights);
  delete(bdf->dataBuffer);
  if (bdf->meanVector) DeleteVector(bdf->meanVector);
  delete(bdf);
}

/********************** Text Utilities  ******************************************/

// extern int strcpy(char *, const char *);
// extern int strcspn(char *, const char *);
// extern int strcat(char *, const char *);

/*
static void GetFileNamePrefix (char *fileName, char *prefix);

static void GetFileNamePrefix (char *fileName, char *prefix)
{
	int nameLen;
	char *dot = ".";

	strcpy(prefix, fileName);
	nameLen = (int) strcspn(prefix, dot);
	prefix[nameLen] = '\0';
}
*/

char *DataFileNameBDF (BinaryDataFile bdf)
{
  strcpy (bdf->workingFileName, bdf->fileNamePrefix);
  strcat (bdf->workingFileName, ".bin");
  return bdf->workingFileName;
}

char *FileNameBDF (BinaryDataFile bdf)
{
  strcpy (bdf->workingFileName, bdf->fileNamePrefix);
  strcat (bdf->workingFileName, ".bdf");
  return bdf->workingFileName;
}

/**********************  buffer utilities  *************************************/    

static inline float DataBufferElementBDF (BinaryDataFile bdf, long index)
{
  long p1, p2;
  PredictorsUsedInColumnBDF(bdf, index, &p1, &p2);
  if (p2)
    return (bdf->dataBuffer[p1] * bdf->dataBuffer[p2]);
  else
    return (bdf->dataBuffer[p1]);
}


/********************************************************************************/    


BinaryDataFile MakeBinaryDataFile (char *path, char *filePrefix,
				   int hasWeights, int useInteractions)
{
  BinaryDataFile bdf;
  char inputFileName[MAX_BDF_NAME];
  double *wts; 
  int cont, outCont;
  long i, nChars, line;
  char *binData;
  FILE *input, *output;
    
  bdf = new aBinaryDataFile;
  ExitOnNull (bdf, "unable to allocate binFile.", NULL);
  bdf->useInteractions = useInteractions;
  bdf->sumWeights = 0.0;
  if (strlen(path)+10 >= MAX_BDF_NAME)
  {	fprintf(stderr, "BDF: name + path too long.\n");
	return NULL;
  }
  else
  { strcpy(bdf->fileNamePrefix, path);
    strcat(bdf->fileNamePrefix, filePrefix);
  }
  strcpy (inputFileName,path);
  strcat (inputFileName,filePrefix);
  strcat (inputFileName,".dat");
  
  // Count the number of lines and data columns
  bdf->nFileColumns = CountFields (inputFileName, 1);
  if (hasWeights) --(bdf->nFileColumns);
  if (bdf->nFileColumns <= 0)
  {	fprintf(stderr, "BDF: found no data in %s; return code %ld.\n",
		inputFileName, bdf->nFileColumns);
	delete(bdf);
	return NULL;
  }
  else printf("BDF: found %ld cols in %s, continuing.",
	      bdf->nFileColumns, inputFileName);
  bdf->nFileLines = CountLines (inputFileName);
  // allocate and check
  if (0 == AllocateBDF (bdf))
    return NULL;
  // Open input text file as read only, output binary file for writing.
  input = fopen(inputFileName, "r");
  if (input == NULL)
  {	fprintf(stderr, "FileOps: could not open input text file.\n");
	delete(bdf);return NULL;	
  };
  output = fopen(DataFileNameBDF(bdf), "w+");
  if (output == NULL)
  {	fprintf(stderr, "FileOps: could not open output binary file.\n");
	fclose(input);
	delete(bdf);return NULL;	
  };
  // Read input line, writing collection of values when accumulated.
  //   Assumes that the input file has equal num of cols per line. 
  //   Store weights to vector if present.
  wts = bdf->pWeights;
  nChars = bdf->nFileColumns * sizeof(float);
  for (line = 0; line < bdf->nFileLines; ++line)
  {	if (bdf->pWeights) cont = fscanf(input, "%lf", wts++);
	for (i=0; i < bdf->nFileColumns; ++i)
	  cont = fscanf(input, "%f", &bdf->dataBuffer[i]);
	binData = (char *) bdf->dataBuffer;
	for (i=0; i < nChars; ++i)
	  outCont = fputc (binData[i], output);
  }
  // close files, and return
  fclose(input);
  fclose(output);
  return bdf;
}

BinaryDataFile CopyBinaryDataFile (BinaryDataFile bdf)
{
  BinaryDataFile copy;
  double *pWts, *pCopyWts;
  long i;
  
  copy = new aBinaryDataFile;
  ExitOnNull (copy, "unable to allocate bdf", NULL);
  copy->useInteractions  = bdf->useInteractions;
  copy->sumWeights = bdf->sumWeights;
  strcpy(copy->fileNamePrefix, bdf->fileNamePrefix);
  copy->nFileColumns = bdf->nFileColumns ;
  copy->nFileLines = bdf->nFileLines;
  if (0 == AllocateBDF (copy))
  { fprintf(stderr, "BDF: Could not allocate copy of data file.\n");
    return NULL;
  }
  // copy weights over
  pCopyWts = copy->pWeights;
  pWts = bdf->pWeights;
  for (i=0; i<copy->nFileLines; ++i)
    *pCopyWts++ = *pWts++;
  // copy over mean vector if exists
  if (bdf->meanVector)
    copy->meanVector = CopyVector(bdf->meanVector,"CopyBinaryDataFile");
  return copy;
}

Vector WeightVectorBDF(BinaryDataFile bdf)
{
  Vector wts;
  long i;
  double *pf, *pd;
  
  wts = NewVector(bdf->nFileLines, "WeightVectorBDF");
  pd = wts->ptr;
  pf = bdf->pWeights;
  for(i=0; i<bdf->nFileLines; ++i)
    *pd++ = (double) *pf++;	// this converts to double as well
  return(wts);
}
	
void	SetWeightVectorBDF(BinaryDataFile bdf, Vector newWeights)
{
  long i;
  double *pf;
  
  // flush old mean vector if exists
  if (bdf->meanVector)
  { DeleteVector(bdf->meanVector);
    bdf->meanVector = NULL;
  }
  // check lengths
  if (bdf->nFileLines != newWeights->len)
  { fprintf(stderr, "BDF: lengths do not agree in new weights.\n");
    return;
  }
  // copy over, converting to floats
  pf = bdf->pWeights;
  for (i=0; i<bdf->nFileLines; ++i)
    *pf++ = VectorElement(newWeights,i);
}

long nFileLinesBDF (BinaryDataFile bdf)
{
  return (bdf->nFileLines);
}

long nFileColumnsBDF (BinaryDataFile bdf)
{
  return (bdf->nFileColumns);
}

double	SumWeightsBDF (BinaryDataFile bdf)
{
  if (bdf->pWeights)
    return ((bdf->sumWeights));
  else
  {	fprintf(stderr, "BDF: Weights are not computed.\n");
	return (0.0);
  }
}


void PrintSummaryBDF (BinaryDataFile bdf)
{
  printf ("\n-----------------------  BDF file  ---------------------------\n");
  printf (" Prefix %s \n", bdf->fileNamePrefix);
  printf (" Dim [%5ld, %5ld] ", bdf->nFileLines, bdf->nFileColumns);
  if (bdf->useInteractions)
    printf ("with interactions.\n");
  else
    printf ("with NO interactions.\n");
  if (bdf->pWeights)
  { 	printf (" Weights ->  [0] %6.4f [1] %6.4f [2] %6.4f ... [%ld] %6.4f\n",
		bdf->pWeights[0], bdf->pWeights[1], bdf->pWeights[2], 
		bdf->nFileLines-1, bdf->pWeights[bdf->nFileLines-1]);
	printf (" Sum of weights is %12.4f\n", bdf->sumWeights);
  }
  else printf (" No weighting.\n");
  if (bdf->meanVector)
    CompactPrintVector(bdf->meanVector, " Mean vector");
  else printf (" Mean vector not allocated.\n");
  printf ("--------------------------------------------------------------\n");
}


/**************************  Open/Close  ************************************/

BinaryDataFile OpenBinaryDataFile (char *fileNamePrefix)
{
  BinaryDataFile bdf;
  int status;
  long line;
  char bdfName[MAX_BDF_NAME];
  double *pW;
  FILE *bdfFile;
  
  // open the .bdf file
  strcpy (bdfName, fileNamePrefix);
  strcat (bdfName, ".bdf");
  bdfFile = fopen (bdfName, "r");
  ExitOnNull (bdfFile, "could not open BDF file.", NULL);
  // alloc a bdf object, assign name
  bdf = new aBinaryDataFile;
  ExitOnNull (bdf, "unable to allocate binFile.", NULL);
  // read file name, dimensions
  status = fscanf(bdfFile, "%s\n", bdf->fileNamePrefix);
  status = fscanf(bdfFile, "[%ld,%ld]\n", &bdf->nFileLines, &bdf->nFileColumns);
  status = fscanf(bdfFile, "interact=%d", &bdf->useInteractions);
  // allocate
  if (0 == AllocateBDF(bdf)) return NULL;
  // read weights from file, one per line
  pW = bdf->pWeights;
  for (line = 0; line < bdf->nFileLines; ++line)
    status = fscanf(bdfFile, "%lf\n", pW++);
  // close file and exit
  fclose(bdfFile);
  return bdf;
}


void SaveBinaryDataFile (BinaryDataFile bdf)
{
  long line;
  char fileName[MAX_BDF_NAME];
  double *pW;
  FILE *bdfFile;
  
  // open the .bdf file
  strcpy (fileName, bdf->fileNamePrefix);  strcat (fileName, ".bdf");
  bdfFile = fopen (fileName, "w");
  if (bdfFile)
  {	// write file name prefix, dimensions
    fprintf(bdfFile, "%s\n", bdf->fileNamePrefix);
    fprintf(bdfFile, "[%ld,%ld]\n", bdf->nFileLines, bdf->nFileColumns);
    fprintf(bdfFile, "interact=%d\n", bdf->useInteractions);
    //  weights to file, one per line
    pW = bdf->pWeights;
    for (line = 0; line < bdf->nFileLines; ++line)
      fprintf(bdfFile, "%f\n", *pW++);
    // close file 
    fclose(bdfFile);
  } else fprintf (stderr, "FileOps: could not save to file.\n");
}

void PredictorsUsedInColumnBDF(BinaryDataFile bdf, long index, long *p1, long *p2)
{
  long nPreds = bdf->nFileColumns-1;	// number of 'basic' predictors
  
  *p2 = 0;
  if (index <= nPreds)				// simple predictor
    *p1 = index;
  else
  {	index -= nPreds;
	while (index + *p2 > nPreds)	// so that index + *x2 <= p when break
	{	index -= nPreds - *p2;
		++ *p2;
	}
	*p1 = index + *p2;
	++ *p2;
  }
}


Vector ColumnBDF(BinaryDataFile bdf, long column)
{	
  FILE *input;
  Vector data;
  long i;
  
  ExitOnNull(input = fopen(DataFileNameBDF(bdf), "r"), "Could not open file", NULL);
  ExitOnNull(data = NewVector(bdf->nFileLines, "ColumnBDF"),
	     "Could not allocate vector", NULL);
  if (column < bdf->nFileColumns)
    for (i=0; i<bdf->nFileLines; ++i)
    {	fread((void *)bdf->dataBuffer, sizeof(float),
	      (unsigned long) bdf->nFileColumns, input);
	AssignVectorElement(data,i,(bdf->dataBuffer)[column]);
    }
  else // interaction present
  {	long c1, c2;
	PredictorsUsedInColumnBDF(bdf, column, &c1, &c2);
	for (i=0; i<bdf->nFileLines; ++i)
	{	fread((void *)bdf->dataBuffer, sizeof(float),
		      (unsigned long) bdf->nFileColumns, input);
		AssignVectorElement(data,i,(bdf->dataBuffer)[c1]
				    * (bdf->dataBuffer)[c2]);
	}
  }
  fclose(input);
  return (data);
}



/*********************** Mean Vector ************************************/

#define LoadBufferBDF(bdf, file) fread((void *)(bdf->dataBuffer), sizeof(float), (unsigned long) bdf->nFileColumns, (file)) 

static void
LoadCenteredVectorFromBuffer (BinaryDataFile bdf, double* pV, long length)
{
  double* pM = bdf->meanVector->ptr;
  long last = length - 6;
  long i = 0;
  while (i < last) {
    *pV =  DataBufferElementBDF(bdf,i) - *pM; ++i; ++pV; ++pM;
    *pV =  DataBufferElementBDF(bdf,i) - *pM; ++i; ++pV; ++pM;
    *pV =  DataBufferElementBDF(bdf,i) - *pM; ++i; ++pV; ++pM;
    *pV =  DataBufferElementBDF(bdf,i) - *pM; ++i; ++pV; ++pM;
    *pV =  DataBufferElementBDF(bdf,i) - *pM; ++i; ++pV; ++pM;
  }
  while (i < length) {
    *pV = DataBufferElementBDF(bdf,i) - *pM;  ++i; ++pV; ++pM;
  }
}

long
NumberOfPredictorsBDF(BinaryDataFile bdf)
{
  long p;
  
  p = bdf->nFileColumns - 1;  		// number of X's
  if (bdf->useInteractions)
    return (p + p + (p * (p-1))/2);	// X + X^2 + X_i*X_j
  else
    return p;
}
		
long
NumberOfVariablesBDF(BinaryDataFile bdf)
{
  return (1+NumberOfPredictorsBDF(bdf));
}


Vector
MeanVectorBDF (BinaryDataFile bdf)
{
  long i, j, line, nMeans, index;
  double *pW;
  float *buffer;
  Vector means;
  FILE *dataFile;
  
  // return one if we have one, else compute it
  if (bdf->meanVector)
    return (bdf->meanVector);
  nMeans = 1 + NumberOfPredictorsBDF(bdf);
  // Check space for means, initialize buffer.
  ExitOnNull(means = NewVector(nMeans, "MeanVectorBDF"),
	     "Allocation of means failed.", NULL);
  // open file as read only
  ExitOnNull(dataFile=fopen(DataFileNameBDF(bdf), "r"),
	     "could not open data file", NULL);
  // initialize totals
  InitializeVector(means, 0.0);
  // loop over lines of the file
  bdf->sumWeights = 0.0;
  pW = bdf->pWeights;
  buffer = bdf->dataBuffer;
  for(line=0; line <bdf->nFileLines; ++line, ++pW)
  { LoadBufferBDF(bdf, dataFile);
    for (i=0; i < bdf->nFileColumns; ++i)
      IncrementVectorElement(means, i, *pW * buffer[i]);
    if (bdf->useInteractions)
    { index = bdf->nFileColumns;
      for (i=1; i < bdf->nFileColumns; ++i)	// start at one to skip Y
	for (j=i; j<bdf->nFileColumns; ++j)	// raw access - never index interact
	  IncrementVectorElement(means, index++, *pW * buffer[i] * buffer[j]);
    }
    bdf->sumWeights += *pW;
  }
  // normalize by sum of weights
  for (i=0; i<nMeans; ++i)
    DivideVectorElement(means, i, bdf->sumWeights);
  // close file, save pointer and return
  fclose(dataFile);
  return(bdf->meanVector = means);
}


Vector SSVectorBDF (BinaryDataFile bdf)
{	
  double dev;
  long i, j, line,  nSS, destIndex;
  double *pW;
  float *buffer;
  Vector ss, means;
  FILE *dataFile;
  
  nSS = NumberOfVariablesBDF(bdf);
  // check space for ss
  ExitOnNull(ss = NewVector(nSS, "SSVectorBDF"), "Allocation of SS failed.", NULL);
  // open file as read only
  ExitOnNull(dataFile=fopen(DataFileNameBDF(bdf), "r"), "could not open data file", NULL);
  // get means
  means = MeanVectorBDF(bdf);
  // initialize, then loop over lines of the file
  InitializeVector(ss, 0.0);
  pW = bdf->pWeights;
  buffer = bdf->dataBuffer;
  for(line=0; line < bdf->nFileLines; ++line, ++pW)
  {
    LoadBufferBDF(bdf, dataFile);
    for (i=0; i < bdf->nFileColumns; ++i)
    {	dev = buffer[i] - VectorElement(means, i);
	IncrementVectorElement(ss, i, *pW * (dev * dev));
    }
    if (bdf->useInteractions)
    {	destIndex = bdf->nFileColumns;
	for (i=1; i < bdf->nFileColumns; ++i)		// start at one to skip Y
	{
	  for (j=i; j< bdf->nFileColumns; ++j)	// direct indexing here
	  { dev =  (buffer[i] * buffer[j]) - VectorElement(means, destIndex);
	    IncrementVectorElement(ss, destIndex, *pW * (dev * dev));
	    ++destIndex;
	  }
	}
    }
  }
  // Close file and return ss vector
  fclose(dataFile);
  return(ss);
}


Vector CPVectorBDF(BinaryDataFile bdf, long column)
{	
  double dev, mean;
  double *pW;
  float *buffer;
  long i, j, line, nCP, destIndex, p1, p2;
  Vector cp;
  FILE *dataFile;
  
  nCP = NumberOfVariablesBDF(bdf);
  // check output column
  if (column >= nCP)
  { fprintf(stderr, "BDF: column %ld does not exist; only have %ld.\n", column, nCP);
    return NULL;
  }
  // check space for cp
  ExitOnNull(cp = NewVector(nCP, "CPVectorBDF"), "allocation of CP failed.", NULL);
  // open file as read only
  ExitOnNull(dataFile=fopen(DataFileNameBDF(bdf), "r"),
	     "could not open data file", NULL);
  // get mean value
  mean = VectorElement(MeanVectorBDF(bdf), column);
  // initialize, then loop over lines of the file
  InitializeVector(cp, 0.0);
  pW = bdf->pWeights;
  buffer = bdf->dataBuffer;
  if (bdf->useInteractions)
  { PredictorsUsedInColumnBDF(bdf, column, &p1, &p2);
    for(line=0; line < bdf->nFileLines; ++line, ++pW)
    { 	LoadBufferBDF(bdf, dataFile);
	if (p2) 
	  dev = buffer[p1] * buffer[p2];
	else dev = buffer[p1];
	dev = dev - mean;
	for (i=0; i < bdf->nFileColumns; ++i)
	  IncrementVectorElement(cp, i, *pW * (dev * buffer[i]));
	destIndex = bdf->nFileColumns;
	for (i=1; i < bdf->nFileColumns; ++i)	// start at one to skip Y
	{
	  for (j=i; j < bdf->nFileColumns; ++j)
	  { IncrementVectorElement(cp, destIndex, *pW * (dev*buffer[i]*buffer[j]));
	    ++destIndex;
	  }
	}
    }
  }
  else
    for(line=0; line < bdf->nFileLines; ++line, ++pW)
    { 	LoadBufferBDF(bdf, dataFile);
	dev = buffer[column] - mean;
	for (i=0; i < bdf->nFileColumns; ++i)
	  IncrementVectorElement(cp, i, *pW * (dev * buffer[i]));
    }
  // Close file and return
  fclose(dataFile);
  return(cp);
}


int	CPArrayBDF(BinaryDataFile bdf, long *pCols, int nCols, Vector vecArray[])
{	
  double dev, incr;
  long i, j, k, line, col, destIndex, nCP;
  double *pW, *pVec;
  float *buffer;
  Vector means;
  FILE *dataFile;
  
  // open file as read only
  ExitOnNull(dataFile=fopen(DataFileNameBDF(bdf), "r"), "could not open data file", -1);
  // Check output space for resulting vector elements, initialize to zero
  nCP = NumberOfVariablesBDF(bdf);
  for (i=0; i<nCols; ++i)
  {	vecArray[i] = NewVector(nCP, "CPArrayBDF");
	ExitOnNull(vecArray[i], "Failed allocating covariance vector", -7);
	InitializeVector(vecArray[i], 0.0);
  }
  means = MeanVectorBDF(bdf);
  // Loop over lines of the file
  pW = bdf->pWeights;
  buffer = bdf->dataBuffer;
  if (bdf->useInteractions)
    for(line = 0; line < bdf->nFileLines; ++line, ++pW)
    { LoadBufferBDF(bdf, dataFile);
      for (k=0; k<nCols; ++k)
      {	col = pCols[k];
	pVec = vecArray[k]->ptr;
	dev = DataBufferElementBDF(bdf, col) - VectorElement(means, col);
	for (i=0; i < bdf->nFileColumns; ++i)	// include Y here
	  pVec[i] = pVec[i] + *pW * dev * buffer[i];
	destIndex = bdf->nFileColumns;
	for (i=1; i < bdf->nFileColumns; ++i)	// start at one to skip Y
	{
	  double buffer_i = buffer[i] * dev * *pW;
	  for (j=i; j<bdf->nFileColumns; ++j)
	  { incr = buffer_i * buffer[j];
	    if (incr > 1.0E25 || incr < -1.0E25)
	    { printf ("Working on dest %ld for col %ld\n", destIndex, col);
	      printf ("Incr %f out of range i=%ld, j=%ld, weight = %f, dev = %f\n",
		      incr,i,j,*pW,dev);
	    }
	    pVec[destIndex] = pVec[destIndex]+ incr;
	    ++destIndex;
	  }
	}
      }
    }
  else
    for (line = 0; line < bdf->nFileLines; ++line, ++pW)
    {	LoadBufferBDF(bdf, dataFile);  // extra bracket deleted here ras
	for (k=0; k<nCols; ++k)
	{ col = pCols[k];
	  pVec = vecArray[k]->ptr;
	  // center the value for column i
	  dev = buffer[col] - VectorElement(means, col);
	  // calc cross-prods with other cols
	  for (i=0; i < bdf->nFileColumns; ++i)
	    pVec[i] += *pW * dev * buffer[i];
	}
    }
  // Close file and return
  fclose(dataFile);
  return(nCols);
}


Matrix CPMatrixBDF (BinaryDataFile bdf, long *pCols, int nCols, Vector wts)
{
  Matrix cpMatrix;
  Vector means;
  double dev, incr, *pW;
  float *buffer;
  long i, j, line;
  FILE *dataFile;
  
  // open file as read only
  ExitOnNull(dataFile=fopen(DataFileNameBDF(bdf), "r"),
	     "could not open data file", NULL);
  // allocate result matrix
  ExitOnNull(cpMatrix = NewMatrix(nCols, nCols,"CPMatrixBDF"),
	     "could not allocate cp matrix", NULL);
  InitializeMatrix(cpMatrix, 0.0);
  // use wts if input, else default
  if (wts)
    pW = wts->ptr;
  else
    pW = bdf->pWeights;
  // loop over lines of the file
  buffer = bdf->dataBuffer;
  means = MeanVectorBDF(bdf);
  for (line = 0; line < bdf->nFileLines; ++line, ++pW)
  { LoadBufferBDF(bdf, dataFile);
    for (i=0; i < nCols; ++i)
    { dev = DataBufferElementBDF(bdf, pCols[i]) - VectorElement(means, pCols[i]);
      IncrementMatrixElement(cpMatrix, i, i, *pW * dev * dev);
      for (j=i+1; j < nCols; ++j)
      {	incr = *pW * dev * DataBufferElementBDF(bdf, pCols[j]);
	IncrementMatrixElement(cpMatrix, i, j, incr);
	IncrementMatrixElement(cpMatrix, j, i, incr);
      }
    }
  }
  // close file and return
  fclose(dataFile);
  return(cpMatrix);
}


/**************************  linear combinations  ***************************/

double SSOfLinearCombinationBDF(BinaryDataFile bdf, Vector coefs, long *coefIndex, double offset)
{
  double lpSS, lp;
  int i;
  long line;
  double *pW;
  FILE *dataFile;
  
  // open file as read only
  ExitOnNull(dataFile=fopen(DataFileNameBDF(bdf), "r"),
	     "could not open data file", 0.0);
  // Loop over lines of the file
  lpSS = 0.0;
  pW = bdf->pWeights;
  for (line = 0; line < bdf->nFileLines; ++line, ++pW)
  {	LoadBufferBDF(bdf, dataFile);
	lp = offset;
	for (i=0; i < coefs->len; ++i)
	  lp += VectorElement(coefs, i) * DataBufferElementBDF(bdf, coefIndex[i]);
	lpSS += *pW * (lp * lp);
  }
  // close file and return
  fclose(dataFile);
  return(lpSS);
}

static double
abs_val(double x)
{
  if (x >= 0.0) return x; else return -x;
}

static double
max_abs(double x, double y)
{
  double ax = abs_val(x);
  double ay = abs_val(y);
  if (ax >= ay) return ax; else return ay;
}


static double
dot_prod_col (double* pX, double** pBeta, long col, int len)
{
  double result = 0.0;
  int last = len - 6;
  int i = 0;
  while (i < last) {
    result = result + *pX * pBeta[i][col]; ++i; ++pX;
    result = result + *pX * pBeta[i][col]; ++i; ++pX;
    result = result + *pX * pBeta[i][col]; ++i; ++pX;
    result = result + *pX * pBeta[i][col]; ++i; ++pX;
    result = result + *pX * pBeta[i][col]; ++i; ++pX;
  }
  while (i < len) {
    result = result + *pX * pBeta[i][col]; ++i; ++pX;
  }
  return result;
}


int FillResidualSSBDF(BinaryDataFile bdf, int k, IndexedVector sweep[], int *skip,
		      Vector fit, Vector obsVariance,
		      Vector a, Vector b2)
{
  double **beta, *pW, *pVi, *pMu, z, absZ, *meanPtr, *aPtr, *b2Ptr;
  double *pX, *pZ;
  long line, j, p;
  int i, iCol;
  FILE *dataFile;
  
  p = VectorLength(b2);
  printf("BDF: Filling RSS, adjusting for %d prior predictors (of %ld)\n",  k, p-1);
  // open file as read only then set up pointers
  ExitOnNull(dataFile=fopen(DataFileNameBDF(bdf), "r"), "could not open bdf data file", -1);
  pW       = bdf->pWeights;                    // sampling weights
  pMu      = fit->ptr;
  pVi      = obsVariance->ptr;                 // variance wts for white estimator or binomial
  meanPtr  = bdf->meanVector->ptr;
  aPtr     = a->ptr;                           // poisson and se for bennett formula
  b2Ptr    = b2->ptr;
  // allocate local vectors
  pX = new double[k];
  pZ = new double[p];
  beta = new (double *)[k];      ExitOnNull(beta, "could not allocate space", -2);
  for(i=0; i<k; ++i)                           // get pointers into the sweep matrix for betas
    { beta[i] = sweep[i+1].theVector->ptr;       // skip over Y row
    // PrintRawVector(10,beta[i], "beta vector");
    }
  // initialize results
  InitializeVector(a , 0.0);
  InitializeVector(b2, 0.0);
  // loop over lines of the file
  for (line = 0; line < bdf->nFileLines; ++line, ++pW, ++pMu, ++pVi)
  { 
    LoadBufferBDF(bdf, dataFile);
    LoadCenteredVectorFromBuffer(bdf, pZ, p); // Unrolled
    // load up predictor vector for this obs with centered values
    for (i=0; i<k; ++i) {
      iCol = sweep[i+1].columnIndex;
      pX[i] = pZ[iCol]; // Unrolled // pX[i]=DataBufferElementBDF(bdf,iCol)-meanPtr[iCol]; // Rolled
    }
    // increment rss at each valid column ... bennett component does NOT handle weights
    for (j=0; j<p; ++j)
      if (!skip[j]) {
	// z = DataBufferElementBDF(bdf, j) - meanPtr[j]; Rolled
	// for (i=0; i<k; ++i)
	//   z -= pX[i] * beta[i][j];                       
	z = pZ[j] - dot_prod_col(pX,beta,j,k);             // sweep out included preds, x_i-x^_i
	absZ = abs_val(z) * max_abs(*pMu,1.0-*pMu);        // weighted by fit (x_i-x^_i)(y-y^)
	if (absZ > aPtr[j]) aPtr[j] = absZ;                // largest in this column?
	b2Ptr[j] = b2Ptr[j] + z * z * (*pVi);              // accum variance diagonal
      }
  }
  // close file and return
  fclose(dataFile);
  delete(pX); delete(pZ);
  delete(beta);
  return 1;
}
  



Vector LinearCombinationBDF(BinaryDataFile bdf, Vector coefs, long *coefIndex, double offset)
{
  Vector linComb;
  int i;
  long line;
  double *pW, lc;
  FILE *dataFile;
  
  // open file as read only
  ExitOnNull(dataFile=fopen(DataFileNameBDF(bdf), "r"),
	     "could not open data file", NULL);
  // allocate lin comb
  ExitOnNull(linComb = NewVector(bdf->nFileLines,"LinearCombBDF"),
	     "could not allocate linear combination", NULL);
  // loop over lines of the file
  pW = bdf->pWeights;
  for (line = 0; line < bdf->nFileLines; ++line, ++pW)
  {	LoadBufferBDF(bdf, dataFile);
	lc = offset;
	for (i=0; i < coefs->len; ++i)
	  lc += VectorElement(coefs, i) * DataBufferElementBDF(bdf, coefIndex[i]);
	AssignVectorElement(linComb, line, lc);
  }
  // close file and return vector
  fclose(dataFile);
  return(linComb);
}


int WriteColumnsToFileBDF(BinaryDataFile bdf, long *outputCols, int nCols,
			  char *outFileName)
{
  int i;
  long line, x1,x2, col;
  float *buffer;
  double *pW;
  FILE *dataFile, *outFile;
  
  // open data file as read only
  ExitOnNull(dataFile=fopen(DataFileNameBDF(bdf), "r"),
	     "could not open data file", -1);
  // open file for writing, deleting prior contents if any
  ExitOnNull(outFile=fopen(outFileName,"w"),"could not open output file", -2);
  // write column header line
  fprintf(outFile, "Weights ");
  for(i=0; i<nCols; ++i)
  { col = outputCols[i];
    PredictorsUsedInColumnBDF(bdf, col, &x1, &x2);
    if (x2)
      fprintf(outFile, "C%ld C%ld C%ld-%ld ", x2, x1, x2, x1);
    else
      fprintf(outFile, "C%ld ", x1);
  }
  fprintf(outFile, "\n");
  // loop over lines of the file
  buffer = bdf->dataBuffer;
  pW = bdf->pWeights;
  for (line = 0; line < bdf->nFileLines; ++line, ++pW)
  { LoadBufferBDF(bdf, dataFile);
    // put the weight in first column
    fprintf (outFile, "%8.5f ", *pW);
    // now put the interactions and base terms as needed
    for (i=0; i<nCols; ++i)
    { col = outputCols[i];
      PredictorsUsedInColumnBDF(bdf, col, &x1, &x2);
      if (x2)
	fprintf(outFile, "%f %f %f ", bdf->dataBuffer[x2], bdf->dataBuffer[x1],
		bdf->dataBuffer[x1] * bdf->dataBuffer[x2]);
      else
	fprintf(outFile, "%f ", bdf->dataBuffer[x1]);
    }
    fprintf(outFile, "\n");
  }
  // close files and return
  fclose(dataFile);
  fclose(outFile);
  return(nCols);
}
