// $Id: sweepMatrix.c,v 1.1 2005/06/13 20:47:51 bob Exp $

/* 
    7 Dec 00 ... Add the qBaseValue slot.
    7 Nov 00 ... Replace tagged lists with maps.
   28 Oct 00 ... Start adding parts written in C++ (sorter.c code)
    6 Aug 00 ... Fix search to use WLS, but evaluate correct F stat.
    3 Aug 00 ... Add code for the managing two sweeps, incremental and full
   16 Jul 99 ... Read variable names from file, with names as Y0, X1, X2, ... , Xm.
   14 Jul 99 ... Fix bugs in backtracking.
   11 Jun 99 ... Indexing allows interactions, noting leading cov is for resp Y.
   13 Nov 98 ... Adjustments for copying elements from cov cache, building X'X
    6 Nov 98 ... Fix bugs in initialization and finding next variables.
*/
 
// Use to check length of retained file name
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <iostream.h>

#include "sorter.h"

#include "sweepMatrix.h"
#include "binaryDataFile.h"
#include "fileOps.h"
#include "memory.h"


#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#ifndef SQR
#define SQR(x) ((x) * (x))
#endif

#ifndef MIN
#define MIN(x,y) (((x) < (y)) ? (x) : (y))
#endif

// ----- Cache size -----

#define CACHE_UPDATE_SIZE    5
#define MAX_TO_ADD         200

/********************************************************************************/

#define ExitOnNull(ptr,str,result) if(!(ptr)){fprintf(stderr,"\nSM: %s\n",str);return (result);}

#define ExitVoidOnNull(ptr,str) if(!(ptr)){fprintf(stderr,"\nSM: %s\n",str);return;}

/*********************** Internal functions *************************************/ 

static int  	Initialize(SweepMatrix sm);

static void 	MarkConstantColumns (SweepMatrix sm);	// flags small ResSS

static void 	FindNextVariables (SweepMatrix sm, Sort_tree *pTree);

static int 	UpdateCache(SweepMatrix sm, long *columns, int nCols, int addAll);

static int  	AddCrossProductToSweep (SweepMatrix sm, long column); //in next row

static int      AdjustNumberToAdd(SweepMatrix sm, int nToAdd);

static void	SweepLastRow(SweepMatrix sm);
static void	SweepOut    (long k, Vector CPk, long j, Vector CPj);
static void	ToPivotForm (long j, Vector CPj);
static void	AdjustRSS   (long j, Vector CPj, SweepMatrix sm);

static void	RestoreCovarY (SweepMatrix sm);

static int	MemberOfVector (long element, long *vector, long len);

static char     gFileNameBuffer[SWEEP_NAME_LEN];
static char*    MakeFileName(SweepMatrix sm, char *suffix);

// ------------------------------------------------------------------------------

static int
MemberOfVector (long element, long *vec, long len)
{
  long i;
  
  i = 0;
  while (*vec != element && i < len)	
  {	++i;
	++vec;
  }
  if (i < len)
    return 1;
  else
    return 0;
}

static char*
MakeFileName(SweepMatrix sm, char *suffix)
{
  strcpy(gFileNameBuffer, sm->path);
  strcat(gFileNameBuffer, suffix);
  // fprintf(stderr, "MakeFileName -> %s\n", gFileNameBuffer);
  
  return gFileNameBuffer;
}


// -------------------------------------------------------------------------
	
static int
Initialize(SweepMatrix sm)
{
  BinaryDataFile bdf;
  int result = 0;
  long i, nCov;
  
  bdf = sm->bdf;
  sm->varNames = NULL;
  nCov = NumberOfVariablesBDF(bdf);
  printf ("\nInitializing  skip vector with %ld columns.\n", nCov);
  sm->skipVector =  new int[nCov];
  ExitOnNull(sm->skipVector,  " skip alloc failed", -1);
  sm->skipVector[0] = TRUE;
  for (i=1; i<nCov; ++i)
    sm->skipVector[i] = 0;
  sm->nSkip = 1;
  printf ("Initializing  SS...\n");
  ExitOnNull(sm->rawSS = SSVectorBDF (bdf), "problem in init SS", -2);
  ExitOnNull(sm->residualSS = CopyVector (sm->rawSS, "Initialize"),
	     " problem in rss", -3);
  // these are initialized to zero by routine that fills them
  ExitOnNull(sm->rWWr = NewVector(nCov, "Init sweep mat"), "cannot alloc", -2);
  printf("Initializing covariances...");
  // first init indexed variable arrays
  for (i=0; i<SWEEP_MAX_COLUMNS; ++i)
  { sm->incrSweepMat[i].columnIndex = -1;
    sm->incrSweepMat[i].theVector = NULL;
    sm->fullSweepMat[i].columnIndex = -1;
    sm->fullSweepMat[i].theVector = NULL;
  } 
  sm->nextRow = 0;
  result = AddCrossProductToSweep (sm, 0);
  if (result <= 0)
  {	printf ( " failed (code = %d).\n", result);
	return (-6);
  } else
  { // Initialize the full sweep matrix
    CopyIndexedVector(&(sm->incrSweepMat[0]), &(sm->fullSweepMat[0]));
  }
  sm->qBase = 0;
  printf( " done. \n");
  return 1;
}


/*
  Insures that the cache has the best cross-product.  If the best one
  (in first position of best columns) is not in cache, then it will add
  that member as well as the others in best columns which are not
  already present.
*/
static int
UpdateCache(SweepMatrix sm, long *columns, int nCols, int addAll)
{
  int nToAdd, i, nCalculated;
  long  bestCol, colsToAdd[MAX_TO_ADD];
  Vector  vecsToAdd[MAX_TO_ADD];

  if (nCols > MAX_TO_ADD)
  { fprintf (stderr, "SM: Cannot add %d predictors at once; reset to %d\n",
	     nCols, MAX_TO_ADD);
    nCols = MAX_TO_ADD;
  }
  bestCol = columns[0];
  map<long,Vector>::iterator it = sm->CPCache.find(bestCol);
  if (addAll && it != sm->CPCache.end())
  { fprintf(stderr, "SM: Cache has best column %ld; no file access.\n", bestCol);
    return 0;
  } else  // find the ones not in the cache, and queue up
  { colsToAdd[0] = bestCol;
    nToAdd=1;
    i = 0;
    while (++i < nCols)
    { it = sm->CPCache.find(columns[i]);
      if (it == sm->CPCache.end())
	colsToAdd[nToAdd++] = columns[i];
    }
    fprintf(stderr, "SM: Cache file access adds %d cross-products for (", nToAdd);
    for (i=0; i<nToAdd; ++i)
      fprintf(stderr, " %ld", colsToAdd[i]);
    fprintf (stderr, " )\n");
    nCalculated = CPArrayBDF(sm->bdf, colsToAdd, nToAdd, vecsToAdd);
    if (nCalculated != nToAdd)
    { fprintf(stderr, "SM: Failed in calculations to update cache.\n");
      return -1;
    }
    for (i=0; i<nToAdd; ++i)
      sm->CPCache.insert(make_pair(colsToAdd[i],vecsToAdd[i]));
    return nToAdd;
  }
}


static void MarkConstantColumns (SweepMatrix sm)
{
  long i, nCov;
  
  fprintf (stderr, "SM: Marking constant columns.\n");
  nCov = NumberOfVariablesBDF(sm->bdf);
  for (i=1; i<nCov; ++i) // skip y in col 0
  {
    if ((FALSE == sm->skipVector[i]) &&
	(VectorElement(sm->residualSS,i) < SWEEP_NEAR_ZERO))
    {
      sm->skipVector[i] = TRUE;
      ++sm->nSkip;
      /* fprintf(stderr, "    Marking column %ld as constant.\n", i); */
    }
  }
}



static int
AddCrossProductToSweep (SweepMatrix sm, long column)
{
  printf("SM: Adding cross-products for variable #%ld to sweep matrix\n", column);
  sm->incrSweepMat[sm->nextRow].columnIndex = column;

  Vector CP;
  map<long,Vector>::iterator it = sm->CPCache.find(column);
  if (it != sm->CPCache.end())
    CP = it->second;
  else
  { fprintf (stderr, "\nSM: cache miss for column %ld; calc from file\n", column);
    CP = CPVectorBDF(sm->bdf, column);
    ExitOnNull(CP, "covariance failed", 0);
    sm->CPCache[column] = CP;
  }                                                        // 13 Nov
  sm->incrSweepMat[sm->nextRow].theVector = CopyVector(CP, "AddCrossProductToSweep"); 
  ++sm->nextRow;
  return 1;
}


static void
SweepLastRow(SweepMatrix sm)     // Remove previous predictors from this one
{
  int row, newRow;
  long j, k;
  Vector CPj, CPk;
  
  newRow = sm->nextRow-1;
  // j denotes column associated with latest row added to sweep
  j   = sm->incrSweepMat[newRow].columnIndex;
  CPj = sm->incrSweepMat[newRow].theVector;
  // Skip j in the future
  if (sm->skipVector[j])
    fprintf(stderr,"SM ERROR: Sweeping on previously swept column #%ld\n", j);
  else
  { sm->skipVector[j] = TRUE;
    ++sm->nSkip;
  }
  // Sweep out prior variables, excluding y in row 0
  // Prior rows are held in sweep pivot form (remove "k" from "j")
  for (row=1; row<newRow; ++row)
  { k = sm->incrSweepMat[row].columnIndex;
    CPk = sm->incrSweepMat[row].theVector;
    SweepOut(k, CPk, j, CPj);
  }
  // Convert the added row to sweep/pivot form
  ToPivotForm(j, CPj);
  // Adjust residual SS after conversion to pivot form
  AdjustRSS (j, CPj, sm);
  // Sweep new row out of Y in incremental and all in fully swept
  SweepOut(j, CPj, 0, sm->incrSweepMat[0].theVector);
  CopyIndexedVector(&(sm->incrSweepMat[0]), &(sm->fullSweepMat[0]));
  for (row = 1; row<newRow; ++row)
  { k = sm->fullSweepMat[row].columnIndex;
    CPk = sm->fullSweepMat[row].theVector;
    SweepOut(j, CPj, k, CPk);
  }  
  // Copy latest row to the fully swept matrix
  CopyIndexedVector(&(sm->incrSweepMat[newRow]), &(sm->fullSweepMat[newRow]));
}

static void	ToPivotForm (long j, Vector CPj)
{
  long i;
  double vjj;
  
  vjj = VectorElement(CPj, j);
  if (vjj < .00000001)
  { fprintf(stderr, "      pivot near zero.\n");
    return;
  }
  for(i=0; i<VectorLength(CPj); ++i)
    DivideVectorElement(CPj, i,  vjj);
  AssignVectorElement(CPj, j, (- (1.0 / vjj)));
}

static void AdjustRSS (long j, Vector CPj, SweepMatrix sm)
{
  double pivot;
  long i, nCov;
  
  pivot = VectorElement(CPj, j);   // CPj in pivot form
  IncrementVectorElement(sm->residualSS, 0, SQR(VectorElement(CPj, 0))/pivot);
  nCov = NumberOfVariablesBDF(sm->bdf);
  for (i=1; i < nCov; ++i)
    if (sm->skipVector[i])
      DecrementVectorElement(sm->residualSS, i, SQR(VectorElement(CPj, i))/pivot);
    else
      IncrementVectorElement(sm->residualSS, i, SQR(VectorElement(CPj, i))/pivot);
  // Set associated diagonal value held in residual CP to pivot
  AssignVectorElement(sm->residualSS, j, -VectorElement(CPj, j));
}

// k from j, assuming k is in "sweep-prepared" form
static void SweepOut (long k, Vector CPk, long j, Vector CPj)
{
  long i;
  double vjk;
  
  fprintf (stderr, "        Sweeping var %ld from %ld.\n", k, j);
  vjk = VectorElement(CPj, k);
  for (i=0; i<VectorLength(CPk); ++i)
    DecrementVectorElement(CPj,i, vjk * VectorElement(CPk,i));
  AssignVectorElement(CPj, k, - (vjk * VectorElement(CPk, k)));
}


// -----------------------------------------------------------------------------

SweepMatrix
NewSweepMatrix(char *path, BinaryDataFile bdf, BinaryDataFile validationBDF)
{	
  int result;
  SweepMatrix sm;
  
  // grab initial space
  ExitOnNull(sm = new aSweepMatrix, "cannot allocate sweep matrix.", NULL);
  // store path, bdf pointer
  strcpy(sm->path, path);
  sm->bdf = bdf;
  sm->validationBDF = validationBDF;
  // store the raw sampling weights from the file
  sm->rawWeights = WeightVectorBDF(sm->bdf);
  // initialize internal structures
  result = Initialize (sm);
  if (result <= 0) return NULL;
  fprintf (stderr, "\n-----\nNewSweepMatrix completed.\n-----\n");
  return sm;
}

void DeleteSweepMatrix (SweepMatrix sm)
{
  Vector v;
  long i, k, nVarNames;
  
  // delete name space used
  if (sm->varNames)
  {	nVarNames = nFileColumnsBDF(sm->bdf);	
	for (i=0; i<nVarNames;++i)
	  delete(sm->varNames[i]);
	delete(sm->varNames);
  }
  // delete vectors that are allocated by SM routines
  DeleteVector (sm->rawWeights);
  DeleteVector (sm->rawSS);
  DeleteVector (sm->residualSS);
  delete (sm->skipVector);
  // delete underlying matrix and cache
  k = NumberOfPredictors(sm);
  for (i=0; i<=k; ++i)
  { DeleteVector (sm->incrSweepMat[i].theVector);
    DeleteVector (sm->fullSweepMat[i].theVector);
  }
  for (map<long,Vector>::iterator it=sm->CPCache.begin(); it!=sm->CPCache.end(); ++it)
  { v = it->second;
    if (v) DeleteVector(v);
  }
  // NEED to delete the cache itself with a better routine
  delete(sm);
}

SweepMatrix	ReweightedSweepMatrix (SweepMatrix sm, Vector weights)
{
  SweepMatrix newSM;
  long *pCols;
  int result, nAdded, nPreds;
  
  // grab initial space
  ExitOnNull(newSM = new aSweepMatrix, "cannot allocate rw sweep matrix.", NULL);
  // copy path and bdf, then reweight the bdf
  strcpy(newSM->path, sm->path);
  newSM->bdf = CopyBinaryDataFile(sm->bdf);
  SetWeightVectorBDF(newSM->bdf, weights);
  newSM->validationBDF = CopyBinaryDataFile(sm->validationBDF);
  // store the raw sampling weights from other sweep matrix
  newSM->rawWeights = CopyVector(sm->rawWeights, "ReweightedSweep");
  // initialize internal structures
  result = Initialize (newSM);
  if (result <= 0) return NULL;
  // add the predictors used in prior sweep matrix
  pCols = ColumnsUsedInModel (sm);
  nPreds = NumberOfPredictors (sm);
  nAdded = AddPredictorsToModel(newSM, pCols+1, nPreds, FALSE); // +1 skips Y (col 0)
  if (nAdded != nPreds)
    fprintf (stderr,
	     "SM: number added to model (%d) not number requested (%d)\n",
	     nPreds, nAdded); 
  delete(pCols);
  fprintf (stderr, "\n-----\nSM: ReweightedSweepMatrix completed.\n-----\n");
  return newSM;
}

SweepMatrix RestoreSweepMatrixFromFile (char *fileName)
{	
  SweepMatrix sm;
  Vector v;
  char nameBuffer[SWEEP_NAME_LEN];
  long i, index, k, p;
  FILE *file;
	
  // alloc initial space, open file
  ExitOnNull (sm = new aSweepMatrix, "could not allocate sweep matrix", NULL);
  ExitOnNull (file = fopen(fileName, "r"),
	      "could not open file for restore.", NULL);
  // set up bdf, checking for validation file
  fscanf(file, "%s\n", sm->path);
  fscanf(file, "%s\n", nameBuffer); 
  printf("BDF prefix is %s\n", nameBuffer);
  ExitOnNull (sm->bdf = OpenBinaryDataFile(nameBuffer),
	      "could not open BDF", NULL);
  // zero until initilized
  printf("BDF reports %f observations.\n", SumWeightsBDF(sm->bdf));  
  fscanf(file, "%s\n", nameBuffer);
  if ('0' == nameBuffer[0])
    printf ("SM: No validation file was found.\n");
  else	ExitOnNull (sm->validationBDF = OpenBinaryDataFile(nameBuffer),
		    "could not open validation BDF", NULL);
  // restore variable names
  fscanf(file, "%s\n", nameBuffer);
  if ('0' == nameBuffer[0])
    printf ("SM: No variable name file was found.\n");
  else
    ReadVariableNamesFromFile (sm);
  // read SS vectors, skip vector
  ExitOnNull(sm->rawSS = RestoreVectorFromFile (file, "RestoreSMFromFile"),
	     "could not recover raw SS", NULL);
  ExitOnNull(sm->residualSS = RestoreVectorFromFile (file, "RestoreSMFromFile"),
	     "could not recover resid SS", NULL);
  p = NumberOfPredictorsBDF(sm->bdf);
  printf ("BDF file shows %ld predictors; getting skip values\n", p);
  sm->skipVector = new int[p+1];  // +1 for Y
  for (i=0, sm->nSkip=0; i<=p; ++i)
  { fscanf(file, "%d ", &sm->skipVector[i]);
    if ((sm->skipVector[i] < 0) || (sm->skipVector[i] > 1))
      printf ("Error: skip vector [%ld] = %d\n", i, sm->skipVector[i]);
    sm->nSkip += sm->skipVector[i];
  }
  // restore partial and marginal covariances
  fscanf (file, "k=%ld\n", &k);
  printf("Preparing to read Y and %ld predictors:\n", k);
  sm->nextRow = k+1;
  // Need to new the cache map???
  for (i=0; i<=k; ++i)
  { fscanf(file, "index=%ld\n", &index); printf (" %ld", index);
    sm->incrSweepMat[i].columnIndex = index;
    sm->incrSweepMat[i].theVector = RestoreVectorFromFile (file, "RestoreSMFromFile");
    ExitOnNull(v = RestoreVectorFromFile (file, "RestoreSMFromFile"),
	       "could not restore cp vector", NULL);
    sm->CPCache[index] = v;
  }
  fclose(file);
  printf ("... done\n");
  return sm;	
}


int SaveSweepMatrixToFile (SweepMatrix sm, char *fileName)
{
  long i, var, k, p;
  FILE *file;
  
  p = NumberOfPredictorsBDF(sm->bdf);
  k = NumberOfPredictors(sm);
  ExitOnNull (file = fopen (MakeFileName(sm, fileName), "w"),
	      "could not open file for save.", 0);
  // write path
  fprintf (file, "%s\n", sm->path);
  // if validation file, use it; otherwise write a null (zero)
  if (sm->validationBDF)
    fprintf (file, "%s\n", sm->validationBDF->fileNamePrefix);
  else
    fprintf (file, "%d\n", 0);
  // check for var names and save file if have names
  if (sm->varNames)
    fprintf (file, "%s\n", "names");
  else
    fprintf(file,"%d\n", 0);
  // save SS vectors and skip vector
  SaveVectorToFile (sm->rawSS, file);
  SaveVectorToFile (sm->residualSS, file);
  for (i=0; i<p+1; ++i)		// +1 for Y
    fprintf (file, "%d ", sm->skipVector[i]);
  fprintf (file, "\n");
  // write the number of predictors in use
  fprintf(file, "k=%ld\n", k);
  // save the rows of the sweep matrix itself along with CP vectors
  for (i=0; i<=k; ++i)
  { var = sm->incrSweepMat[i].columnIndex;
    fprintf (file, "index=%ld\n", var);
    SaveVectorToFile (sm->incrSweepMat[i].theVector, file);
    SaveVectorToFile(sm->CPCache[var], file);
  }
  fclose(file);
  return 0;
}


// -------------------------------  Fitting functions  --------------------------

static int AdjustNumberToAdd(SweepMatrix sm, int nToAdd)
{	
  long nPred, nCov;

  //  current size of the swept model
  nPred = NumberOfPredictors(sm);
  nCov = NumberOfVariablesBDF(sm->bdf);
  //  check that we do not run out of space
  if ((nToAdd + nPred) > SWEEP_MAX_COLUMNS)
  { nToAdd = SWEEP_MAX_COLUMNS - nPred;
    fprintf(stderr,
	    "SM: attempt to exceed max number cols; reset to %d\n", nToAdd);
  }
  //  make sure file has this many columns of data
  if ((nToAdd + nPred) > (nCov-1))
  { nToAdd = nCov - 1 - nPred;
    fprintf(stderr,
	    " SM: not enough covs to add this many; reset to %d\n", nToAdd);
  }
  return (nToAdd);
}


int
AddPredictorsToModel(SweepMatrix sm, long *pCols, int nToAdd, int forced)
{	
  int count;
  
  nToAdd = AdjustNumberToAdd(sm, nToAdd);
  count = nToAdd;
  if (UpdateCache(sm, pCols, nToAdd, 0) < 0) return -1;
  while (count > 0)
  { // Mark columns with no variation prior to search
    MarkConstantColumns(sm);
    // Add next predictor from input list
    AddCrossProductToSweep(sm, *pCols++);
    // Sweep out effects from row just added
    SweepLastRow(sm);
    --count;
    // PrintCurrentFit (sm);
  }
  if (forced) sm->qBase += nToAdd;
  return nToAdd;
}

int
ExpandStepwiseModel(SweepMatrix sm, int nToAdd)
{	
  int nAddedSoFar = 0;
  long bestVars[CACHE_UPDATE_SIZE], bestVarToUse;
  int i, gotOne;
  
  nToAdd = AdjustNumberToAdd(sm, nToAdd);
  while (nAddedSoFar < nToAdd)
  { /* this is for hard threshold
      Key_sorter::s_threshold = 
      2.0 * log(((double)NumberOfPossiblePredictors(sm)-(double) sm->qBase));
      fprintf (stderr, "SM: Hard threshold F = %6.2f.\n", Key_sorter::s_threshold);
    */
    Key_sorter::s_threshold = 
      2.0 * log( ((double)NumberOfPossiblePredictors(sm)-(double) sm->qBase)/
		 (1.0 + (double)NumberOfPredictors(sm) - (double) sm->qBase));
    fprintf (stderr, "SM: Adaptive threshold F = %7.3f with q base at %d\n",
	     Key_sorter::s_threshold,sm->qBase);
    // Mark columns with no variation prior to search
    MarkConstantColumns(sm);
    // Init tree so that have a new tree each time in loop (dont fill old one)
    Sort_tree tree;
    FindNextVariables(sm, &tree);
    fprintf (stderr, "SM: Moving var indices, updating cache.\n");
    Sort_tree::iterator treeIt;
    for (i=0, treeIt=tree.begin(); i<CACHE_UPDATE_SIZE; ++i, ++treeIt)
      bestVars[i] = treeIt->second;
    if (UpdateCache(sm, bestVars, CACHE_UPDATE_SIZE, 1) < 0) return -1;
    // Find column with highest partial covariance (starting with last)
    // and check that it has a large F stat
    fprintf (stderr, "SM: Checking found predictor corr.\n");
    treeIt = tree.begin();
    Key bestKey = treeIt->first;
    bestVarToUse = treeIt->second;
    if (bestKey.second > Key_sorter::s_threshold)
      gotOne = 1;
    else gotOne = 0;
    if (!gotOne)
      fprintf (stderr, "SM: *** Did not find variable meeting F rule *** \n");
    AddCrossProductToSweep(sm, bestVarToUse);
    // Sweep out effects from row just added
    SweepLastRow(sm);
    ++nAddedSoFar;
    // PrintCurrentFit (sm);
  }
  return nAddedSoFar;
}


static long stepColumns[SWEEP_MAX_COLUMNS];
static long flagColumns[SWEEP_MAX_COLUMNS];

int TrimStepwiseModel(SweepMatrix sm, int nToRemove)
{
  int i, j, nPred;
  Matrix CP;
  
  nPred = NumberOfPredictors(sm);
  if (nToRemove >= nPred)
  { fprintf(stderr, " *** Cannot remove %d vars; only have %d.\n",
	    nToRemove, sm->nextRow-1);
    return 0;
  }
  else
  { CP = CrossProductMatrix(sm, 0);
    if (!CP)
    {	fprintf(stderr, "Cannot build X'X matrix.\n");
	return 0;
    } else
    {	// find predictors to remove, leaving in stepColumns
      ForwardStepwise (CP, nPred, stepColumns);
      ReverseStepwise (CP, nToRemove, stepColumns);
      DeleteMatrix(CP);
      fprintf(stderr,
	      "SM: Vars chosen to remove by stepwise matrix operation are: \n");
      for (i=0; i<nToRemove; ++i)
	fprintf(stderr,"%5ld, ", stepColumns[i]);
      //  reset elements in the skip vector and its total
      for (i=0; i<nToRemove; ++i)
      {	j = sm->incrSweepMat[stepColumns[i]].columnIndex;
	if (!sm->skipVector[j]) 
	{  fprintf (stderr, "SM:  ***  error  *** \n");
	   fprintf (stderr, "   Consistency error in skip vector.\n");
	}
	sm->skipVector[j] = FALSE;
	-- sm->nSkip;
      }
      //  mark predictors in sm to remove, saving index in others
      for (j=1; j<=nPred; ++j)                                 	// 14 Jul 99
      {	if (MemberOfVector(j, (long *)&stepColumns, nToRemove)) // Remove it
	flagColumns[j] = 0;
      else
	flagColumns[j] = sm->incrSweepMat[j].columnIndex;
      }
      //  skip over predictors that do not require altering until reach zero
      j = 1;
      while (flagColumns[j]) ++j;
      sm->nextRow = j;
      //  delete old sweep rows, then rebuild *all* past this one from cache
      for (i=j; i<=nPred; ++i)
      {	sm->incrSweepMat[i].columnIndex = 0;
	DeleteVector(sm->incrSweepMat[i].theVector);
      }
      for (i=j; i<=nPred; ++i)
      {
	if (flagColumns[i])
	{ AddCrossProductToSweep(sm, flagColumns[i]);
	  SweepLastRow(sm);
	}
      }
      //  adjust first row of the sweep matrix and residual SS (diagonal)
      RestoreCovarY(sm);
      // WriteCurrentFit (sm);
      return nToRemove;
    }
  }
}

static void RestoreCovarY (SweepMatrix sm)
{
  int j, varIndex, nextRow;

  //  restore original SS vector
  DeleteVector (sm->residualSS);
  sm->residualSS = CopyVector(sm->rawSS, "RestoreCovarY");
  //  pull in original Cov(Y,Xj) vector, juggling nextRow index
  nextRow = sm->nextRow;
  sm->nextRow = 0;
  AddCrossProductToSweep(sm, 0);
  sm->nextRow = nextRow;
  //  sweep out each of the rows in the sweep from y and residual SS
  for (j=1; j<nextRow; ++j)
  { varIndex = sm->incrSweepMat[j].columnIndex;
    SweepOut(varIndex, sm->incrSweepMat[j].theVector, 0, sm->incrSweepMat[0].theVector);
    AdjustRSS (varIndex, sm->incrSweepMat[j].theVector, sm);
  }
}

// ----------------------------------------------------------------------------
//   Properties of the fitted model.
// ----------------------------------------------------------------------------

double RSquared(SweepMatrix sm)
{
  return(1.0 - VectorElement(sm->residualSS,0)/VectorElement(sm->rawSS, 0));
}

double SSquared(SweepMatrix sm)
{
  return(VectorElement(sm->residualSS,0)/
	 (double) DegreesOfFreedom(sm));
}

long NumberOfObservations (SweepMatrix sm)
{
  return(nFileLinesBDF(sm->bdf));
}

double NumberOfWeightedObservations (SweepMatrix sm)
{
  return(SumWeightsBDF(sm->bdf));
}

long NumberOfPredictors (SweepMatrix sm)
{
  return((long)sm->nextRow - 1);
}

long NumberOfPossibleBasePredictors(SweepMatrix sm) // excluding interactions
{
  return(nFileColumnsBDF(sm->bdf)-1);
}

long NumberOfPossiblePredictors (SweepMatrix sm)
{
  return(NumberOfPredictorsBDF(sm->bdf));
}


long
NumberOfCoefficients (SweepMatrix sm) 	// Includes constant.
{
  return(1+NumberOfPredictors (sm));
}

double
DegreesOfFreedom (SweepMatrix sm)
{
  // Lisp-Stat, Morrison (p 230) suggest N-k-1 divisor, but this is
  // biased in my simulations, so I use weighted count.
  return (double)(NumberOfWeightedObservations(sm) - sm->nextRow);
}


double MDL (SweepMatrix sm)
{
  double n,k;
  double regrSS;
  
  n = (double) NumberOfWeightedObservations(sm); // 9 Oct 00 Use weighted
  k = (double) NumberOfCoefficients(sm);
  regrSS = VectorElement(sm->rawSS, 0) - VectorElement(sm->residualSS,0);
  if (regrSS <=0 )
  {	fprintf(stderr,
		"*** SM: Regr SS %f <= 0; setting to 1. *** \n", regrSS);
	regrSS = 1;
  }
  return (  (n-k) * log(SSquared(sm))
	    + k * log(regrSS) 
	    + (n-k-1) * log(n/(n-k))
	    - (k+1) * log(k)   );
}


/*
  Use these macros to extract the right terms, with j denoting index
  of those in the sweep matrix, not the original data.  Make sure
  terms like s2 and sm are defined in the scope of these macros.
*/

#define COEF(j) (0==(j)) ? Intercept(sm) : VectorElement(sm->incrSweepMat[0].theVector, (j))
#define SE(j)   sqrt(s2 * VectorElement(sm->residualSS,(j)))


double Intercept (SweepMatrix sm)
{	
  int i,k;
  long col;
  double b0;
  Vector means;
  
  k = NumberOfCoefficients(sm);
  means = MeanVectorBDF(sm->bdf);
  // init to Y mean
  b0 = VectorElement(means, 0);
  // yBar - bHat * xBar
  for (i = 1; i < k; ++i)
  {	col = sm->incrSweepMat[i].columnIndex;
	b0 -= COEF(col) * VectorElement(means, col);
  }
  return b0;
}


RegrCoef EstimatedCoefficients (SweepMatrix sm)
{
  RegrCoef coefs;
  double s2;
  int j,k;
	
  k = NumberOfCoefficients(sm);
  coefs = new aRegrCoef[k];
  s2 = SSquared(sm);
  // Fill in slopes, skipping constant
  for (j=1; j<k; ++j)
  {	coefs[j].variable  = sm->incrSweepMat[j].columnIndex;
	coefs[j].coef = COEF(coefs[j].variable);
	coefs[j].se   = SE(coefs[j].variable);
  }
  coefs[0].variable = 0;
  coefs[0].coef = Intercept(sm);
  coefs[0].se = -1.0;
  return coefs;
}


Vector CoefficientVector (SweepMatrix sm)
{
  int i, k;
  Vector coefs;
  
  k = NumberOfCoefficients (sm);   // includes the intercept;
  coefs = NewVector(k, "CoefficientVector");
  AssignVectorElement(coefs,0, Intercept(sm));
  for (i=1; i<k; ++i)
    AssignVectorElement(coefs,i,COEF(sm->incrSweepMat[i].columnIndex));
  return(coefs);
}


double ValidationSS (SweepMatrix sm)
{
  Vector coefs;
  long *pCoefIndices, i;
  double intercept, SS;
	
  if (sm-> validationBDF)
  { coefs = CoefficientVector(sm);
    // Save intercept and insert -1 to pick up Y and subtract
    intercept = VectorElement(coefs,0);
    AssignVectorElement (coefs, 0, -1.0);
    // Locate the appropriate columns
    pCoefIndices = new long[coefs->len];
    for(i=0; i<coefs->len; ++i)
    {	pCoefIndices[i] = sm->incrSweepMat[i].columnIndex;
	// printf ("Column index %d is %d\n", i, pCoefIndices[i]);		
    }
    SS = SSOfLinearCombinationBDF(sm->validationBDF,coefs,pCoefIndices,intercept);
    DeleteVector(coefs);
    delete(pCoefIndices);
    return SS;
  }
  else return  -1.0;
}

long *ColumnsUsedInModel (SweepMatrix sm)
{
  long *ptr, i,k;
  
  k = NumberOfCoefficients(sm);
  ExitOnNull(ptr = new long[k], "Could not alloc index vector", NULL);
  // first element will be 0 denoting the response column
  for (i=0; i<k; ++i)
    ptr[i] = sm->incrSweepMat[i].columnIndex;
  return(ptr);
}

void WriteCurrentFitSummary (SweepMatrix sm)
{
  long k, p;
  double nObs;
  FILE *logFile;
  
  k = NumberOfPredictors(sm);
  p  = NumberOfPredictorsBDF(sm->bdf);
  nObs = SumWeightsBDF(sm->bdf);
  ExitVoidOnNull(logFile = fopen (MakeFileName(sm, "regr.log"), "a"),
		 "could not open log file");
  fprintf (logFile,
	   "------------------------------------------------------------------\n");
  fprintf (logFile,
	   "    k       p       n   R^2         S^2               Residual_SS        Validation_SS   MDL \n");
  fprintf (logFile,"* %3ld %8ld %6.1f %6.4f %15.8f %20.5f %20.5f %15.5f\n", 
	   k, p, nObs, RSquared(sm), SSquared(sm),
	   VectorElement(sm->residualSS,0), ValidationSS(sm),
	   MDL(sm));
  fprintf (logFile,
	   "------------------------------------------------------------------\n");
  fclose (logFile);
}

void WriteCurrentFit (SweepMatrix sm)
{
  Matrix homoCovMat, heteroCovMat;
  double s2, coef, se, wse;
  long j, k, xIndex, x1, x2, mm, nBasic;
  FILE *logFile;
  
  k = NumberOfCoefficients(sm);
  s2 = SSquared(sm);
  homoCovMat   = HomoCovMatOfEstimates(sm);
  heteroCovMat = HeteroCovMatOfEstimates(sm);
  nBasic  = nFileColumnsBDF(sm->bdf) - 1;
  ExitVoidOnNull(logFile = fopen (MakeFileName(sm, "regr.log"), "a"),
		 "could not open log file");
  fprintf (logFile, "--------------------------------------------------------\n");
  fprintf (logFile, " %ld predictors:\n     ", k-1);
  for(j=1; j<k; ++j)
    fprintf (logFile, "%ld ", sm->incrSweepMat[j].columnIndex);
  fprintf (logFile, "\n--------------------------------------------------------\n");
  fprintf (logFile, "Column            Coefficient        SE         t         wSE        wT\n");
  for (j=0; j<k; ++j)
  { xIndex = sm->incrSweepMat[j].columnIndex;
    coef = COEF(xIndex);
    // find nominal and weighted se
    if (0 == j) // does not handle constant term
    { se = -1.0; wse=-1.0;}
    else
    { se = sqrt(MatrixElement(homoCovMat, j-1, j-1));
      wse = sqrt(MatrixElement(heteroCovMat, j-1,j-1));
      if ((se < .00000000000001) || (wse < .000000000000001))
        fprintf(stderr, "SM: an se is very close to zero %f %f\n", se, wse);
    }
    IndexToFileColumns (xIndex, nBasic , &x1, &x2);
    if (x2) // interaction term
    {
      if (x1 > x2) // swap terms
      {	mm = x1; 
	x1 = x2; x2 = mm;
      }
      fprintf (logFile,"%6ld %6ld %15.6f %10.5f %10.2f %10.5f %10.2f",
	       x1,x2,  coef, se, coef/se, wse, coef/wse);
      if (sm->varNames)
	fprintf (logFile," %20s %20s\n", sm->varNames[x1], sm->varNames[x2]);
      else fprintf (logFile,"\n");
    }
    else 	// main effect
    { fprintf (logFile,"%6ld        %15.6f %10.5f %10.2f %10.5f %10.2f", 
	       xIndex, coef, se, coef/se, wse, coef/wse);
      if (sm->varNames)
	fprintf (logFile," %20s\n", sm->varNames[x1]);
      else fprintf (logFile,"\n");	
    }
  }
  fclose(logFile);
  WriteCurrentFitSummary (sm);
  DeleteMatrix(homoCovMat);
  DeleteMatrix(heteroCovMat);
}



long
WriteCoefficientsToFile (SweepMatrix sm)
{
  Vector coefs;
  long *pIndices;
  char name[12];
  long i,q;
  FILE *fp;

  q = NumberOfPredictors(sm);
  sprintf(name,"model.%03ld",q);  // zero pad prefix
  ExitOnNull (fp = fopen(MakeFileName(sm, name),"w"),
	      "could not open coef file", -1);
  fprintf(fp, "%ld\n", q);
  pIndices = new long[q+1];
  pIndices = ColumnsUsedInModel(sm); // 0 is in leading term
  coefs = CoefficientVector(sm);
  for(i=0; i<=q; ++i)  // one more for constant
    fprintf(fp, "%ld %e\n", pIndices[i], VectorElement(coefs,i));
  // Write suffix with p (not counting interactions nor weights)
  fprintf (fp, "%ld\n",NumberOfPossibleBasePredictors(sm));
  fclose(fp);
  delete(pIndices);
  DeleteVector(coefs);
  return(q);
}


void PrintSweepMatrix (SweepMatrix sm)
{
  long i, k, nCols, nCov;
  double nObs;
	
  nObs = SumWeightsBDF (sm->bdf);
  nCols= nFileColumnsBDF(sm->bdf);
  nCov = NumberOfVariablesBDF(sm->bdf);
  printf ("---------------------------------------------------\n");
  printf ("SWEEP MATRIX \n");
  printf ("Data file %s\n    %ld file columns, %f weighted observations, interact=%d\n",
	  sm->bdf->fileNamePrefix, nCols, nObs, sm->bdf->useInteractions);
  if (sm->validationBDF)
    printf ("Validation data file %s\n", sm->validationBDF->fileNamePrefix);
  if (sm->varNames)
  { printf ("Leading variable names: (from file %s)\n", "names");
    for (i=1;  i<((nCols < 10)?nCols:10); ++i)
      printf(" [%ld]%s", i, sm->varNames[i]);
    printf("\n");
  }
  CompactPrintVector (MeanVectorBDF(sm->bdf), "Means");
  CompactPrintVector (sm->rawSS, "Raw Sums of Squares");
  CompactPrintVector (sm->residualSS, "Residual Sums of Squares");
  printf ("Skip vector (%ld out of %ld are skipped)\n ", sm->nSkip, nCov);
  for (i=0; i<((nCov < 50)?nCov:50); ++i)
  {	printf ("%d", sm->skipVector[i]);
	if ((i>0) && (0 == (i % 10))) printf("|");
  }
  printf ("\n");
  cout << "Covariance cache holds:" << endl;
  for(map<long,Vector>::iterator it=sm->CPCache.begin(); it!=sm->CPCache.end(); ++it)
    cout << " [" << it->first << "]";
  cout << endl;
  printf("R^2 = %6.4f, s^2 = %15.6f\n", RSquared(sm), SSquared(sm));
  k =  NumberOfCoefficients(sm)-1;
  printf("Column    Incremental sweep matrix for %ld predictors...\n",k);
  for (i=0; i<sm->nextRow; ++i)
    PrintRowVector(sm->incrSweepMat[i].theVector, sm->incrSweepMat[i].columnIndex);
  printf ("---------------------------------------------------\n");
  printf("Column    Fully swept covariance matrix...\n");
  for (i=0; i<sm->nextRow; ++i)
    PrintRowVector(sm->incrSweepMat[i].theVector, sm->fullSweepMat[i].columnIndex);
  printf ("---------------------------------------------------\n\n");
}


//  -----------------------------------  File IO  -----------------------------

void WriteRegressionDataToFile (SweepMatrix sm)
{
  long *cols, nCols;
  
  nCols = NumberOfCoefficients (sm);	
  cols = ColumnsUsedInModel(sm);
  WriteColumnsToFileBDF(sm->bdf, cols, nCols, MakeFileName(sm, "regr.dat"));	
  delete(cols);
}

int readline (FILE *file, int max, char *str);
int readline (FILE *file, int max, char *str)
{
  int i;
  int c;
  
  for (i=0; i<max-1 && (c = getc(file)) != EOF && c != '\n'; ++i)
    str[i] = (char) c;
  /* Dont want the newline characters
     if (c == '\n')
     {	str[i] = c;
     ++i;
     }
  */
  str[i] = '\0';
  ++i;
  return i;
}

#define MAX_VAR_NAME 100

int
ReadVariableNamesFromFile (SweepMatrix sm)
{
  char *p, buffer[MAX_VAR_NAME];
  int len;
  long nVarNames, nLines;
  FILE *fp;

  ExitOnNull (fp = fopen (MakeFileName(sm, "names"), "r"),
	      "could not read names file", -1);
  nVarNames = nFileColumnsBDF(sm->bdf);	
  printf ("Reading %ld var names from file %s\n", nVarNames, "names");
  ExitOnNull(sm->varNames = new (char *)[nVarNames], "alloc var names", -2);
  for (nLines=0; nLines < nVarNames; ++ nLines)
  {	len = readline(fp, MAX_VAR_NAME, buffer);
	ExitOnNull (p = new char[len], "could not alloc var name", -4);
	strcpy (p, buffer);
	sm->varNames[nLines] = p;
  }
  fclose(fp);
  return (nLines);
}


//  ------------------------------  Finding Next Predictor  ------------------
// Used only in finding next variables

static void
FitProperties (double rcp, double rss, double rwwr, double sse, double df,
	       double *dRSS, double *fStat);

static void
FitProperties (double rcp, double rss, double rwwr, double sse, double df,
		    double *dRSS, double *fStat)
{   
  *dRSS = (rcp * rcp)/rss;
  // s2   = (sse - *dRSS)/(df-1); // y'y-b'x'xb for model with added var
  *fStat = (rcp * rcp) / rwwr; // s2 already part of rwwr (s2 * rwwr);
}


void
FindNextVariables (SweepMatrix sm, Sort_tree *p_tree)
{
  Sort_tree& tree(*p_tree);
  Vector RSS, RWWR, RCP, res; // diagonal of rWr and rWWr and first row rWy 
  double fStat,dRSS, sse, df;
  long nCov;
  int *skip, i;
  char ch;
  FILE *logFile;
  
  fprintf (stderr, "SM: Find next variables (cache size %d).\n", CACHE_UPDATE_SIZE);
  nCov = NumberOfVariablesBDF(sm->bdf);
  skip = sm->skipVector;
  df = DegreesOfFreedom (sm);
  // Compute squared term for F-stat calculations
  res = ResidualVector(sm);
  SquareVector(res);
  FillResidualSSBDF(sm->bdf, NumberOfPredictors(sm), sm->fullSweepMat, skip, res,
		    sm->rWWr);
  // CompactPrintVector(sm->rWWr, "rWWr");
  // Grab pointers to numerator and den of ratio
  RCP  = sm->incrSweepMat[0].theVector;  
  RSS  = sm->residualSS;
  RWWR = sm->rWWr;
  sse = VectorElement(sm->residualSS,0); // current residual ss
  // Locate the columns offering most improvement (skip first since its Y)
  // base locates the smallest retained change found so far.
  // Find first potential non-constant item for cache
  i = 0;  // 0 is Y
  while (++i < nCov)
  { if (! skip[i])
    { FitProperties(VectorElement(RCP,i),VectorElement(RSS,i),VectorElement(RWWR,i),
		    sse, df, &dRSS, &fStat);
      tree.insert(make_pair(Key(dRSS,fStat), i));
    }
  }
  ExitVoidOnNull(logFile = fopen (MakeFileName(sm, "regr.log"), "a"),
		 "could not open log file");
  fprintf (logFile,
	   " ----  ([col], dRSS, F) sorted on dRSS (tau^2=%7.4f, qBase %d) -----\n",
	   Key_sorter::s_threshold, sm->qBase);
  Sort_tree::iterator treeIt;
  for(i=0, treeIt=tree.begin(), ch='+'; i<10; ch=' ', ++treeIt, ++i)
  {
    Key sortKey = treeIt->first;
    if (sortKey.second >= Key_sorter::s_threshold)
      fprintf(logFile, "%c    %6ld     %12.3f  F = %8.3f\n",
	      ch, treeIt->second, sortKey.first, sortKey.second);
    else
      fprintf(logFile, "%c    %6ld     %12.3f  f = %8.3f\n",
	      ch, treeIt->second, sortKey.first, sortKey.second);      
  }
  fprintf (logFile, " ---------------------------------------------------------\n");
  fclose(logFile);
}


//    --------------  Matrix operations related to reversing direction  -----------

Matrix CrossProductMatrix(SweepMatrix sm, int excludeY)
{
  Matrix CP;
  Vector cp;
  long i, j, varIndex, dim;
  
  dim = NumberOfCoefficients (sm);
  if (1 == excludeY) --dim;
  CP = NewMatrix(dim, dim, "CrossProductMatrix");
  for (i=0; i<dim; ++i)
  { varIndex = sm->incrSweepMat[i+excludeY].columnIndex;
    cp = sm->CPCache[varIndex];
    if (NULL == cp)
    { fprintf(stderr,
	      "SM: error building cross-product array for %ld\n", varIndex);
      DeleteMatrix(CP);
      return NULL;
    } else
      for (j=0; j<dim; ++j)
	AssignMatrixElement(CP, i, j,
			    VectorElement(cp,
					  sm->incrSweepMat[j+excludeY].columnIndex));
  }
  return CP;
}


Vector FitVector (SweepMatrix sm)
{
  Vector coefs, fit;
  long *pIndex;
  double intercept;
	
  pIndex = ColumnsUsedInModel(sm);
  coefs = CoefficientVector (sm);
  intercept = coefs->ptr[0];
  // yukky stuff here to avoid origin
  coefs->len = (coefs->len)-1;
  coefs->ptr = (coefs->ptr)+1;
  // call to bdf function
  /* printf ("Building fit vector from following model coefs:\n");
     for (i=0; i<coefs->len; ++i)
     printf("%ld  [%ld] %10.3f\n", i, pIndex[i+1], VectorElement(coefs,i));
  */
  fit = LinearCombinationBDF(sm->bdf, coefs, pIndex+1, intercept); // +1 avoids Y col
  // restore vector's guts
  coefs->len = (coefs->len)+1;
  coefs->ptr = (coefs->ptr)-1;
  DeleteVector(coefs); delete(pIndex);
  return(fit);
}

Vector	ResidualVector (SweepMatrix sm)
{
  Vector coefs, resids;
  double negIntercept;
  long i, *pIndex;
	
  pIndex = ColumnsUsedInModel(sm);
  coefs = CoefficientVector (sm);
  // stuff 1 into leading term for the y vector weight
  negIntercept = - coefs->ptr[0];
  coefs->ptr[0] = 1.0;
  // reverse sign on rest of slopes
  for (i=1; i<coefs->len; ++i)
    coefs->ptr[i] = - coefs->ptr[i] ;
  // call bdf function
  resids = LinearCombinationBDF(sm->bdf, coefs, pIndex, negIntercept); 
  delete (pIndex);
  DeleteVector(coefs);
  return(resids);
}

Vector	BinomialWeightVector (SweepMatrix sm, double *sumWts)
{
  Vector wtVector, fitVector;
  double fit, fitVar;
  long nBig, nSmall, i;
  
  wtVector = sm->rawWeights;			// the initial weights
  fitVector = FitVector(sm);                    // current fitted predictions
  for (i=0, *sumWts=0.0, nBig=0, nSmall=0; i<fitVector->len; ++i)
  {	fit = VectorElement(fitVector,i);
	if (fit >= 0.99999)
	{	fitVar = 0.00001;	++nBig;	}
	else if (fit <= 0.00001) 
	{	fitVar = 0.00001;	++nSmall;	}
	else 
	  fitVar = 4.0 * fit * (1.0 - fit) ;    // mult by 4 to keep on range 0-1
	AssignVectorElement (fitVector, i, fitVar * VectorElement(wtVector,i) );
	*sumWts += VectorElement(fitVector, i);
  }
  if (nBig || nSmall)
    fprintf(stderr,
	    "SM: number predictions large = %ld; number small  = %ld\n", nBig, nSmall);
  return(fitVector);
}

void
CheckWeightVector(SweepMatrix sm, double *maxDiff, double *avgDiff, double *corr)
{
  Vector currentWts, newWts;
  double sumWts;
  
  currentWts = WeightVectorBDF(sm->bdf);
  newWts = BinomialWeightVector (sm, &sumWts);
  *maxDiff = VectorDistance (currentWts, newWts, 0);		// L0 norm
  *avgDiff = VectorDistance (currentWts, newWts, 1);
  *avgDiff /= (double) VectorLength (currentWts);
  *corr = VectorCorrelation(currentWts, newWts);
  if (0)
  { int i; FILE *test;
    test=fopen ("weights", "w");
    for (i=0; i<VectorLength(currentWts); ++i)
      fprintf(test, "%f %f\n", VectorElement(currentWts,i), VectorElement(newWts,i));
    fclose(test);
  }
  DeleteVector(currentWts);
  DeleteVector(newWts);
}
	

Matrix
RawCovMatOfEstimates (SweepMatrix sm, int code)
{
  Matrix XpXinv;
  double s2;
  long i,j;
	
  if (0 == code)  // from X'X inversion
  { Matrix XpX;
    XpX = CrossProductMatrix(sm, 1);
    XpXinv = InverseMatrix(XpX, "RawCovMatOfEstimates");
    DeleteMatrix(XpX);
  }
  else            // from sweep matrix
  { Vector xx;
    long k, cj;
    k = NumberOfPredictors(sm);
    XpXinv = NewMatrix(k,k,"RawCovMatOfEstimates");
    // can also get from lower triangular portion of incremental
    for(i = 0; i < k; ++i)
    { xx = sm->fullSweepMat[i+1].theVector;
      for(j = 0; j < k; ++j)
      { cj = sm->fullSweepMat[j+1].columnIndex;
	// note neg sign since held in pivot form
	AssignMatrixElement(XpXinv, i, j, -VectorElement(xx,cj));
      }
    }
  }
  s2 = SSquared(sm);
  for (i=0; i<XpXinv->nRows; ++i)
    for (j=0; j < XpXinv->nRows; ++j)
      MultiplyMatrixElement(XpXinv, i, j, s2);
  return(XpXinv); 
}


Matrix
HomoCovMatOfEstimates (SweepMatrix sm)  // (X'WX)-1 (X'WWX) (X'WX)-1
{
  Matrix XpX, XpXinv, XWWX, prod1, prod2;
  Vector wtVector;
  double s2;
  long i,j, *pCols;
	
  XpX = CrossProductMatrix(sm, 1);
  XpXinv = InverseMatrix(XpX, "WeightedCovMatOfEstimates");
  // compute the squared weight vector
  wtVector = WeightVectorBDF(sm->bdf);
  SquareVector(wtVector);
  // compute cross product with modified weights
  pCols = ColumnsUsedInModel(sm);
  printf("SM: computing weighted X'X matrix\n");
  XWWX = CPMatrixBDF(sm->bdf, pCols+1, XpX->nRows, wtVector);  	// +1 skips over Y
  delete(pCols);
  printf("SM: forming matrix products\n");
  prod1 = MatrixProduct(XpXinv, XWWX, "WeightedCovMatOfEstimates");
  prod2 = MatrixProduct(prod1, XpXinv, "WeightedCovMatOfEstimates");
  // release space
  DeleteVector(wtVector);
  DeleteMatrix(XpXinv); DeleteMatrix(XWWX); DeleteMatrix(prod1);  DeleteMatrix(XpX);
  // multiply matrix elements by s^2, with cov left in XpXinv
  printf ("SM: scaling the var/covariance matrix using s2.\n");
  s2 = SSquared(sm);
  for (i=0; i<prod2->nRows; ++i)
    for (j=0; j < prod2->nRows; ++j)
      MultiplyMatrixElement(prod2, i, j, s2);
  return(prod2);
}

Matrix
BinCovMatOfEstimates (SweepMatrix sm)
{
  BinaryDataFile copy;   // use this to do weighted matrix calculations
  Matrix XpX, XpXinv, XWX, prod1, prod2;
  Vector wtVector;
  double s2, sumWts, trace;
  long i,j, *pCols;
	
  XpX = CrossProductMatrix(sm, 1);
  XpXinv = InverseMatrix(XpX, "BinCovMatOfEstimates");
  // binomial wts for  v^2 (X'X)^-1 (X'WX)  (X'X)^-1 ; includes sampling wts
  wtVector = BinomialWeightVector(sm, &sumWts);
  // copy data file and replace weights
  printf("SM: copying BDF for weighting.\n");
  copy = CopyBinaryDataFile (sm->bdf);
  SetWeightVectorBDF(copy, wtVector);
  pCols = ColumnsUsedInModel(sm);
  printf("SM: computing weighted X'X matrix from data file\n");
  XWX = CPMatrixBDF(copy, pCols+1, XpX->nRows, NULL);  	// +1 skips over Y
  delete(pCols);
  printf("SM: forming matrix products\n");
  prod1 = MatrixProduct(XpXinv, XWX, "BinCovMatOfEstimates");
  trace = TraceOfMatrix(prod1);
  prod2 = MatrixProduct(prod1, XpXinv, "BinCovMatOfEstimates");
  // put product into XpXinv, adjust s2
  DeleteMatrix(XpXinv);
  XpXinv = prod2;
  printf("SM: divisor for s2 (sum - trace): %f - %f = %f\n",
	 sumWts, trace, sumWts-trace);
  s2 = SSquared(sm);
  s2 *= (SumWeightsBDF(sm->bdf) - XpX->nRows)/(sumWts - trace);
  // release space
  DeleteBinaryDataFile(copy);
  DeleteVector(wtVector);
  DeleteMatrix(XWX); DeleteMatrix(prod1);
  // multiply matrix elements by s^2, with cov left in XpXinv
  printf ("SM: scaling the var/covariance matrix using s2.\n");
  for (i=0; i<XpXinv->nRows; ++i)
    for (j=0; j < XpXinv->nRows; ++j)
      MultiplyMatrixElement(XpXinv, i, j, s2);
  DeleteMatrix(XpX);
  return(XpXinv);
}


Matrix
HeteroCovMatOfEstimates (SweepMatrix sm)
{
  Matrix XpX, XpXinv, XEX, prod, covMat;
  Vector e2, wts;
  long *pCols;
  
  XpX = CrossProductMatrix(sm, 1);
  XpXinv = InverseMatrix(XpX, "HeteroCovMatOfEstimates");
  
  // use residuals^2 as weights, scaling old weights
  e2 = ResidualVector(sm);
  SquareVector (e2);
  wts = WeightVectorBDF(sm->bdf);  // copy of weight vector
  SquareVector (wts);  // 31 Jul 00
  MultiplyVectors (e2, wts);  

  // compute matrix using revised weights
  pCols = ColumnsUsedInModel(sm);
  printf("SM: computing weighted X'X matrix\n");
  XEX = CPMatrixBDF(sm->bdf, pCols+1, XpX->nRows, wts);  	// +1 skips over Y
  /* printf("    100 sqrt diagonal XEX: ");
     for (i=0; i<XpX->nRows; ++i) printf (" %f ", 100.0*sqrt(MatrixElement(XEX,i,i)));
     printf("\n");
  */
  delete(pCols);
  printf("SM: forming matrix products\n");
  prod   = MatrixProduct(XpXinv, XEX, "CovMatOfEstimates");
  covMat = MatrixProduct(prod, XpXinv, "CovMatOfEstimates");
  // release space
  DeleteVector(wts); DeleteVector(e2);
  DeleteMatrix(XpX); DeleteMatrix(XpXinv); DeleteMatrix(XEX); DeleteMatrix(prod);
  return(covMat);
}

////////////////////////  EOF  //////////////////////////////////////////////
