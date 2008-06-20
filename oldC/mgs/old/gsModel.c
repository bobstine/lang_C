// $Id: gsModel.c,v 1.1 2002/02/09 03:49:23 bob Exp $

/*
  21 Jun 01 ... Created for new version of sweeper.
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

// #include <string.h>

#include "gsModel.h"
#include "memory.h"
#include "operator.h"
#include "queue.h"
#include "raggedArray.h"

//////////////////

#define MAX_GS_SIZE 300

//////////////////

#define ExitOnNull(ptr,str,result) if(NULL==(ptr)){fprintf(stderr,"\nDA: %s\n",(str));return (result);}

#define ExitOn(c, str, val) if (c){fprintf(stderr, "\nDA: %s\n", (str)); return (val);}

// --- Local functions ---

static long   min(long a, long b);
static void   ShortPrintDouble (double *x, long n, char *msg);

static void   FitConstant (GSModel gs);
static void   InitQVQ (GSModel gs);

static double *ColumnOfR (GSModel gs, long col);
double DotProductY (GSModel gs, long k);

// --- New, delete, print ---

GSModel
NewGSModel (char *inputFileName, char *path, int nPad)
{
  GSModel gs;
  long n;
  
  gs = (GSModel) Allocate(sizeof(aGSModel), "NewGSModel", "GSnw");
  ExitOnNull(gs, "could not allocate model structure", NULL);
  strcpy(gs->path, path);
  gs->q = 0;
  gs->nPad = nPad;
  gs->database = ReadDataArray(inputFileName, 0); // not padded, altered
  n=NumberOfObservationsGSModel(gs);
  gs->sumWts = 0.0;
  printf("GS: Allocating new model array with dim [%ld, %d(+%d)]\n", n, 3, nPad);
  gs->X   = NewDataArray(n, 1, nPad, "NewGSModel"); // 1 for const
  gs->wYQ = NewDataArray(n, 3, nPad, "NewGSModel"); // 3 for w, y, const
  gs->R   = NewRaggedArray (MAX_GS_SIZE, "NewGSModel");
  gs->QtY = NewVector (MAX_GS_SIZE, "NewGSModel");
  ExitOnNull (gs->QtY, "Could not allocate QtY", NULL);
  gs->temp=(double *)Allocate(MAX_GS_SIZE*sizeof(double), "NewGSModel", "GSnw");
  gs->temp2=(double *)Allocate(MAX_GS_SIZE*sizeof(double), "NewGSModel", "GSnw");
  ExitOnNull (gs->temp2, "Could not allocate temp space", NULL);
  // do stuff since we now have the room
  FitConstant(gs);  // computes sum of weights
  InitQVQ(gs);
  ExitOnNull (gs->QVQ, "Could not allocate initial QVQ matrix", NULL);
  return(gs);
}

static void
FitConstant (GSModel gs)
{
  float **dbRows  = BasePtrOfDataArray(gs->database);
  float **XRows   = BasePtrOfDataArray(gs->X);
  float **dstRows = BasePtrOfDataArray(gs->wYQ);
  double *pR, wt, sw, swY;
  long n = NumberOfObservationsGSModel(gs);

  // Initialize the size of model
  gs->q = 1;
  // Initialize the X data array
  for (long i=0; i<n; ++i)
    XRows[i][0] = 1.0;
  // Find leading element of R
  pR = (double *) Allocate(sizeof(double), "FitConstant", "doub");
  sw = swY = 0.0;
  for(long i=0; i<n; ++i)
  { wt = dbRows[i][0];
    sw += wt;
    swY += wt * dbRows[i][1];
    dstRows[i][0] = wt;
  }
  // r_00 = sqrt(Sum w_i), Q'WY is Sum w_iY_i/sqrt()
  gs->sumWts = sw;
  double yBar = swY/sw;
  *pR = sqrt(sw);
  AssignVectorElement(gs->QtY,0,swY/(*pR));
  AppendToRaggedArray(gs->R, pR, 1);
  // Init the elements of Y and const
  double c   = 1.0 / *pR;
  double tss = 0.0;
  for(long i=0; i<n; ++i)
  { dstRows[i][1] = dbRows[i][1] - yBar;
    tss += dstRows[i][1] * dstRows[i][1];
    dstRows[i][2] = c;
  }
  // Initial RSS is the total ss
  gs->rss = gs->tss = tss;
}

static void
InitQVQ (GSModel gs)
{
  int dim = 1 + gs->nPad;
  // Pad with zeros for room to grow
  gs->QVQ = NewMatrix(dim, dim, "InitGSModel");
  for (int i=0; i<dim; ++i)
    for (int j=0; j<dim; ++j)
      AssignMatrixElement(gs->QVQ, i,j, 0.0);
  // q_0=1/sqrt(n), so Q'VQ is just sum of squared mean deviations / n
  long n = NumberOfObservationsGSModel(gs);
  AssignMatrixElement (gs->QVQ,0,0,gs->tss/(double)n);
}

void
DeleteGSModel (GSModel gs)
{
  printf ("GS: DeleteGSModel not yet implemented.\n");
}

static long
min (long a, long b)
{
  return (a < b) ? a : b;
}

#define SHORT_COUNT 6

static void
ShortPrintDouble (double *x, long len, char *msg)
{
  printf ("%s [%ld]\n ", msg, len);
  if (len > SHORT_COUNT)
  { for (long i=0; i<SHORT_COUNT; ++i)
      printf("%6.2f ", x[i]);
    printf("... %6.2f\n", x[len-1]);
  }
  else
  { for (long i=0; i<len; ++i)
      printf("%6.2f ", x[i]);
    printf("\n");
  }
}

void
PrintGSModel (GSModel gs, char *msg)
{
  long q = NumberOfPredictorsGSModel(gs);
  long n = NumberOfObservationsGSModel(gs);
  
  printf ("\n---------------------\nGram Schmidt model\n---------------------\n");
  printf ("%s (n=%ld, sum wts = %8.2f, %ld predictors)\n", msg, n, gs->sumWts, q);
  printf ("File path      %s\n", gs->path);
  printf ("Model order q=%d (incld const) with %d cols padding.\n", gs->q, gs->nPad);
  printf ("Total SS       %f\n", gs->tss);
  printf ("Current RSS    %f\n", gs->rss);
  ShortPrintDouble (gs->QtY->ptr, q+1, "QtY");
  PrintRaggedArray (gs->R, "R");
  PrintDataArray (gs->database, "Data base", 2, min(5,gs->database->nCols));
  PrintDataArray (gs->X, "X", 2, min(5,gs->database->nCols));
  PrintDataArray (gs->wYQ, "Model Array", 2, min(5,gs->wYQ->nCols));
  PrintMatrix(gs->QVQ, "QVQ");  printf("\n");
  ShortPrintDouble (gs->temp,  MAX_GS_SIZE, "Temp space");
  ShortPrintDouble (gs->temp2, MAX_GS_SIZE, "Temp2 space");
  printf ("---------------------\n");
}

// --- File operations ---

int
WriteGSModelToFile(GSModel gs)
{
  char fileName[80];
  FILE *fp;
  
  // build file name and open it for writing
  sprintf(fileName,"%sgsModel.%d",gs->path, gs->q-1);
  printf("GS: Writing model to file %s\n", fileName);
  fp = fopen(fileName, "w");
  // write the simple stuff on one line
  fprintf(fp, "path    %s\n", gs->path);
  fprintf(fp, "q       %d\n", gs->q);
  fprintf(fp, "nPad    %d\n", gs->nPad);
  fprintf(fp, "tss     %f\n", gs->tss);
  fprintf(fp, "rss     %f\n", gs->rss);
  fprintf(fp, "sumWts  %f\n", gs->sumWts);
  // write the arrays
  WriteRaggedArrayToFile(gs->R, fp);
  SaveVectorToFile(gs->QtY, fp);
  SaveMatrixToFile(gs->QVQ, fp);
  fclose(fp);
  // save the two data arrays to distinct files
  sprintf(fileName,"%sgsModel.%d.X",gs->path, gs->q-1);
  printf("GS: Writing predictor array to file %s\n", fileName);
  WriteDataArray (gs->X, fileName);
  sprintf(fileName,"%sgsModel.%d.model",gs->path, gs->q-1);
  printf("GS: Writing model array wYQ to file %s\n", fileName);
  WriteDataArray (gs->wYQ, fileName);
  return 1;
}

GSModel
ReadGSModelFromFile(char *filename)
{
  GSModel gs;
  char str[80];
  int count;
  FILE *fp;

  fp = fopen (filename, "r");
  ExitOnNull (fp, "could not open file", NULL);
  gs = (GSModel) Allocate (sizeof(aGSModel), "ReadGSModel", "read");
  ExitOnNull (gs, "could not allocate model", NULL);
  count = fscanf(fp, "path %s", gs->path);
  if (count <=0)
  { printf ("GS: Could not read model from file %s\n", filename);
    return NULL;
  }
  printf("GS: Reading model %s from file %s\n", gs->path, filename);
  printf("read the little stuff\n");
  fscanf(fp, "\nq     %d\n", &gs->q);
  fscanf(fp, "\nnPad  %d\n", &gs->nPad);   ++gs->nPad;  // increment pad so not zero
  fscanf(fp, "\ntss    %lf\n", &gs->tss);    
  fscanf(fp, "\nrss    %lf\n", &gs->rss);
  fscanf(fp, "\nsumWts %lf\n", &gs->sumWts);
  printf("read ragged array R\n");
  gs->R = ReadRaggedArrayFromFile(fp);
  PrintRaggedArray(gs->R,"R");
  printf("read vector QtY\n");
  gs->QtY = RestoreVectorFromFile(fp, "ReadGSModel");
  CompactPrintVector(gs->QtY, "QtY");
  gs->QVQ = RestoreMatrixFromFile(fp, gs->nPad, "ReadGSModel");
  PrintMatrix (gs->QVQ, "QVQ");
  fclose(fp);
  // read padded data arrays from separate files
  sprintf(str,"%sdb",gs->path);
  printf("GS: Reading database from %s\n", str);
  gs->database = ReadDataArray (str, gs->nPad);
  ExitOnNull (gs->database, "Could not allocate database on read", NULL);
  sprintf(str,"%sgsModel.%d.X",gs->path, gs->q-1);
  printf("GS: Reading X from %s\n", str);
  gs->X = ReadDataArray (str, gs->nPad);
  ExitOnNull (gs->X, "Could not allocate X on read", NULL);
  sprintf(str,"%sgsModel.%d.model",gs->path, gs->q-1);
  printf("GS: Reading model from  %s\n", str);
  gs->wYQ = ReadDataArray (str, gs->nPad);
  ExitOnNull (gs->wYQ, "Could not allocate wYQ on read", NULL);
  gs->temp=(double *)Allocate(MAX_GS_SIZE*sizeof(double), "NewGSModel", "GS_x");
  gs->temp2=(double *)Allocate(MAX_GS_SIZE*sizeof(double), "NewGSModel", "GS_x");
  ExitOnNull (gs->temp2, "Could not allocate temp space on read", NULL);
  return gs;
} 

 
// --- Basic properties ---

long
NumberOfObservationsGSModel (GSModel gs)
{
  return ( NRowsOfDataArray(gs->database) );
}
    

long
NumberOfPossiblePredictorsGSModel (GSModel gs)  // p = # cols - 1 each for wts, y
{
  return ( NColsOfDataArray(gs->database) - 2 );
}

long
NumberOfPredictorsGSModel (GSModel gs)    //  gs->q includes the constant, so sub 1
{
  return gs->q-1;
}


double
ResidualDFGSModel (GSModel gs)
{
  double df = (double)NumberOfObservationsGSModel(gs);

  df -=  gs->q;
  return df;
}


double
ResidualVarianceGSModel (GSModel gs)
{
  return gs->rss/ResidualDFGSModel(gs);
}



// --- Modeling operations ---

// Copies into X array, finds mean, then moves centered column into wYQ

void
CopyLastXIntoWYQ (GSModel gs)
{ 
  float **XRows = BasePtrOfDataArray(gs->X);
  float **wYQRows = BasePtrOfDataArray(gs->wYQ);
  long dstCol = gs->q + 2; // 2 = w + y, and gs->q includes const
  long n = NumberOfObservationsGSModel(gs);;

  // Adjust padding in the destination and check indexing
  long newCol = UsePaddingOfDataArray(gs->wYQ);
  assert(newCol == dstCol);  // better match up
  for (long i=0; i<n; ++i)
    wYQRows[i][dstCol] =  XRows[i][gs->q];
}
    
static double *
ColumnOfR (GSModel gs, long column)
{
  long size = SizeOfRaggedArray(gs->R);
  double *xtx;
  
  // See if we have the desired column
  if (size <= column)
  { printf("GS: expanding R matrix from %ld to %ld columns...\n",
	   size, 1+column);
    for (long j=size; j<=column; ++j)  // need j+1 elements
    {  xtx = (double *) Allocate((j+1)*sizeof(double), "CrossProducts", "vdbl");
       if (NULL==xtx)
       { printf ("GS: Unable to allocate XtX");
	 return NULL;
       }
       AppendToRaggedArray (gs->R, xtx, j+1);
    }
  }
  else
  { printf("GS: Access to ragged array col %ld\n", column);
    xtx = ElementOfRaggedArray(gs->R,column);
  }
  return xtx;
}

double
DotProductQ(GSModel gs, long j, long k) // dot Q[j] from Q[k]
{
  long n = NumberOfObservationsGSModel(gs);
  float **row = BasePtrOfDataArray(gs->wYQ);

  ExitOn( (j>k), "Attempt to sweep too far", 0.0);
  // offset past weights and Y
  k += 2;
  j += 2;
  double dp = 0.0;
  for (long i=0; i<n; ++i)
   dp += row[i][0] * row[i][k] * row[i][j];  // accumulate next dp
  return dp;
}

void
SweepFromQ (GSModel gs, long j, long k) // sweep Q[j] from Q[k]
{
  long n = NumberOfObservationsGSModel(gs);
  float **row = gs->wYQ->pRows;
  double beta = IndexRaggedArray(gs->R,j,k);

  // shift indices to skip w and Y, use weights from [0]
  j +=2;
  k +=2;
  // Find the appropriate coef from R
  for (long i=0; i<n; ++i)
    row[i][k] -= beta * row[i][j];
}

void
NormalizeQ (GSModel gs, long j, double divisor) //  Q[j]/divisor
{
  long n = NumberOfObservationsGSModel(gs);
  float **row = gs->wYQ->pRows;

  assert(divisor>0);
  // shift indices to skip w and Y, use weights from [0]
  j += 2;
  // Find the appropriate coef from R
  for (long i=0; i<n; ++i)
    row[i][j] /= divisor;
}


double
DotProductY (GSModel gs, long k)
{
  float **row = gs->wYQ->pRows;
  long n = NumberOfObservationsGSModel(gs);

  // Shift index of predictor by 2
  k += 2;
  // Accumulate dot product
  double dp = 0.0;
  for (long i=0; i<n; ++i)
    dp += row[i][0] * row[i][1] * row[i][k]; // weight[0]  y[1]
  return dp;
}


double
SweepLastFromY (GSModel gs)
{
  double beta = VectorElement(gs->QtY,gs->q);
  long n = NumberOfObservationsGSModel(gs);
  float **row = gs->wYQ->pRows;
  
  // shift indices to skip w and Y
  long col = 2 + gs->q; 
  // Accumulate residual ss
  double rss = 0.0;
  for (long i=0; i<n; ++i)
  { row[i][1] -= beta * row[i][col];
    rss += row[i][0] * row[i][1] * row[i][1];
  }
  return rss;
}

static void
UpdateQVQ (GSModel gs)
{
  // zero the upper half of the matrix
  for (int row=0; row<gs->q; ++row)
    for (int col=row; col<gs->q; ++col)
      AssignMatrixElement(gs->QVQ,row,col,0.0);
  // accumulate Q'VQ as sum of outer products
  for (long i=0; i<gs->wYQ->nRows; ++i)
  { float *pRow = gs->wYQ->pRows[i];
    double we2 = pRow[0] * pRow[1] * pRow[1]; // weighted squared residual
    for (int row=0; row<gs->q; ++row)
      for (int col=row; col<gs->q; ++col)
	// offset the index for positions of predictors
	IncrementMatrixElement(gs->QVQ,row,col, we2 * pRow[row+2] * pRow[col+2]);
  }
  // fill the lower half of the matrix
  for (int row=0; row<gs->q; ++row)
    for (int col=0; col<row; ++col)
      AssignMatrixElement(gs->QVQ,row,col,MatrixElement(gs->QVQ,col,row));
}

double
ChangeInRSSGSModel (GSModel gs, Operator f)
{
  double *XtZ;         // points to last col of X'X array
  double *YtZ;         // points into last element of Y'X
  double bxxb;
  long qp1 = 1+NumberOfPredictorsGSModel(gs);  // add one for constant term

  // Check to see if room for the next predictor
  if (SizeOfRaggedArray(gs->R) <= qp1)
    AppendToRaggedArray (gs->R,
        (double *) Allocate( (qp1+1)*sizeof(double), "ChangeRSS", "vdbl"), qp1+1);
  XtZ= ElementOfRaggedArray(gs->R, qp1);  //  has explicit constant 
  YtZ = & (gs->QtY->ptr[qp1]);
  // Zero the destination elements
  for (long j=0; j<=qp1; ++j)    XtZ[j] = 0.0;
  *YtZ = 0.0;
  // Watch for weighted calculation
  for (long i=0; i<gs->database->nRows; ++i)
  { float *pDataRow  = gs->database->pRows[i];
    float *pModelRow = gs->wYQ->pRows[i];
    double wt = *pDataRow;  
    float z = EvalOperator(f, pDataRow+1);      // skip over weight
    *YtZ += wt * pModelRow[1] * z;              // Y in col 1
    XtZ[0] += wt * z;                           // accumulate sum
    for (long j=1; j<qp1; ++j)            
      XtZ[j] += wt * pModelRow[j+1] * z;  // skip over wt, y
    XtZ[qp1] += wt * z * z;
    /*
      if (0==i)
	printf ("%ld: w=%6.2f y=%6.2f z=%6.2f yz=%6.2f sz=%6.2f ssz=%6.2f\n",
	           i, wt, pModelRow[1], z, *YtZ, XtZ[0], XtZ[1]);
      */
    }
  bxxb = XtZ[qp1];   // z'z
  for(long j=0; j<qp1; ++j)
    bxxb -= (XtZ[j]*XtZ[j])/IndexRaggedArray(gs->R,j,j);
  if (bxxb <= 0.0)
  { printf("GS: divisor = %f; underflow evaluating predictor for model\n",bxxb);
    return 0.0;
  }
  else
    return ((*YtZ * *YtZ)/bxxb);
}


double
PartialFGSModel (GSModel gs, Operator f)
{
  long q = NumberOfPredictorsGSModel(gs);  
  long n = NumberOfObservationsGSModel(gs);
  double rootN = sqrt((double) n);

  // Set up scratch space for accumulators
  double etz = 0.0, temp;
  double zVz = 0.0;
  double *Qtz = gs->temp;
  double *QtVz = gs->temp2;
  for (long j=0; j<=q; ++j)
  { Qtz[j] = 0.0;
    QtVz[j] = 0.0;
  }
  
  for (long i=0; i<n; ++i)
  { float *pDataRow  = gs->database->pRows[i];
    float *pModelRow = gs->wYQ->pRows[i];
    double wt = *pDataRow;  
    double z = EvalOperator(f, pDataRow+1);      // +1 makes skip over weight
    double e = pModelRow[1];                     // current residual
    double v = e * e;
    etz += wt * e * z;
    Qtz[0] += temp = wt * z / rootN;             // accumulate sum
    QtVz[0] += v * temp;
    for (long j=1; j<=q; ++j)            
    { temp = wt * pModelRow[j+1] * z;            // skip over wt, y
      Qtz[j] += temp;
      QtVz[j] += v * temp;
    }
    zVz += wt * v * z * z;
  }
  double denom = zVz;
  for (long j=0; j<=q; ++j)
    denom -= 2.0 * Qtz[j] * QtVz[j];
  denom += QuadraticForm (gs->QVQ, Qtz);
  printf ("GS: num = (%9.4f)^2  den = %9.4f\n", etz, denom);
  if (denom <= 0.0)
  { printf("GS: denominator = %f; underflow evaluating predictor for model\n",denom);
    return 0.0;
  }
  else return (etz * etz / denom);
}

Operator
FindBestPredictorForGSModel (GSModel gs, vector<Operator> &ops, FILE *reply)
{
  Operator op = ops[0];
  
  op->score = 20.;
  return op;
}


Operator
EvaluatePredictorsForGSModel (GSModel gs, OperatorQueue opQueue, FILE *reply)
{  
  Operator op;
  double fStat=0.0, fGoal=0.0;
  
  // Build sequence of operators from the file
  while((op = GetNextOperator (opQueue)))
  { // dRSS = ChangeInRSSGSModel(gs, op);
    // fStat = dRSS / ResidualVarianceGSModel(gs);
    // printf ("    dRSS=%9.4f  F=%9.4f   goal %8.3f\n", dRSS, fStat, fGoal);
    fGoal = op->threshold;
    fStat = PartialFGSModel(gs, op);
    fprintf (stderr, "GS: EvalPred cons F=%9.4f   goal %8.3f\n", fStat, fGoal);
    if (fStat >= fGoal)
    { op->score = fStat;
      fprintf (reply, "(accept ");  
      PrintOperatorToFile (op, reply);
      fprintf (reply, ")\n");
      break;
    }
    else
    { fprintf (reply, "reject\n");  
      DeleteOperator(op);
    }
    fflush(reply);
  }
  return op;  // nil if not found
}


static double
CheckConservativeF(GSModel gs)
{
  float **row = gs->wYQ->pRows;
  long n = NumberOfObservationsGSModel(gs);
  
  // shift indices to skip w and Y
  long col = 2 + gs->q; 
  // Accumulate num and den dot products
  double num = 0.0;
  double den = 0.0;
  for (long i=0; i<n; ++i)
  { double wyz =  row[i][0] * row[i][1] * row[i][col];  // wt y~ z~
    num += wyz;
    den += wyz * row[i][1] * row[i][col];  // wt z~ (y~^2) z~
  }
  assert (den > 0);
  return (num * num) / den;
}


/*
  Committing a predictor
  (1) Concatenates predictor (op results) as last col of predictor data array
      so that the column that is printed is the *new* column index.
  (2) Moves predictor into model array after centering (ie,sweep constant).
  (3) Sweeps other predictors from this one using the MGS method.
  (4) Writes the regression data out to disk.
*/

long
CommitPredictorToGSModel (GSModel gs, Operator f)
{
  double *Rq=NULL;
  
  if (0==gs->nPad)
    printf("GS: lack padding to expand model on commit; exiting\n");
  else
    --gs->nPad;
  long newCol = UsePaddingOfDataArray (gs->X);
  printf("+++++ q = %d; committing predictor into [%ld] of X\n", gs->q, newCol);
  PrintOperatorToFile(f, stdout);
  // Build the predictor in X, then copy into QR decomp
  ApplyOperatorToDataArray (f, gs->database, 1, gs->X, gs->q);  // 1 says weighted
  printf("+++++ Copying predictor into model\n");
  CopyLastXIntoWYQ(gs);
  // Set up space to store cross products
  Rq = ColumnOfR(gs, gs->q);
  // Sweep out the prior columns
  for (long j=0; j<gs->q; ++j)
  {  Rq[j] = DotProductQ(gs, j, gs->q);
     printf("       dp  R[%ld] = %8.3f\n", j, Rq[j]);
     SweepFromQ (gs, j, gs->q);
  }
  // Check partial F calculation before update residuals
  printf("+++++ Verify conservative F = %9.3f\n", CheckConservativeF(gs));
  // Sum squared residuals in Q[k] and put sqrt into R[k,k], then normalize
  Rq[gs->q] = sqrt(DotProductQ(gs, gs->q, gs->q));
  NormalizeQ(gs, gs->q, Rq[gs->q]);
  ShortPrintDouble(Rq, gs->q+1, "R[q]");
  printf("+++++ Setting up new residuals\n");
  ExitOn((gs->q > VectorLength(gs->QtY)), "Sweep too far; exiting.", 0);
  double dp = gs->QtY->ptr[gs->q] = DotProductY(gs,gs->q);
  printf("GS: Using b=%8.3f/%8.3f, RSS that was %8.3f is now ",
	 dp, IndexRaggedArray(gs->R,gs->q,gs->q), gs->rss);
  gs->rss = SweepLastFromY(gs);
  printf(" %8.3f \n", gs->rss);
  ++ gs->q;
  printf("+++++ Updating the QVQ array\n");
  UpdateQVQ (gs);
  return (gs->q);
}
