// $Id: operator.c,v 1.14 2001/11/28 21:28:33 bob Exp $

/*

   7 Jul 01 ... First pass at code.
   
*/

#include <stdlib.h>
#include <stdio.h>

#include "operator.h"
#include "memory.h"

#define N_OPERATORS 3

OperatorName gOperatorNameTable[N_OPERATORS] = {"INDX", "LINC", "INTR"};

// -------------------- Operators ---------------------------


static Operator AllocateOperator (OperatorName);

static float    Index     (float *x, long n, long *col, double *args);
static void     FillIndexOperatorFromFile (Operator op, FILE *fp);

static float    LinComb   (float *x, long n, long *col, double *args);
static void     FillLinCombOperatorFromFile (Operator op, FILE *fp);

static float    Interact  (float *x, long n, long *col, double *args);
static void     FillInteractOperatorFromFile (Operator op, FILE *fp);


///////------------------------------------------------------

static Operator
AllocateOperator (OperatorName name)
{
  Operator op;

  op = (Operator) Allocate (sizeof(anOperator), "AllocateOperator", name);
  if (NULL == op)
    printf("OP: could not allocate operator\n");
  else
    strcpy(op->name, name);
  return op;
}

void
DeleteOperator (Operator f)
{
  if (NULL == f)
    printf("OP: Attempt to delete a null operator.\n");
  else
    Release(f);
}


float
Index (float *x, long n, long *col, double *args)
{
  float xVal = x[n];
  // nprintf ("Got column %ld value %f\n", *col, xVal);
  return xVal;
}

Operator
MakeIndexOperator (long index)
{
  Operator op = AllocateOperator("INDX");

  op->f = Index;
  op->nArg = index;  // nasty thing here
  op->pCols = NULL;
  op->pArgs = NULL;
  return op;
}

void
FillIndexOperatorFromFile (Operator op, FILE *fp)
{
  op->f    = Index;
  fscanf(fp, "%ld", &op->nArg);
  op->pCols = NULL;
  op->pArgs = NULL;
}



float
LinComb (float *x, long n, long *col, double *args)
{
  double accum = 0.0;
  float result;

  for (long j=0; j<n; ++j)
    accum += args[j] * x[ col[j] ];
  result = (float) accum;
  return result;
}

void
FillLinCombOperatorFromFile (Operator op, FILE *fp)
{
  op->f    = LinComb;
  fscanf(fp, "%ld", &op->nArg);
  op->pCols = (long *) Allocate (op->nArg * sizeof(long), "ParseOp", "pcol");
  for (long j=0; j<op->nArg; ++j)
    fscanf(fp, "%ld", &op->pCols[j]);
  op->pArgs = (double *) Allocate (op->nArg * sizeof(float),"ParseOp", "parg");
  for (long j=0; j<op->nArg; ++j)
    fscanf(fp, "%lf", &op->pArgs[j]);
}


float
Interact (float *x, long n, long *col, double *args)
{
  float result=x[col[0]];
  
  // printf ("Calling Interact with %ld col 0=%ld and 1=%ld\n", n, col[0], col[1]);
  for (long j=1; j<n; ++j)
    result *= x[ col[j] ];
  return result;
}


void
FillInteractOperatorFromFile (Operator op, FILE *fp)
{
  op->f    = Interact;
  fscanf(fp, "%ld", &op->nArg);
  if (1 == op->nArg)
    fprintf(stderr, "Warning: got one index for an interaction term\n");
  op->pCols = (long *) Allocate (op->nArg * sizeof(long), "ParseOp", "pcol");
  for (long j=0; j<op->nArg; ++j)
    fscanf(fp, "%ld", &op->pCols[j]);
  op->pArgs = NULL;
}

///////////////////////////////////////////////////////////////////

Operator
ParseOperatorFromFile (FILE *fp)
{
  OperatorName name;
  double fGoal;
  Operator op = NULL;

  int count = fscanf(fp, "%s %lf", name, &fGoal);
  if (2 == count)
  { printf("OP: Parsing operator  ");
    for(int j=0; j < N_OPERATORS; ++j)
      if (0 == strcmp(gOperatorNameTable[j], name))
      {
	printf("%s\n", name);
	op = AllocateOperator(name);
	op->threshold = fGoal;
	op->score = 0.0;
	switch (j)
	{
	case 0:  
	  FillIndexOperatorFromFile (op,fp);        break;
	case 1:
	  FillLinCombOperatorFromFile (op,fp);      break;
	case 2:
	  FillInteractOperatorFromFile (op,fp);     break;
	default:
	  printf("OP: Invalid operator index %d, returning null\n", j);
	}
      }
  } else
    printf("\nOP: Operator not found, returning null\n");
  return op;
}

float
EvalOperator (Operator op, float *x)
{
  return op->f(x,op->nArg,op->pCols, op->pArgs);
}

void
PrintOperatorToFile (Operator f, FILE *fp)
{ char *indx = "INDX";

  if (0 == strcmp(indx, f->name))
    fprintf(fp, "((INDX %ld) (score %7.2f) (goal %7.2f))\n",
	    f->nArg, f->score, f->threshold);
  else
  { fprintf(fp, "((%s ", f->name);
    for (long i=0; i<f->nArg; ++i)
      fprintf(fp, "%ld ", f->pCols[i]);
    fprintf(fp, ") (score %7.2f) (goal %7.2f))\n",f->score, f->threshold);
  }
  fflush(fp);
}

OperatorQueue
NewOperatorQueueFromFile (char *filename)
{
  FILE *fp = fopen(filename,"r");
  if (NULL == fp)
  { printf ("OP: Could not open file for operator queue\n");
    return NULL;
  }
  else
    return NewOperatorQueueFromOpenFile(fp);
}

OperatorQueue
NewOperatorQueueFromOpenFile (FILE *fp)
{
  OperatorQueue opQueue;
  
  opQueue = (OperatorQueue) Allocate( sizeof(anOperatorQueue), "Open", "OPqu");
  if (NULL == opQueue)
  { printf ("OP: Could not allocate memory for operator queue\n");
    return NULL;
  }
  else
  { strcpy(opQueue->fileName, "Open File");
    opQueue->position = 0;
    opQueue->fp = fp;
    return opQueue;
  }
}

void
CloseOperatorQueue (OperatorQueue queue)
{
  fclose(queue->fp);
}

Operator
GetNextOperator (OperatorQueue queue)
{
  ++queue->position;
  return ParseOperatorFromFile(queue->fp);
}


/////////////////////////  Sequences  /////////////////////////////////

vector <Operator>
LinearOperators (long firstIndex, long lastIndex)
{
  vector<Operator> vec (lastIndex-firstIndex+1);

  for (long j=0; j <= lastIndex-firstIndex+1; ++j)
    vec[j] = MakeIndexOperator (j+firstIndex);

  return vec;
}

vector <Operator>
QuadraticOperators (long firstIndex, long lastIndex)
{
  vector<Operator> vec (lastIndex-firstIndex+1);

  fprintf (stderr, "Should not be using quad operators yet!\n");
  for (long j=firstIndex; j <= lastIndex; ++j)
    vec[j] = MakeIndexOperator (j);

  return vec;
}
