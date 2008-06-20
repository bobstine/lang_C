// $Id: operator.h,v 1.1 2005/06/13 20:47:51 bob Exp $

/*
  Dispacter for operators on data arrays (and other places)

   7 Jul ... Created (on the road in Ohio!)
   
*/

#ifndef _OPERATOR_
#define _OPERATOR_

#include <vector>

#define OP_NAME_LEN 5

typedef char OperatorName[OP_NAME_LEN];
typedef float (*OperatorFunction)(float *x, long n, long *pCols, double *pArgs);

typedef struct anOp
{
  OperatorName name;
  OperatorFunction f;
  long nArg;
  long *pCols;
  double *pArgs;
  double threshold;
  double score;
} anOperator;

typedef anOperator *Operator;


////////////////////////////////////

typedef struct aOQ
{
  long position;
  char fileName[60];
  FILE *fp;
} anOperatorQueue;

typedef anOperatorQueue *OperatorQueue;

///////////////////////////////////


void
DeleteOperator (Operator f);


/////////////  Operator Queue  //////////////////

OperatorQueue
NewOperatorQueueFromFile (char *filename);

OperatorQueue
NewOperatorQueueFromOpenFile (FILE *fp);

void
CloseOperatorQueue (OperatorQueue queue);

Operator
GetNextOperator (OperatorQueue);

Operator
ParseOperatorFromFile (OperatorQueue);

Operator
GenerateNextIndexOperator (OperatorQueue);

////////////  Vector of operators  /////////////////

vector <Operator>
LinearOperators (long firstIndex, long lastIndex);
/*
  Makes a sequence of operators from first to last index.
*/

vector <Operator>
QuadraticOperators (long firstIndex, long lastIndex);
/*
  Makes a sequence of quadratic operators from first to last index.
*/

float
EvalOperator (Operator f, float *x);

void
PrintOperatorToFile (Operator f, FILE *fp);

#endif
