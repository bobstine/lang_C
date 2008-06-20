// $Id: gsModel.test.c,v 1.1 2002/02/09 03:49:23 bob Exp $

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "gsModel.h"
#include "dataArray.h"
#include "operator.h"
#include "queue.h"
#include "memory.h"
#include "random.h"


// --------------------------------------------------------------------------

void Wait(void);
void PrintDuration(clock_t, clock_t);
void BuildFileName (char *prefix, char *buffer);


void PrintDuration(clock_t start, clock_t finish)
{
  printf ("    Required =  %f  seconds.  Any character continues.\n",
	  ((double) (finish - start)) / CLOCKS_PER_SEC);
  // Wait();
}

void Wait (void)
{
  char ch;
  ch = getchar();
}

void
BuildFileName (char *prefix, char *buffer)
{
  char *path = "/home/bob/C/mgs-sweep2/";
  strcpy(buffer, path);
  strcat(buffer, prefix);
  strcat(buffer, ".db");
}


// --------------------------------------------------------------------------
int
GenerateTestDataFile (char *fileName, long nRows, long nCols)
{
  float x;
  long i,j;
  FILE *fp;
  
  printf ("\n ---  Generate test data file  --- \n");
  fp = fopen (fileName, "w");
  fprintf (fp, "%ld %ld \n", nRows, nCols);
  for (i=0; i<nRows; ++i)
  { fprintf (fp, "%f ", 1.0);  // set weights to 1
    for (j=1; j<nCols; ++j)
    { x =  NormalRand();
      fprintf(fp, "%f ", x);
    }
    fprintf(fp, "\n");
  }
  printf ("Wrote %ld rows with %ld columns to %s\n", nRows, nCols, fileName);
  fclose(fp);
  return 1;
}

int main(void)
{
  GSModel gs0, gs00;
  Operator op;
  OperatorQueue opQueue;
  int useWeights = 1;
  long nRows=20, nCols=10;
  long seed = 10203;
  long q;
  char fileName[80];
  char *prefix="ctest";
  char *opFileName = "driver.input";  // remove the model name
  FILE *fp;
  // clock_t startTime, stopTime;

  // initialization of this and other sections
  InitializeRandomGenerator (1, seed);

  // build input test data file
  BuildFileName ("ctest", fileName);
  GenerateTestDataFile(fileName, nRows, nCols);

  // create and print the initial model
  gs0 = NewGSModel(fileName, prefix, useWeights);
  PrintGSModel(gs0, "Initial model");
  WriteGSModelToFile (gs0);
  gs00 = ReadGSModelFromFile ("ctest.gsModel.0");

  // Open operator queue from file and find one that meets goal
  fp = fopen(opFileName, "r");
  opQueue = NewOperatorQueueFromOpenFile (fp);
  op = EvaluatePredictorsForGSModel (gs00, opQueue);
  CloseOperatorQueue(opQueue);
  if (op)
  { printf ("Following predictive operator meets goal:\n");
    PrintOperatorToFile (op, stdout);
    q = CommitPredictorToGSModel(gs0, op);
    PrintGSModel(gs0, "Model after committing one");
    WriteGSModelToFile (gs0);
    GSModel gs1 = ReadGSModelFromFile ("test.gsModel.2");
    PrintGSModel(gs1, "Model with one predictor and const, read from file");
  } else
  { printf ("No operator meets specified F goal\n");
  }
  PrintMemoryUse();
  return 0;
}
