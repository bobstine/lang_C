// $Id: driver.c,v 1.1.1.1 2001/12/04 20:32:35 bob Exp $

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gsModel.h"
#include "operator.h"
#include "queue.h"


// --------------------------------------------------------------------------

static char buffer[120];

int
ReadOpsFromFileQ (char *str)
{
  if (0 == strcmp(str, "file"))
    return 1;
  else
    return 0;
}

char *
AddPathToName (char *prefix)
{
  char *path = "/home/bob/C/mgs-sweep2/";
  strcpy(buffer, path);
  strcat(buffer, prefix);
  return buffer;
}

// --------------------------------------------------------------------------

int main(void)
{
  GSModel gsm;
  const int useWeights = 1;
  char modelFileName[80];
  char key[20];
  FILE *inputFile, *outputFile;

  // Open input and output files (or pipes if from Lisp)
  inputFile  = fopen( AddPathToName("driver.input" ), "r");
  outputFile = fopen( AddPathToName("driver.output"), "w");

  // Read type of operation from first word of input file (just first letter)
  fscanf(inputFile, "%s", key);
  switch (key[0]) 
  {
  case 'i':  // Create an initial model
    { char dataFileName[20];
      char modelPrefix[20];
      fscanf (inputFile, "%s", dataFileName);
      fscanf (inputFile, "%s", modelPrefix);
      gsm = NewGSModel( AddPathToName(dataFileName), modelPrefix, useWeights);
      WriteGSModelToFile(gsm);  // writes model 0
      fprintf(stderr, "----- Initialized model %s ----- \n", modelPrefix);
      fprintf(outputFile, "(init model %s completed)\n", modelPrefix);
    } break;
  case 'q':  // Build a queue from sequence of operators
    { fscanf (inputFile, "%s", modelFileName);
      gsm = ReadGSModelFromFile (modelFileName);
      fprintf(outputFile, "(adding to model %s)\n", modelFileName);
      fflush(outputFile);
      // Read operators from the input file until success or "end"
      OperatorQueue opQueue = NewOperatorQueueFromOpenFile (inputFile);
      Operator op = EvaluatePredictorsForGSModel (gsm, opQueue, outputFile);
      // Check to see if found an op
      if (op)
      { CommitPredictorToGSModel(gsm, op);
	PrintGSModel(gsm, "Model after committing");
	WriteGSModelToFile (gsm);
      }
      else
      { fprintf (stderr, "----- No operator meets goal -----\n");
	fprintf (outputFile, "none\n");
      }
      CloseOperatorQueue(opQueue); // inputFile gets closed here
    } break;
  case 's':  // Search for best predictor among collection of operators
    { char searchType[20];
      vector<Operator> ops;
      long first, last;
      double goal;
      fscanf (inputFile, "\nmodel: %s", modelFileName);
      fscanf (inputFile, "\ngoal: %lf", &goal);
      fscanf (inputFile, "\ntype: %s", searchType);
      fscanf (inputFile, "\nrange: %ld %ld",  &first, &last);
      switch (searchType[0])
      {
      case 'l':
      case 'L':
	ops = LinearOperators(first, last);
	break;
      case 'q':
      case 'Q':
	ops = QuadraticOperators(first, last);
	break;
      default: fprintf(stderr, "Do not recognize search type %s.\n", searchType);
      }
      gsm = ReadGSModelFromFile (modelFileName);
      fprintf(outputFile, "(searching to add to model %s)\n", modelFileName);
      fflush(outputFile);
      // Search over vector of operators
      Operator op = FindBestPredictorForGSModel (gsm, ops, outputFile);
      // Check to see if found op is good enough
      if (op->score > goal)
      { CommitPredictorToGSModel(gsm, op);
	PrintGSModel(gsm, "Model after committing");
	WriteGSModelToFile (gsm);
      }
      else
      { fprintf (stderr, "----- Best operator does not meet search goal -----\n");
	fprintf (outputFile, "none\n");
      }
    } break;
  default:  // Print a message informing key was not found
    { fprintf (outputFile, "Key string %s not understood.\n", key);
      fprintf (stderr, "Could not parse command string\n");
    }
  }
  fclose(outputFile);
  return 0;
}
