#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "dataArray.h"
#include "memory.h"

// --------------------------------------------------------------------------

void Wait(void);
void PrintDuration(clock_t, clock_t);
void BuildFileName (char *prefix, char *buffer);


void PrintDuration(clock_t start, clock_t finish)
{
  printf ("    Required =  %f  seconds.  Any character continues.\n",
	  ((double) (finish - start)) / CLOCKS_PER_SEC);
  Wait();
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
  strcat(buffer, ".dat");
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
  {
    for (j=0; j<nCols; ++j)
    { x = i+j;
      fprintf(fp, "%f ", x);
    }
    fprintf(fp, "\n");
  }
  printf ("Wrote %ld rows with %ld columns to %s\n", nRows, nCols, fileName);
  fclose(fp);
  return 1;
}

float
AddTwoColumns (float *x, long *cols, float *args)
{
  double result = x[1]+x[2];
  return result;
}

int main(void)
{
  DataArray da;
  double avg;
  int result;
  long nPad=1;
  long nRows=50, nCols=7;
  clock_t startTime, stopTime;
  char fileName[80];

  BuildFileName ("test", fileName);
  startTime = clock();
  GenerateTestDataFile(fileName, nRows, nCols);
  stopTime= clock();
  PrintDuration(startTime, stopTime);

  startTime = clock();
  da = ReadDataArray (fileName, nPad);
  stopTime= clock();
  printf ("Have read the data array\n");
  PrintDuration(startTime, stopTime);

  PrintDataArray(da, "Data array from file", 5, 5);
  avg = ApplyOperatorToDataArray (da, AddTwoColumns, NULL, NULL, 0); //  weights
  printf ("Avg = %8.3f\n", avg);
  
  BuildFileName ("test2", fileName);
  startTime = clock();
  result = WriteDataArray (da, fileName);
  stopTime= clock();
  printf ("Have written the data array\n");
  PrintDuration(startTime, stopTime);
  
  PrintMemoryUse();
  return 1;
}
