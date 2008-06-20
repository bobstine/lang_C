// $Id: operator.test.c,v 1.1 2005/06/13 20:47:51 bob Exp $

#include <stdio.h>

#include "operator.h"

int main (void)
{
  Operator f;
  float x[5]={0.0, 1.0, 2.0, 3.0, 4.0};
  FILE *file;
  char *fileName = "test.ops";

  file = fopen(fileName, "r");
  f = ParseOperatorFromFile(file);
  printf ("Result of calling operator: %8.3f\n", EvalOperator(f,x));
  fclose(file);
  
}
    
