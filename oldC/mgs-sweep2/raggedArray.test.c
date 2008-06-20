// $Id: raggedArray.test.c,v 1.1 2001/08/13 21:28:07 bob Exp $

#include <stdio.h>

#include "raggedArray.h"
#include "matrix.h"

int main (void)
{
  RaggedArray ra, ra1, ra2;
  double x[1]={2.0};
  double y[2]={-1.0, 2.0};
  double z[3]={0.0, -1.0, 2.0};
  FILE *file;
  char *fileName = "test.ra";

  ra = NewRaggedArray(10,"test");
  AppendToRaggedArray(ra,x,1);
  AppendToRaggedArray(ra,y,2);
  AppendToRaggedArray(ra,z,3);
  PrintRaggedArray(ra,"Test with three members");

  printf("Writing to file %s two times\n", fileName);
  file = fopen(fileName, "w");
  WriteRaggedArrayToFile(ra,file);
  WriteRaggedArrayToFile(ra,file);
  fclose(file);
  
  file = fopen(fileName, "r");
  printf("Reading two back from file\n");
  ra1 = ReadRaggedArrayFromFile(file);
  ra2 = ReadRaggedArrayFromFile(file);
  fclose(file);
  PrintRaggedArray(ra1, "first array read from file");
  PrintRaggedArray(ra2, "second array read from file");

  Matrix mat = MatrixFromRaggedArray(ra);
  PrintMatrix (mat, "Initial matrix");
  Matrix inverse = InverseTriangularRaggedArray(ra);
  PrintMatrix (inverse, "Inverse via ragged array");
  Matrix inv = InverseMatrix(mat, "test");
  PrintMatrix (inv, "Inverse via matrix ops");
}
    
