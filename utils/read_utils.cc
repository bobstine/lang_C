/*
 *  read_utils.cc
 *  utils
 *
 *  Created by Robert Stine on 11/29/05.
 *  Copyright 2005. All rights reserved.
 *
 */

#include "read_utils.h"


//  This section of code handles standard C++ input

#include <string>
#include <sstream>

std::istream&
operator>>(std::istream& input, std::vector<double>& vec)
{
  std::string line;
  if(getline(input,line)) {
    std::istringstream ss(line);
    double x;
    while (ss >> x)
      vec.push_back(x);
  }
  return input;
}


//  This section is the scanf style input

#include <cstdio>
const int maxNameLength = 255;

bool
read_string(char *s, int n, register FILE *iop)
{
  register int c;
  register char *cs;
  cs = s;
  while (--n > 0 && (c = getc(iop)) != EOF)
  {
    if (c != ' ')
      if ((*cs++ = c) == '\n')
				break;
  }
  --cs;
  *cs='\0';
  return (cs == s);
}

int
read_int_from_file(FILE *input)
{
	int i(0);
	fscanf(input, "%d", &i);
	return i;
}


int
fill_vector_from_file(FILE* inputFile, std::vector<double>::iterator xIter)
{
  const int n(read_int_from_file(inputFile));
  return fill_vector_from_file(inputFile, n, xIter);
}


int
fill_vector_from_file(FILE* inputFile, int n, std::vector<double>::iterator xIter)
{
  input_file_iterator<double> fileIter(inputFile);
  int i;
  for(i=0; inputFile && (i<n); ++i)
  { *xIter = *fileIter;
    ++fileIter;
    ++xIter;
  }
  return i;
}


// type traits

std::string type_traits<double>::scanStr ("%lf");
std::string type_traits<int>::scanStr ("%ld");


