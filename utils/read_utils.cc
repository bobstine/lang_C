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

int
read_file_line (char *s, int maxLength, FILE *fp)
{
  int count (0);
  register char c;
  while (--maxLength > 0 && (c = getc(fp)) != EOF)
  { *s = c;
    if (c == '\n')
      break;
    ++s;
    ++count;
  }
  *s = '\0';
  return count;
}


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



// type traits

std::string type_traits<double>::scanStr ("%lf");
std::string type_traits<int>::scanStr ("%ld");


