/*
 *  read_utils.cc
 *  utils
 *
 *  Created by Robert Stine on 11/29/05.
 *  Copyright 2005. All rights reserved.
 *
 */

#include "read_utils.h"


int
read_utils::ctoi(char c)
{
  switch(c) {
  case '0': return(0);
  case '1': return(1);
  case '2': return(2);
  case '3': return(3);
  case '4': return(4);
  case '5': return(5);
  case '6': return(6);
  case '7': return(7);
  case '8': return(8);
  case '9': return(9);
  case 'a':
  case 'A': return(10);
  case 'b':
  case 'B': return(11);
  case 'c':
  case 'C': return(12);
  case 'd':
  case 'D': return(13);
  case 'e':
  case 'E': return(14);
  case 'f':
  case 'F': return(15);
  default: return 0; break;
  }
}





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


//

std::string
get_word_from_string(std::string const& s)
{
  std::string r;

  for(std::string::const_iterator it=s.begin(); it != s.end(); ++it)
    if(' ' == *it)
      return r;
    else
      r.push_back(*it);
  return r;
}



//  This section is the scanf style input

#include <cstdio>

int
read_file_line (char *s, int maxLength, FILE *fp)
{
  int count (0);
  char c;
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
read_string(char *s, int n, FILE *iop)
{
  int c;
  char *cs;
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
	if (1 == fscanf(input, "%d", &i))
	  return i;
	else
	  return -1;
}



// type traits

std::string type_traits<double>::scanStr ("%lf");
std::string type_traits<int>::scanStr ("%ld");


