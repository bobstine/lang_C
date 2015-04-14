/*
 *  read_utils.cc
 *  utils
 *
 *  Created by Robert Stine on 11/29/05.
 *  Copyright 2005. All rights reserved.
 *
 */

#include "read_utils.h"

using std::string;


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

//  Parse attributes

const string equalStr {"="};
const string commaStr {","};

std::map<string,string>
read_utils::parse_attributes_from_string (string const& line)
{ 
  std::cout << "TESTING: read_utils adding attributes from comma delimited paired assignment list:  " << line << std::endl;
  size_t pos0 = 0, pos1 = 0;
  std::map<string,string> attrMap;
  while (true)
  { pos1 = line.find(equalStr, pos0);
    if(pos1 == std::string::npos) break;            // not found
    string name = read_utils::trim(line.substr(pos0,pos1-pos0));
    pos0 = pos1+1;
    pos1 = line.find(commaStr, pos0);
    if(pos1 == std::string::npos) pos1=line.size(); // no more options
    string value = read_utils::trim(line.substr(pos0,pos1-pos0));
    std::cout << "TESTING: name = " << name << "  value = " << value << std::endl;
    attrMap[name]=value;
    pos0 = pos1+1;
  }
  return attrMap;
}


//  This section of code handles standard C++ input

#include <string>
#include <sstream>

std::istream&
operator>>(std::istream& input, std::vector<double>& vec)
{
  string line;
  if(getline(input,line)) {
    std::istringstream ss(line);
    double x;
    while (ss >> x)
      vec.push_back(x);
  }
  return input;
}


//

string
get_word_from_string(string const& s)
{
  string r;

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
  while (--maxLength > 0 && (c = (char)getc(fp)) != EOF)
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
      if ((*cs++ = (char)c) == '\n')
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

string type_traits<double>::scanStr ("%lf");
string type_traits<int>::scanStr ("%ld");


