// $Id: variable.test.cc,v 1.1 2005/06/13 20:47:51 bob Exp $

#include "variable.h"

#include <fstream>
#include <iostream>
#include <string>
#include <strstream>
#include <vector>

istream&
operator>>(istream& input, vector<Variable>& vec)
{
  string line;

  while(getline(input,line)) {
    if (line.length()>0) {
      strstream ss;
      ss << line;
      cout << "Reading a var from line of length " << line.length() << endl;
      Variable var(ss);
      cout << var << endl;
      vec.push_back(var);
    }
  }
  return input;
}

int main (void)
{
  cout << "Defining two variables from input file \n";
  ifstream input ("test/variable");
  Variable var1(input);
  Variable var2(input);
  cout << var1 << endl << var2 << endl;

  cout << "Making an interaction of these two \n";
  Variable product = var1 * var2;
  cout << product << endl;

  cout << "Defining the vector" << endl;
  vector<Variable> varVec;
  cout << "Reading rest into the vector" << endl;
  input >> varVec;

  cout << "Printing the " << varVec.size() << " variables in the vector: \n";
  for(vector<Variable>::const_iterator it = varVec.begin();
      it < varVec.end(); ++it)
    cout << (*it);
  
  return 0;
}
    
