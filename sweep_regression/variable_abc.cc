/*
    Variable.c implementation
 */

#include "variable_abc.h"

// Base class destructor

// Variable_abc::~Variable_abc()
// { cout << "ABC destructor called" << endl; }


// Variable library singleton class

VariableLibrary *VariableLibrary::sLibrary = 0;

int
VariableLibrary::register_pair(const string& s, Variable_abc *var)
{
  mDict[s] = var;
  return 1;
}


Variable_abc *
VariableLibrary::clone_object_from (istream &input)
{
  string tag;
  input >> tag;
  if (mDict[tag])
    return mDict[tag]->clone_from(input);
  else
  {
    cout << "Variable Library: tag " << tag << " not found.\n";
    return 0;
  } 
}

VariableLibrary*
VariableLibrary::get_library()
{
  if(sLibrary == 0)
    sLibrary = new VariableLibrary;
  return sLibrary;
}



