// $Id: variable_abc.h,v 1.1 2005/06/13 20:47:51 bob Exp $

/*

 Variables are functions defined on observations.
 
 29 Nov ... Converted from old operators to be real objects.
  7 Jul ... Created (as C operators on the road in Ohio!)

*/

#ifndef _VARIABLE_ABC_H_
#define _VARIABLE_ABC_H_

#include <iostream>
#include <string>
#include <map>

#include "observation.h"


class VariableLibrary;   // forward declaration

class Variable_abc : public unary_function<Observation, double> {
  friend VariableLibrary;
  
 public:
  virtual ~Variable_abc() { }
  
  Variable_abc () { };

  virtual double operator()(const Observation& obs) const = 0;

  virtual Variable_abc* clone () const = 0;

  virtual void write_to (ostream& output) const = 0;

  virtual const string& type () const = 0;
  
 private:

  virtual Variable_abc* clone_from (istream& input) const = 0;

 protected:  // detect run-time errors at compile
  Variable_abc(const Variable_abc& var);
  void operator=(const Variable_abc& var);

};

class VariableLibrary {    // singleton
 public:
  int register_pair(const string& s, Variable_abc *var);

  Variable_abc* clone_object (const string& type);
  Variable_abc* clone_object_from (istream &input);
  
  static VariableLibrary* get_library();
    
 private:
  VariableLibrary() : mDict() { }
  
  static VariableLibrary* sLibrary;
  map<string, Variable_abc *> mDict;
};

#endif
