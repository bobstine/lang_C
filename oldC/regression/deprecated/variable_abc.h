// $Id: variable_abc.h,v 1.1 2003/11/26 20:05:26 bob Exp $

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

class Variable_abc : public std::unary_function<Observation, double> {
  friend class VariableLibrary;
  
 public:
  virtual ~Variable_abc() { }
  
  Variable_abc () { };

  virtual double operator()(const Observation& obs) const = 0;

  virtual Variable_abc* clone () const = 0;

  virtual void write_to (std::ostream& output) const = 0;

  virtual const std::string& type () const = 0;
  
 private:

  virtual Variable_abc* clone_from (std::istream& input) const = 0;

 protected:  // detect run-time errors at compile
  Variable_abc(const Variable_abc& var);
  void operator=(const Variable_abc& var);

};

class VariableLibrary {    // singleton
 public:
  int register_pair(const std::string& s, Variable_abc *var);

  Variable_abc* clone_object (const std::string& type);
  Variable_abc* clone_object_from (std::istream &input);
  
  static VariableLibrary* get_library();
    
 private:
  VariableLibrary() : mDict() { }
  
  static VariableLibrary* sLibrary;
  std::map<std::string, Variable_abc *> mDict;
};

#endif
