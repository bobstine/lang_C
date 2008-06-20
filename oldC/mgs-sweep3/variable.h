// $Id: variable.h,v 1.11 2002/03/12 18:36:45 bob Exp $

/*

 Variables are functions defined on observations.

 11 Feb 02 ... Add centered variable type.
 13 Dec 01 ... Addition of interactions, with reformulated product vars.
 29 Nov 01 ... Converted from old operators to be real objects.
  7 Jul 01 ... Created (as C operators on the road in Ohio!)

*/

#ifndef _VARIABLE_H_
#define _VARIABLE_H_

#include "variable_abc.h"
#include "observation.h"

#include <iostream>
#include <string>

////////////////  Variable envelope class  ///////////////////////////

class Variable;

 // How to make this "hidden or private"???
Variable_abc* index_variable_ptr (long index);
Variable_abc* product_variable_ptr (long index1, long index2);
Variable_abc* interaction_ptr (const Variable& v1, const Variable& v2);
Variable_abc* centered_variable_ptr (const Variable& v, const double c);


class Variable: public Variable_abc {
  Variable_abc *mPimple;
  
 public:
  ~Variable () {
    // cout << "Deleting variable; pimple = " << mPimple << endl;
    if (mPimple) delete mPimple;
  }
  
  Variable ()
    : mPimple()
    { cout << "Variable empty constructor    0" << endl; }
  Variable (long i)
    : mPimple(index_variable_ptr(i))
    { cout << "Variable index constructor   " << mPimple << endl; }
  Variable (long i1, long i2)
    : mPimple(product_variable_ptr(i1,i2))
    { cout << "Variable product constructor " << mPimple << endl; }    
  Variable (const Variable& v1, const Variable& v2)
    : mPimple(interaction_ptr(v1,v2))
    { cout << "Variable interaction constr  " << mPimple << endl; }
  Variable (const Variable& v, const double c)
  : mPimple(centered_variable_ptr(v,c))
    { cout << "Variable centered constr  " << mPimple << endl; }
  Variable (istream& input)
    : mPimple(VariableLibrary::get_library()->clone_object_from(input))
    { cout << "Variable stream constructor  " << mPimple << endl; }
  Variable (const Variable &var)
    : mPimple() {
    if (var.mPimple)
      mPimple = var.mPimple->clone();
    // cout <<   "Variable copy constructor    " << mPimple << endl; 
  }
  
  virtual Variable& operator= (const Variable& rhs);

  virtual double operator()(const Observation& obs) const
    { return (*mPimple)(obs); }

  virtual void write_to(ostream &output) const
    { mPimple->write_to(output); }

  virtual const string& type() const { return (*mPimple).type(); }

 private:
  Variable* clone () const
    { assert("This clone should not be called"==0); return 0; }
  
  Variable*
    clone_from (istream &input) const
    { assert("This clone_from should not be called"==0); return 0; }

};

ostream&
operator<<(ostream &output, const Variable& variable);


////////////////  Index Variable  /////////////////////////

class IndexVariable: public Variable_abc {
 public:
  IndexVariable (long index)
    :
    Variable_abc(),
    mIndex(index) { }

  void write_to (ostream &output) const;
      
  double operator()(const Observation& obs) const;

  const string& type() const {return sType;}
  
 private:
  long mIndex;
  static const int sRegistration;
  static const string sType;
  
  IndexVariable *clone() const;
  
  IndexVariable *clone_from(istream &input) const;
};


/////////////////  ProductVariables  /////////////////////// 

class ProductVariable: public Variable_abc {
 public:
  ProductVariable()
    :
    Variable_abc() { }
  ProductVariable (const long i1, const long i2)
    :
    Variable_abc(),
    mIndex1(i1), mIndex2(i2) { }

  void write_to (ostream &output) const;
    
  double operator()(const Observation& obs) const;

  const string& type() const {return sType;}
  
 private:
  long mIndex1, mIndex2;
  static const int    sRegistration;
  static const string sType;
  
  ProductVariable *clone() const;
  
  ProductVariable *clone_from(istream &input) const;

};


/////////////////  Interactions  /////////////////////// 

class Interaction: public Variable_abc {
 public:
  Interaction()
    : Variable_abc() { }
  Interaction (const Variable& v1, const Variable& v2)
    : Variable_abc(),
    mVar1(v1), mVar2(v2) { }

  void write_to (ostream &output) const;
    
  double operator()(const Observation& obs) const
    { return (mVar1(obs) * mVar2(obs)); }

  const string& type() const { return sType; }

  Interaction* clone() const;
  Interaction* clone_from(istream& input) const;
  
  friend Variable
    operator* (const Variable& lhs, const Variable& rhs);

 private:
  Variable mVar1, mVar2;
  static const string sType;
};


/////////////////  Centered Variable  /////////////////////// 

class CenteredVariable: public Variable_abc {
 public:
  CenteredVariable()
    : Variable_abc() { }
  CenteredVariable (const Variable& var, const double center)
    : Variable_abc(),
    mVar(var), mCenter(center) { }
  
  CenteredVariable* clone() const;
  CenteredVariable* clone_from(istream& input) const;
  
  void write_to (ostream &output) const;
  
  double operator()(const Observation& obs) const
    { return mVar(obs)-mCenter; }

  const string& type() const { return sType; }

 private:
  Variable mVar;
  double mCenter;
  static const string sType;
};

inline Variable
make_centered_variable (const Variable &var, const double c)
{
  return Variable(var,c);
}

#endif
