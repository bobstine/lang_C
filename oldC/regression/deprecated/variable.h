// $Id: variable.h,v 1.1 2003/11/26 16:55:47 bob Exp $

/*

 Variables are unary functions defined on observations, having a
 string type identifier and an operator() function.  We need virtual
 functions so that we can have a list of variables, each doing
 different sorts of things.  Since the class is not known at run time,
 we use the pImple sort of wrapper, via a mixin Envelope.

 Variables are just the lightweight wrapper, an envelope embellished
 to pass along the underlying virtual function calls, so these should
 be passed around *by value*, as Variable x rather than const Variable&.

  2 Jan 03 ... Work on version for derived variables with envelope mixin, add function
 18 Dec 02 ... Transformed variables.
 11 Feb 02 ... Add centered variable type.
 13 Dec 01 ... Addition of interactions, with reformulated product vars.
 29 Nov 01 ... Converted from old operators to be real objects.
  7 Jul 01 ... Created (as C operators on the road in Ohio!)

*/

#ifndef _VARIABLE_H_
#define _VARIABLE_H_

#include "envelope.h"
#include "observation.h"
#include "range_ops.h"
#include "function_utils.h"

#include <functional>    // define unary_function
#include <set>
#include <list>
#include <iostream>
#include <string>


/////////////////////  Abstract base class  ////////////////////////////

class Variable;

class Variable_abc : public std::unary_function<Observation, double>
{
 public:
  virtual ~Variable_abc();   // implement in .cc for linker location

  virtual double              operator()(Observation const& obs)             const = 0;
  virtual std::set<int>       base_indices()                                 const = 0;
  virtual std::set<Variable>  next_variables()                               const = 0;
  virtual std::set<Variable>  base_variables()                               const = 0;
  virtual std::list<Variable> parse_list()                                   const = 0;
  virtual void                write_to (std::ostream& output)                const = 0;
  virtual std::string         type ()                                        const = 0;
  virtual Variable_abc*       clone()                                        const = 0;
  virtual bool                equal_assuming_same_type (Variable_abc *ptr)   const = 0;
  virtual bool                less_assuming_same_type (Variable_abc *ptr)    const = 0;

  bool operator==(Variable_abc* ptr)
    {
      if (typeid(*ptr)==typeid(*this))
	return equal_assuming_same_type(ptr);
      else return 0;
    }
  
  bool operator<(Variable_abc* ptr)
    {
      if (typeid(*ptr)==typeid(*this))
	return less_assuming_same_type(ptr);
      else
	return typeid(*this).before(typeid(*ptr));
    }

};

//////////////////  Variable envelope class  ///////////////////////////


class Variable: public Envelope<Variable_abc>,  // public for benefit of compiler
		public Variable_abc
{  
 public:
  Variable()  : Envelope<Variable_abc>(0) { };
  Variable(Variable_abc* pVar)  : Envelope<Variable_abc>(pVar) { };
  
  double              operator()(const Observation& obs)  const { return (*mPtr)(obs); }
  std::set<int>       base_indices()                      const { return mPtr->base_indices(); }
  std::set<Variable>  next_variables()                    const { return mPtr->next_variables();}
  std::set<Variable>  base_variables()                    const { return mPtr->base_variables();}
  std::list<Variable> parse_list()                        const { return mPtr->parse_list(); }
  void                write_to(std::ostream &output)      const { mPtr->write_to(output); }
  std::string         type()                              const { return mPtr->type(); }

  Variable_abc* clone() const
    { assert("this function should not be called" == 0); return 0;  }
  
  bool equal_assuming_same_type (Variable_abc *ptr) const
    { assert("this function should not be called" == 0); return ptr == 0; }
  bool less_assuming_same_type (Variable_abc *ptr) const
    { assert("this function should not be called" == 0); return ptr == 0; }

  bool
    operator==(Variable const& var) const   { return mPtr->operator==(var.mPtr); }
  bool
    operator<(Variable const& var) const    { return mPtr->operator<(var.mPtr); }

  void
    operator=(Variable const& var)          { Envelope<Variable_abc>::operator=(var);  }

};

std::ostream&
operator<<(std::ostream &output, Variable const& var);


/////////////////////////  Index Variable  //////////////////////////////

Variable make_index_variable(int index);

class IndexVariable: public Variable_abc
{
 private:
  int mIndex;
  
 public:
  IndexVariable (int index)
    : mIndex(index) { }

  double operator()(const Observation& obs) const    { return obs.element(mIndex); }

  std::set<int> base_indices() const              { std::set<int> s; s.insert(mIndex); return s;}
  std::set<Variable> next_variables() const       { return std::set<Variable>(); } 
  std::set<Variable> base_variables() const       { std::set<Variable> s; s.insert(make_index_variable(mIndex)); return s; }
  std::list<Variable> parse_list() const          { std::list<Variable> l; l.push_front(make_index_variable(mIndex)); return l; }
  void write_to (std::ostream &output) const      { output << "_" <<  mIndex ; }
  std::string type() const                        { return "Index";}
  IndexVariable* clone() const                    { return new IndexVariable(mIndex);    }

  bool equal_assuming_same_type (Variable_abc *ptr) const
    {
      if (IndexVariable* iv = dynamic_cast<IndexVariable*>(ptr))
	return (mIndex == iv->mIndex);
      else
      {	assert("Logic error lead to incorrect cast."==0);
	return false;
      }
    }                                              
  bool less_assuming_same_type (Variable_abc *ptr) const
    {
      if (IndexVariable* iv = dynamic_cast<IndexVariable*>(ptr))
	return (mIndex < iv->mIndex);
      else
      {	assert("Logic error lead to incorrect cast."==0);
	return false;
      }
    }
  
 private:
  static const std::string sType;
};

inline
Variable
make_index_variable(int index)
{
  return Variable(new IndexVariable(index));
}

/////////////////////////  Transformed Variable  //////////////////////////////


template<class UnaryOp>
Variable
make_transformed_variable(UnaryOp f, Variable var, std::string const& label);


template<class F>
class TransformedVariable: public Variable_abc
{
 private:
  F mF;
  Variable mVar;
  std::string mLabel;

 public:
  TransformedVariable (F f, Variable var, std::string const& label)
    : mF(f), mVar(var), mLabel(label) { }

  double operator()(const Observation& obs) const    { return mF(mVar(obs)); }

  std::set<int> base_indices() const             { return mVar.base_indices(); }
  std::set<Variable> next_variables() const      { std::set<Variable>s;
                                                   s.insert(mVar); return s;}
  std::set<Variable> base_variables() const      { std::set<Variable>s(mVar.base_variables());
                                                   s.insert(mVar); return s;}
  std::list<Variable> parse_list() const         { std::list<Variable> l(mVar.parse_list());
                                                   l.push_back(make_transformed_variable(mF,mVar,mLabel)); return l; }

  void write_to (std::ostream &output) const     { output << mLabel << "(" <<  mVar << ")" ; }
  std::string type() const                       { return "Trans"; }
  TransformedVariable* clone() const             { return new TransformedVariable<F>(mF,mVar,mLabel);    }
  
  bool equal_assuming_same_type (Variable_abc *ptr) const
    {
      if (TransformedVariable<F>* tv = dynamic_cast<TransformedVariable<F>*>(ptr))
	return (mVar == tv->mVar);
      else
      { assert("Logic error lead to incorrect cast."==0);
	return false;
      }
    }                                              

  bool less_assuming_same_type (Variable_abc *ptr) const
    {
      if (TransformedVariable<F>* tv = dynamic_cast<TransformedVariable<F>*>(ptr))
	return (mVar < tv->mVar);
      else
      { assert("Logic error lead to incorrect cast."==0);
	return false;
      }
    }                                              

  const std::string& label() const                   { return mLabel; }
};

template<class UnaryOp>
inline
Variable
make_transformed_variable(UnaryOp f, Variable var, std::string const& label)
{
  return Variable(new TransformedVariable<UnaryOp>(f,var,label));
}

/////////////////////////  Binary Variable  //////////////////////////////

template<class BinaryOp>
inline
Variable
make_binary_variable(BinaryOp f, Variable var1, Variable var2, std::string const& label);

namespace{
  using std::set;
  using std::list;
  
  template<typename T>
    set<T> join_sets (set<T> const& c1, set<T> const& c2)
    {
      set<T> result(c1);
      result.insert(c2.begin(), c2.end());
      return result;
    }

  template<typename T>
    list<T> join_lists (list<T> const& c1, list<T> const& c2)
    {
      list<T> result(c1);
      std::copy(c2.begin(), c2.end(), std::back_inserter< list<T> >(result));
      return result;
    }
}

template<class F>
class BinaryVariable: public Variable_abc
{
 private:
  F mF;
  Variable mVar1;
  Variable mVar2;
  std::string mLabel;

 public:
  BinaryVariable (F f, Variable var1, Variable var2, std::string const& label)
    : mF(f), mVar1(var1), mVar2(var2), mLabel(label) { }

  double operator()(const Observation& obs) const    { return mF(mVar1(obs),mVar2(obs)); }

  std::set<int> base_indices() const            { return join_sets(mVar1.base_indices(), mVar2.base_indices()); }
  std::set<Variable> next_variables() const     { std::set<Variable> s;
                                                  s.insert(mVar1); s.insert(mVar2);
                                                  return s;}
  std::set<Variable> base_variables() const     { std::set<Variable> s(join_sets(mVar1.base_variables(),mVar2.base_variables()));
                                                  s.insert(mVar1); s.insert(mVar2);
                                                  return s;}
  std::list<Variable> parse_list() const        { std::list<Variable> l(join_lists(mVar1.parse_list(),mVar2.parse_list()));
                                                   l.push_back(make_binary_variable(mF,mVar1,mVar2,mLabel)); return l; }
  void write_to (std::ostream &output) const    { output << "(" <<  mVar1 << " " << mLabel << " " << mVar2 << ")" ; }
  std::string type() const                      { return "Binary"; }
  BinaryVariable* clone() const                 { return new BinaryVariable<F>(mF,mVar1,mVar2,mLabel);    }
  
  bool equal_assuming_same_type (Variable_abc *ptr) const
    {
      if (BinaryVariable<F>* bv = dynamic_cast<BinaryVariable<F>*>(ptr))
	return (mVar1 == bv->mVar1 && mVar2 == bv->mVar2);
      else
      {	assert("Logic error lead to incorrect cast."==0);
	return false;
      }
    }                                              

  bool less_assuming_same_type (Variable_abc *ptr) const
    {
      if (BinaryVariable<F>* bv = dynamic_cast<BinaryVariable<F>*>(ptr))
	if (mVar1 < bv->mVar1)
	  return true;
	else
	  if (mVar1 == bv->mVar1)
	    return mVar2 < bv->mVar2;
	  else
	    return false;
      else
      {	assert("Logic error lead to incorrect cast."==0);
	return false;
      }
    }                                              

  const std::string& label() const                   { return mLabel; }
};

template<class BinaryOp>
inline
Variable
make_binary_variable(BinaryOp f, Variable var1, Variable var2, std::string const& label)
{
  return Variable(new BinaryVariable<BinaryOp>(f,var1,var2,label));
}


#endif
