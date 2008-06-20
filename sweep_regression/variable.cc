/*
    Variable.c implementation
 */

#include "variable.h"
#include <assert.h>

// Variable print

ostream&
operator<<(ostream &output, const Variable& variable)
{ variable.write_to(output); return output; }

///////////////////  Variable  ////////////////////////////

Variable&
Variable::operator= (const Variable &var)
{
  cout << "Variable assignment constructor" << endl;
  if (this != &var)
    this->mPimple = var.mPimple->clone();
  return *this;
}

///////////////////  Index variable  //////////////////////

void
IndexVariable::write_to (ostream &output) const
{
  output << IndexVariable::sType
	 << " " <<  mIndex << " " ;
}

IndexVariable *
IndexVariable::clone() const
{
  return new IndexVariable(mIndex);
}

IndexVariable *
IndexVariable::clone_from(istream &input) const
{
  IndexVariable *iv = clone();
  input >> (*iv).mIndex;
  return iv;
}

double
IndexVariable::operator()(const Observation& obs) const
{
  // cout << "indexing " << obs << " at " << mIndex << endl;
  return obs.element(mIndex);
}

const string IndexVariable::sType("Index");

const int IndexVariable::sRegistration =
VariableLibrary::get_library()->register_pair(IndexVariable::sType, new IndexVariable(-1));

Variable_abc*
index_variable_ptr (long index)
{
  return new IndexVariable(index);
}


//////////  ProductVariable  /////////////


void
ProductVariable::write_to (ostream &output) const
{
  output << ProductVariable::sType
	 << " " << mIndex1 << " " << mIndex2 << " ";
}

ProductVariable *
ProductVariable::clone () const
{
  return new ProductVariable(this->mIndex1, this->mIndex2);
}

ProductVariable *
ProductVariable::clone_from(istream &input) const
{
  ProductVariable *iv = clone();
  input >> (*iv).mIndex1 >> (*iv).mIndex2;
  return iv;
}

double
ProductVariable::operator()(const Observation& obs) const
{
  return obs.element(mIndex1) * obs.element(mIndex2);
}

const string ProductVariable::sType("Product");

const int ProductVariable::sRegistration =
VariableLibrary::get_library()->
register_pair(ProductVariable::sType, new ProductVariable(-1,-1));


Variable_abc*
product_variable_ptr (long index1, long index2)
{
  return new ProductVariable(index1, index2);
}

///////////////////  Interaction  /////////////////////////


void
Interaction::write_to (ostream &output) const
{
  output << "Interaction    " << mVar1 << " x " << mVar2 << " ";
}


Interaction*
Interaction::clone() const
{
  return new Interaction(mVar1, mVar2);
}

Interaction*
Interaction::clone_from(istream& input) const
{
  assert("Cannot clone from a stream"==0);
  return 0;
}

Variable
operator* (const Variable& lhs, const Variable& rhs)
{
  Variable var (lhs, rhs);
  return var;
}

const string Interaction::sType("Interaction");

Variable_abc*
interaction_ptr (const Variable& a, const Variable& b)
{
  return new Interaction(a,b);
}


///////////////////  Centered Variable  /////////////////////////


void
CenteredVariable::write_to (ostream &output) const
{
  output << "Centered    " << mVar << " at " << mCenter << " ";
}

CenteredVariable*
CenteredVariable::clone() const
{
  return new CenteredVariable(mVar, mCenter);
}

CenteredVariable*
CenteredVariable::clone_from(istream& input) const
{
  assert("Cant clone centered var from stream\n" == 0);
  return 0;
}

const string CenteredVariable::sType("Centered");

Variable_abc*
centered_variable_ptr (const Variable& a, const double c)
{
   return new CenteredVariable(a,c);
}
