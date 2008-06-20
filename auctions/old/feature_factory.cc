// $Id: feature_factory.cc,v 1.1 2008/01/20 19:55:40 bob Exp $

#include "feature_factory.h"
#include <algorithm>

////  Initialization

void
FeatureFactory::convert_initial_column_vector()
{
  for (ColumnVector::const_iterator it = mColumns.begin(); it != mColumns.end(); ++it)
    make_column_feature_ptr(*it);
  std::cout << "FFAC: Initialization converted " << mColumns.size() << " columns into features.\n";
}


////  Accessor

struct compare_featureABCs
{
  bool
  operator()(const FeatureABC* lhs,const FeatureABC* rhs)
  {
    assert(lhs != 0);
    assert(rhs != 0);
    return ((*lhs) < rhs);
  }
};

FeatureFactory::FeatureVector
FeatureFactory::features (std::string const& className) const
{
  FeatureVector fv;
  FeatureMap    fm;
  if (className == "ColumnFeature")
    fm = mColumnMap;
  else if (className == "InteractionFeature")
    fm = mInteractionMap;
  else if (className == "LinearCombinationFeature")
    fm = mLinearCombinationMap;
  else if (className == "UnaryFeature")
    fm = mUnaryMap;
  else if (className == "BinaryFeature")
    fm == mBinaryMap;
  else
  { std::cout << "FFAC: Class name " << className << " not recognized.\n";
    return fv;
  }
  for(FeatureMap::const_iterator it = fm.begin(); it != fm.end(); ++it)
    fv.push_back(it->second);
  std::cout << "FFAC: Returned " << fv.size() << " features of class " << className << std::endl;
  std::sort(fv.begin(),fv.end(),compare_featureABCs());
  return fv;
}

////  Free space

void
FeatureFactory::free_column_memory()
{
  std::cout << "FFAC: Deleting memory for column features.\n";
  for(std::vector<Column>::iterator it = mColumns.begin(); it < mColumns.end(); ++it)
  {
    if (it->memory())
      delete it->memory();
    else std::cout << "FFAC: Attempted to delete memory that has already been deleted.\n";
  }
}


////  Basic constructors

Column*
FeatureFactory::make_empty_column_ptr(std::string const& name, int n)
{
  if (mColumns.size()>0 && n != mColumns[0].size())
  { std::cout << "FFAC: *** Error *** Lengths of columns do not match.\n";
    return new Column("Empty",0,0);
  }
  std::cout << "FFAC: Making empty column (" << name << ") with " << n << " rows.\n";
  double *x = new double[n];
  return new Column(name.c_str(), x, x+n);
}

Column*
FeatureFactory::make_empty_column_ptr(std::string const& name)
{
  return make_empty_column_ptr(name, mColumns[0].size());
}


ColumnFeature*
FeatureFactory::make_column_feature_ptr(std::string const& name, std::vector<double> const& v)
{
  Column *colPtr (make_empty_column_ptr(name, v.size()));
  // make raw pointer range
  std::copy(v.begin(), v.end(), begin(colPtr->range()));
  // make column to hold these pointers
  mColumns.push_back(*colPtr);
  // return feature pointer
  return make_column_feature_ptr(*colPtr);
}
  
ColumnFeature*
FeatureFactory::make_column_feature_ptr(Column const& c)
{
  ColumnFeature *f;
  f = dynamic_cast<ColumnFeature*> (mColumnMap[c.name()]);
  if (not f)
  { f = new ColumnFeature(c);
    mColumnMap[c.name()] = f;
  }
  return f;
}

InteractionFeature*
FeatureFactory::make_interaction_feature_ptr(FeatureABC const* f1, FeatureABC const* f2)
{
  InteractionFeature *f = new InteractionFeature(f1,f2);
  assert(f);
  std::string newName (f->name());
  InteractionFeature *old = dynamic_cast<InteractionFeature*> (mInteractionMap[newName]);
  if (old)
  { delete f;
    return old;
  }
  else
  { mInteractionMap[newName] = f;
    return f;
  }
}

LinearCombinationFeature*
FeatureFactory::make_linear_combination_feature_ptr(std::vector<double> const& b,
						    std::vector<FeatureABC*> const& fv)
{
  Column* colPtr (make_empty_column_ptr("lin_comb"));
  LinearCombinationFeature *f = new LinearCombinationFeature(b,fv,colPtr);
  assert(f);
  std::string newName (f->name());
  LinearCombinationFeature *old = dynamic_cast<LinearCombinationFeature*> (mLinearCombinationMap[newName]);
  if (old)
  { delete f;
    delete colPtr;
    std::cout << "FFAC: Returning old linear combination.\n";
    return old;
  }
  else
  { mLinearCombinationMap[newName] = f;
    return f;
  }
}


////  Stream Constructors
void
FeatureFactory::build_stream_constructor_map()
{
  mStreamConstructorMap["ColumnFeature"]            = &FeatureFactory::make_column_feature_ptr_from_stream;
  mStreamConstructorMap["InteractionFeature"]       = &FeatureFactory::make_interaction_feature_ptr_from_stream;
  mStreamConstructorMap["LinearCombinationFeature"] = &FeatureFactory::make_linear_combination_feature_ptr_from_stream;
  mStreamConstructorMap["UnaryFeature"]             = &FeatureFactory::make_unary_feature_ptr_from_stream;
  mStreamConstructorMap["BinaryFeature"]            = &FeatureFactory::make_binary_feature_ptr_from_stream;
  
}

FeatureABC*
FeatureFactory::make_feature_ptr_from_stream(std::istream& is, FeatureVector const& fv)
{
  std::string className;
  is >> className;
  Constructor c (mStreamConstructorMap[className]);
  // std::cout << " *** making a feature with class name " << className << std::endl;
  if (c)
    return (this->*c)(is, fv);
  else
  {
    if (className != "")
      std::cout << "\nFFAC: *** Error *** Could not construct class [" << className << "] from input stream.\n";
    return 0;
  }
}


FeatureABC*
FeatureFactory::find_feature (FeatureABC const* f, FeatureVector const& fv)
{
  for (FeatureVector::const_iterator b = fv.begin(); b != fv.end(); ++b)
  {
    if ((*b) == f)
      return *b;
  }
  return 0;
}

FeatureABC*
FeatureFactory::find_feature (std::string const& name, FeatureVector const& fv)
{
  for (FeatureVector::const_iterator b = fv.begin(); b != fv.end(); ++b)
  {
    if ((*b)->name() == name)
      return *b;
  }
  return 0;
}

FeatureABC*
FeatureFactory::make_column_feature_ptr_from_stream(std::istream& is, FeatureVector const& source)
{
  std::string name;
  is >> name;
  FeatureABC* f = find_feature(name, source);
  if (f)
  { f->read_from(is);
    return f;
  }
  else
  { std::cout << "FFAC: Could not find column feature " << name << " in source.\n";
    return 0;
  }
}


FeatureABC*
FeatureFactory::make_interaction_feature_ptr_from_stream (std::istream& is, FeatureVector const& source) 
{
  FeatureABC* f1 = make_feature_ptr_from_stream(is, source);
  FeatureABC* f2 = make_feature_ptr_from_stream(is, source);
  if (f1 && f2)
  { 
    FeatureABC *f =  make_interaction_feature_ptr(f1,f2);
    f->read_from(is);
    return f;
  }
  else
    std::cout << "FFAC: Unable to create features for interaction from stream.\n";
  return 0;
}


FeatureABC*
FeatureFactory::make_linear_combination_feature_ptr_from_stream (std::istream& is, FeatureVector const& source) 
{
  // read size
  int k;
  is >> k;
  // read weights
  std::vector<double> b(k);
  for (int j=0; j<k; ++j)
    is >> b[j];
  std::vector<FeatureABC*> features;
  for (int j=0; j<k-1; ++j)
  { features.push_back(make_feature_ptr_from_stream(is, source));
    if (features.back() == 0)
    { std::cout << "FFAC: Unable to create feature for linear combination.\n";
      return 0;
    }
  }
  FeatureABC *f = make_linear_combination_feature_ptr(b,features);
  if (f)
  { f->read_from(is);
    return f;
  }
  else
    std::cout << "FFAC: Unable to create linear combination from stream.\n";
  return 0;
}

FeatureABC*
FeatureFactory::make_unary_feature_ptr_from_stream (std::istream& is, FeatureVector const& source) 
{
  char opSym;
  is >> opSym;
  FeatureABC *result (0);
  FeatureABC *f(0);
  switch (opSym)
  {
    // simple operators do not require parameters and can proceed to make features directly from input
  case 'L':
    f = make_feature_ptr_from_stream(is, source);
    if (f)
      result = make_unary_feature_ptr(Function_Utils::LogisticPos(),f);
    break;
  case '^':
    f = make_feature_ptr_from_stream(is, source);
    if (f)
      result = make_unary_feature_ptr(Function_Utils::Square(),f);
    break;
    // spline operator needs to read its parameters before constructing the arguments
  case 'S':
    { SmoothingSplineOperator ssOp (is);
      f = make_feature_ptr_from_stream(is, source);
      if (f)
	result = make_unary_feature_ptr(ssOp,f);
    }
      break;
  default:
    std::cout << "FFAC: Did not recognize unary operator symbol " << opSym << std::endl;
  }
  if (result)
    result->read_from(is);
  else
    std::cout << "FFAC: *** Error *** Unable to create argument for unary feature from stream.\n";
  return result;
}


FeatureABC*
FeatureFactory::make_binary_feature_ptr_from_stream (std::istream& is, FeatureVector const& source) 
{
  char opSym;
  is >> opSym;
  FeatureABC* f1 = make_feature_ptr_from_stream(is, source);
  FeatureABC* f2 = make_feature_ptr_from_stream(is, source);
  if (f1 && f2)
  {
    FeatureABC *f (0);
    switch (opSym)
    {
    case '*':
      f = make_binary_feature_ptr(std::multiplies<double>(),f1,f2);
      break;
    case '+':
      f = make_binary_feature_ptr(std::plus<double>(),f1,f2);
      break;
    default:
      std::cout << "FFAC: Did not recognize binary operator symbol " << opSym << std::endl;
    }
    if (f) f->read_from(is);
    return f;
  }
  else
    std::cout << "FFAC: Unable to create arguments for binary feature from stream.\n";
  return 0;
}


