
//  Feature constructors  (define after defining the different types of features)

template<class Op>
Feature::Feature(Op const& op, Feature const &x)
{
  mFP = new UnaryFeature<Op>(op,x);
}


template<class Op>
Feature::Feature(Op const& op, Feature const &x1, Feature const& x2)
{
  mFP = new BinaryFeature<Op>(op,x1,x2);
}



//  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature


template<class Op>
FeatureABC::Arguments
UnaryFeature<Op>::arguments()  const
{
  Arguments a;
  std::string opName (operator_traits<Op>::name());
  std::string fName  (mFeature->name());
  
  if (opName == "square")
    a[fName] = 2;
  else if (opName == "cube")
    a[fName] = 3;
  else if (opName == "fourth")
    a[fName] = 4;
  else if (opName == "fifth")
    a[fName] = 5;
  else if (opName == "sixth")
    a[fName] = 6;
  else
    a[name()] = 1;
  return a;
}


template<class Op>
void
UnaryFeature<Op>::write_to (std::ostream& os) const
{
  os << class_name() << " " << operator_traits<Op>::symbol() << " " << operator_traits<Op>::parameters(mOp) << std::endl;
  mFeature->write_to(os);
  FeatureABC::write_to(os);
}


//  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  

template<class Op>
void
BinaryFeature<Op>::write_to (std::ostream& os) const
{
  os << class_name() << " " <<  operator_traits<Op>::symbol() << " " << operator_traits<Op>::parameters(mOp) << std::endl;
  mFeature1->write_to(os);
  mFeature2->write_to(os);
  FeatureABC::write_to(os);
}


