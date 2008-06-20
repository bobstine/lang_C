// $Id: features.Template.h,v 3.0 2004/11/19 18:58:36 foster Exp $


//  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature

template<class Op>
std::string
UnaryFeature<Op>::name() const
{
  return operator_traits<Op>::name() + "(" + mFeature->name() + ")";
}

template<class Op>
void
UnaryFeature<Op>::write_to (std::ostream& os) const
{
  os << "UnaryFeature " << operator_traits<Op>::symbol() << std::endl;
  mFeature->write_to(os);
}

template<class Op>
FeatureABC*
UnaryFeature<Op>::clone()         const
{
  return new UnaryFeature<Op>(mOp, mFeature->clone());
}



//  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature  BinaryFeature

template<class Op>
std::string
BinaryFeature<Op>::name() const
{
  return operator_traits<Op>::name() + "_(" + mFeature1->name() + ")"
    + operator_traits<Op>::symbol() + "(" + mFeature2->name() + ")";
}

template<class Op>
void
BinaryFeature<Op>::write_to (std::ostream& os) const
{
  os << "BinaryFeature " << operator_traits<Op>::symbol() << std::endl;
  mFeature1->write_to(os);
  mFeature2->write_to(os);
}

template<class Op>
FeatureABC*
BinaryFeature<Op>::clone()         const
{
  return new BinaryFeature<Op>(mOp, mFeature1, mFeature2);
}



