// $Id: my_features.Template.h,v 3.1 2008/01/30 03:57:01 bob Exp $

//  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature  UnaryFeature


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


