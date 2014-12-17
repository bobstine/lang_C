
// operator<<

inline
std::ostream&
operator<< (std::ostream& os, Feature const& feature)
{
  feature->print_to(os);
  FeatureABC::DependenceMap m = feature->dependence_map();
  if (!m.empty())
  { os << std::endl << "                 Dependence map is   { ";
    std::for_each(m.begin(), m.end(),
		  [&os] (FeatureABC::DependenceMap::value_type const& p)
		  { os << " (" << p.first->name();
		    if(p.second>1)
		      os << "^" << p.second;
		    os << ")"; }
		  );
    os << " } ";
  }
  return os;
}


inline
std::ostream&
operator<< (std::ostream& os, FeatureVector const& featureVec)
{
  int max (10);
  int n   (featureVec.size());
  int show = (max < n) ? max : n;
  for (int i = 0; i < show; ++i)
  { featureVec[i]->print_to(os);
    os << std::endl;
  }
  if (max < n)
  { os << "   ..... " << n-show-1 << " .....\n";
    featureVec[n-1]->print_to(os);
    os << std::endl;
  }
  return os;
}



////  Column     Column     Column     Column     Column     Column     Column     Column     Column     Column     

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


