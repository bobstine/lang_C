// $Id: feature_factory.Template.h,v 1.1 2008/01/20 19:55:40 bob Exp $

template<class Iter>
void
FeatureFactory::append_features_from_stream(std::istream& is, FeatureVector const& fv, Iter it)
{
  int count (0);
  while (is)
  { FeatureABC* f = make_feature_ptr_from_stream(is,fv);
    if (f)
    { ++count;
      *it = f;
      ++it;
    }
    else break; 
  }
  std::cout << "FFAC: Appended " << count << " features from input stream.\n";
}

template<class Op>
UnaryFeature<Op>*
FeatureFactory::make_unary_feature_ptr(Op const& op, FeatureABC const* f)
{
  UnaryFeature<Op> *uf = new UnaryFeature<Op>(op,f);
  assert(uf);
  std::string newName (uf->name());
  UnaryFeature<Op> *old = dynamic_cast<UnaryFeature<Op>*>(mUnaryMap[newName]);
  if (old)
  { delete uf;
    return old;
  }
  else
  { mUnaryMap[newName] = uf;
    return uf;
  }
}


template<class Op>
BinaryFeature<Op>*
FeatureFactory::make_binary_feature_ptr(Op const& op, FeatureABC const* f1, FeatureABC const* f2)
{
  BinaryFeature<Op> *f = new BinaryFeature<Op>(op,f1,f2);
  assert(f);
  std::string newName (f->name());
  BinaryFeature<Op> *old = dynamic_cast<BinaryFeature<Op>*>(mBinaryMap[newName]);
  if (old)
  { delete f;
    return old;
  }
  else
  { mBinaryMap[newName] = f;
    return f;
  }
}



