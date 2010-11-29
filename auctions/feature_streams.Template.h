template<class Iterator, class Trans>
  std::string
  FeatureStream<Iterator,Trans>::name()              const
{
  return mName;
}

template<class Iterator, class Trans>
  std::string
  FeatureStream<Iterator,Trans>::feature_name()              const
{
  if (const_has_feature())
    return mThread->first_output_name();
  return "empty/busy";
}

template<class Iterator, class Trans>
  void
  FeatureStream<Iterator, Trans>::print_to(std::ostream& os) const
{
  os <<  mName << " @ " << feature_name();
}



template<class Iterator, class Trans>
  int
  FeatureStream<Iterator, Trans>::number_remaining()          const
{
  return mIterator.number_remaining();
}


template<class Iterator, class Trans>
  bool
  FeatureStream<Iterator, Trans>::is_busy()                   const
{
  return !mThread.done();
}


template<class Iterator, class Trans>
  bool
  FeatureStream<Iterator,Trans>::is_empty()                  const
{
  if (!mThread.has_worker())
    return true;
  else
    return (mThread->empty());
}



template<class Iterator, class Trans>
  bool
  FeatureStream<Iterator,Trans>::const_has_feature()       const
{
  if (is_busy() || is_empty())  // does not try to make feature
    return false;
  else
    return true;
}


template<class Iterator, class Trans>
  bool
  FeatureStream<Iterator,Trans>::has_feature()
{
  if (is_busy())
    return false;
  else if (is_empty())          // start to make next
  { make_features();
    return false;
  }
  else
    return true;
}


template<class Iterator, class Trans>
  FeatureVector
  FeatureStream<Iterator,Trans>::pop()
{
  assert (has_feature());
  FeatureVector fv (mThread->output_features());
  make_features();
  return fv;
}


template<class Iterator, class Trans>
  void
  FeatureStream<Iterator,Trans>::make_features()
{ 
  if (mIterator.valid())
  { // load up the transform operator
    mTransform.input(*mIterator);
    // advance the iterator
    ++mIterator;
    // start thread on transformation
    mThread(mTransform);
  }
}
