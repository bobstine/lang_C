#include "feature_iterators.h"

//  LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator

LagIterator&
LagIterator::operator++()
{
  ++mLag; --mRemaining;
  if ((mLag > mMaxLag) && (mRemaining>0))
    mLag = 1;  // go around again
  return *this;
}


//   CrossProductIterator     CrossProductIterator     CrossProductIterator     CrossProductIterator     CrossProductIterator

bool
CrossProductIterator::valid()             const
{
  update_index_vector();
  return (mSlowIndex < mSlowSource.size()) && (mFastIndices[mSlowIndex] < mFastSource.size());
}

int
CrossProductIterator::number_remaining () const
{
  if (! valid() ) return 0;
  int n (0);
  for (unsigned iSlow=0; iSlow < mSlowSource.size(); ++iSlow)
    n += mFastSource.size() - mFastIndices[iSlow];
  std::cout << "TESTING, Cross product of vectors of sizes " << mSlowSource.size() << " x " << mFastSource.size()
	    << " has " << n << " remaining features." << std::endl;
  return n;
}


Feature
CrossProductIterator::operator*()         const
{
  assert(valid());
  return Feature(mSlowSource[mSlowIndex], mFastSource[mFastIndices[mSlowIndex]]);
}


CrossProductIterator&
CrossProductIterator::operator++()
{
  ++mFastIndices[mSlowIndex];                     
  update_index_vector();                                                                                                                                  
  // find first not used
  mSlowIndex=0;
  while(mSlowIndex < mSlowSource.size())
  { if (mFastIndices[mSlowIndex] < mFastSource.size())                                                                                                                      
      break;
    else
      ++mSlowIndex;
  }
  if (mSlowIndex < mFastIndices.size())
    debugging::debug("CPIT",4) << "Increment position to " << mSlowIndex << " x " << mFastIndices[mSlowIndex] << std::endl;
  else
  { --mSlowIndex;  // keep position in valid range in case fast list expands
    debugging::debug("CPIT",4) << "Increment results in empty iterator.\n";
  }
  return *this;
}


void
CrossProductIterator::update_index_vector()    const
{
  while (mFastIndices.size() < mSlowSource.size())
    mFastIndices.push_back(0);
}


void
CrossProductIterator::print_to (std::ostream& os) const
{ os << "CrossProductIterator with indices ";
  print_indices(os);
  if(valid())
    os << "@ " << mSlowSource[mSlowIndex]->name() << " x " << mFastSource[mFastIndices[mSlowIndex]]->name() << std::endl;
  else
    os << "is empty.\n";
}

void
CrossProductIterator::print_indices(std::ostream& os)  const
{
  os << mSlowIndex << " { ";
  for (unsigned i=0; i<mFastIndices.size(); ++i)
    os << mFastIndices[i] << " ";
  os << "}";
}


    

