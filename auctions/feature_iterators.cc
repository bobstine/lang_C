#include "feature_iterators.h"
#include "string_trim.h"
#include "debug.h"

#include <sstream>
#include <fstream>

using debugging::debug;

std::string tag = "FITR: ";

//     EigenwordIterator     EigenwordIterator     EigenwordIterator     EigenwordIterator     EigenwordIterator

/*
EigenwordIterator&
EigenwordIterator::operator++()
{
  while (true)
  { if (mPosition < mEigenDim-1)
      ++mPosition;
    else
      mPosition = 0;
    if (!mUsedDim[mPosition])
      break;
  }
  return *this;
}

Feature
EigenwordIterator::operator*()        const
{
  typedef std::vector<std::string>::const_iterator Iter;
  Column<Scalar> column(mName + "_" + std::to_string(mPosition),
			"role x stream " + mName,
			mTokens.size(), mTokens.begin(), [this](Iter const& it)->Scalar { return mDictionary.at(*it)[mPosition]; } );
  return Feature(column);
}



void
EigenwordIterator::print_to(std::ostream& os)      const
{
  os << "EigenwordIterator @ ";
  if (points_to_valid_data())
    os << mPosition << " with " << mNumberRemaining << " remaining.";
  else
    os << " empty ";
}
*/

//  LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator     LagIterator

LagIterator&
LagIterator::operator++()
{
  ++mLag; --mRemaining;
  if ((mLag > mMaxLag) && (mRemaining>0))
    mLag = 1;  // go around again
  return *this;
}


    

