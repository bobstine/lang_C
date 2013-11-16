#ifndef _EIGEN_ITERATOR_H_
#define _EIGEN_ITERATOR_H_

#include <Eigen/Core>
#include <iterator>

//      eigen_iterator     eigen_iterator     eigen_iterator     eigen_iterator     eigen_iterator

class EigenVectorIterator: public std::iterator<std::forward_iterator_tag, double>
{
  
  int mIndex;
  Eigen::VectorXd const* mpVector;
  
public:
  EigenVectorIterator (Eigen::VectorXd const* v)
    : mIndex(0), mpVector(v)                  { }

  EigenVectorIterator (EigenVectorIterator const& vi)
    : mIndex(vi.mIndex), mpVector(vi.mpVector) {}

  EigenVectorIterator & operator= (EigenVectorIterator const& other)
    {
      if (this != &other) // protect against invalid self-assignment
      { mIndex = other.mIndex;
	mpVector = other.mpVector;
      }
      return *this;
    }
  
  EigenVectorIterator& operator++()          { ++mIndex; return *this; }
  double               operator*()    const  { return (*mpVector)[mIndex]; }

  bool operator==(EigenVectorIterator const& it) const { return ((mIndex == it.mIndex) && (mpVector == it.mpVector)); }
  bool operator!=(EigenVectorIterator const& it) const { return ((mIndex != it.mIndex) || (mpVector != it.mpVector)); }

  bool operator<(EigenVectorIterator const& it) const { return mIndex < it.mIndex; }
  bool operator>(EigenVectorIterator const& it) const { return mIndex > it.mIndex; }
};


class EigenColumnIterator: public std::iterator<std::forward_iterator_tag, double>
{
  int mColumn;
  int mIndex;
  Eigen::MatrixXd const* mpMatrix;
  
public:
  EigenColumnIterator (Eigen::MatrixXd const* m, int column)
    : mColumn(column), mIndex(0), mpMatrix(m)                  { }

  EigenColumnIterator (EigenColumnIterator const& mi)
    : mColumn(mi.mColumn), mIndex(mi.mIndex), mpMatrix(mi.mpMatrix) {}

  EigenColumnIterator & operator= (EigenColumnIterator const& other)
    {
      if (this != &other) // protect against invalid self-assignment
      { mColumn  = other.mColumn;
	mIndex   = other.mIndex;
	mpMatrix = other.mpMatrix;
      }
      return *this;
    }
  
  EigenColumnIterator& operator++()          { ++mIndex; return *this; }
  double               operator*()    const  { return (*mpMatrix)(mIndex,mColumn); }

  bool operator==(EigenColumnIterator const& it) const { return ((mIndex == it.mIndex) && (mpMatrix == it.mpMatrix)); }
  bool operator!=(EigenColumnIterator const& it) const { return ((mIndex != it.mIndex) || (mpMatrix != it.mpMatrix)); }

  bool operator<(EigenColumnIterator const& it) const { return mIndex < it.mIndex; }
  bool operator>(EigenColumnIterator const& it) const { return mIndex > it.mIndex; }
};

#endif
