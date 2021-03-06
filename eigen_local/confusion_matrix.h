#ifndef _confusion_matrix_h_
#define  _confusion_matrix_h_

#include "eigen_base_types.h"

#include <iostream>
#include <Eigen/Core>

class ConfusionMatrix
{
  typedef SCALAR Scalar;
  typedef Eigen::Matrix3i CMatrix;  // margins hold sums 

  Scalar  mThreshold;
  CMatrix mMatrix;

 public:
  template<class Iter1, class Iter2>
    ConfusionMatrix (size_t n, Iter1 yBegin, Iter2 yHatBegin, Scalar threshold=0.5): mThreshold(threshold), mMatrix() { init(n,yBegin,yHatBegin); }

  template<class Collection1, class Collection2>
    ConfusionMatrix (Collection1 const& y, Collection2 const& yHat, Scalar threshold=0.5): mThreshold(threshold), mMatrix() { init(y.size(), y.begin(), yHat.begin()); }

  Scalar sensitivity() const;
  Scalar specificity() const;
  Scalar precision()   const;
  Scalar f1()          const;
  Scalar pct_correct() const;
  
  void print_to (std::ostream& os) const;
  
 private:
  template<class Iter1, class Iter2>
    void
    init(size_t n, Iter1 y, Iter2 yHat)
    {
      mMatrix = CMatrix::Zero();
      mMatrix(2,2) = (int)n;
      for(size_t i=0; i<n; ++i)
      { ++mMatrix(int(*y),2);        // ++ row margin
	if(*yHat < mThreshold)
	{ ++mMatrix(int(*y),0);
	  ++mMatrix(   2   ,0);      // ++ col margin
	}
	else
	{ ++mMatrix(int(*y),1);
	  ++mMatrix( 2     ,1);
	}
	++y; ++yHat;
      }
    }
};

inline
std::ostream& operator<<(std::ostream& os, ConfusionMatrix const& cm)
{
  cm.print_to(os);
  return(os);
}
#endif
