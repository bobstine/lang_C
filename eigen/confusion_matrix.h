#ifndef _confusion_matrix_h_
#define  _confusion_matrix_h_

#include <iostream>
#include <Eigen/Core>

class ConfusionMatrix
{
  typedef Eigen::Matrix3i CMatrix;  // margins hold sums 

  float   mThreshold;
  CMatrix mMatrix;

 public:
  template<class Iter1, class Iter2>
    ConfusionMatrix (size_t n, Iter1 yBegin, Iter2 yHatBegin, float threshold=0.5): mThreshold(threshold), mMatrix() { init(n,yBegin,yHatBegin); }

  template<class Collection1, class Collection2>
    ConfusionMatrix (Collection1 const& y, Collection2 const& yHat, float threshold=0.5): mThreshold(threshold), mMatrix() { init(y.size(), y.begin(), yHat.begin()); }

  float sensitivity() const;
  float specificity() const;
  float precision()   const;
  float f1()          const;
  float pct_correct() const;
  
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
