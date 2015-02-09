#include "confusion_matrix.h"
#include <iomanip>

float
ConfusionMatrix::sensitivity() const
{
  return (float)mMatrix(1,1)/(float)mMatrix(1,2);
}

float
ConfusionMatrix::specificity() const
{
  return (float)mMatrix(0,0)/(float)mMatrix(0,2);
}


float
ConfusionMatrix::precision() const
{
  return (float)mMatrix(1,1)/(float)(mMatrix(2,1));    // TP / (TP + FP)
}

float
ConfusionMatrix::f1() const
{
  size_t tp2 = 2 * mMatrix(1,1);
  
  return (float)(tp2)/(float)(tp2 + mMatrix(0,1) + mMatrix(1,0)); // F1 = 2 TP/ (2 TP + FP + FN)
}

void
ConfusionMatrix::print_to(std::ostream &os) const
{
  os
    << "[0] " << std::setw(5) << mMatrix(0,0) << "  " << std::setw(5) << mMatrix(0,1) << " | " << std::setw(5) << mMatrix(0,2)
    << "    spec=" << std::setprecision(3) << specificity() << "   f1=" << std::setprecision(3) << f1()
    << std::endl
    << "[1] " << std::setw(5) << mMatrix(1,0) << "  " << std::setw(5) << mMatrix(1,1) << " | " << std::setw(5) << mMatrix(1,2)
    << "    sens=" << std::setprecision(3) << sensitivity() << " prec=" << std::setprecision(3) << precision()
    << std::endl
    << "    " << std::setw(5) << mMatrix(2,0) << "  " << std::setw(5) << mMatrix(2,1) << " | " << std::setw(5) << mMatrix(2,2)
    << std::endl;
}
