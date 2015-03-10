#include "confusion_matrix.h"
#include <iomanip>

ConfusionMatrix::Scalar
ConfusionMatrix::sensitivity() const
{
  return (Scalar)mMatrix(1,1)/(Scalar)mMatrix(1,2);
}

ConfusionMatrix::Scalar
ConfusionMatrix::specificity() const
{
  return (Scalar)mMatrix(0,0)/(Scalar)mMatrix(0,2);
}


ConfusionMatrix::Scalar
ConfusionMatrix::precision() const
{
  return (Scalar)mMatrix(1,1)/(Scalar)(mMatrix(2,1));    // TP / (TP + FP)
}

ConfusionMatrix::Scalar
ConfusionMatrix::f1() const
{
  size_t tp2 = 2 * mMatrix(1,1);
  
  return (Scalar)(tp2)/(Scalar)(tp2 + mMatrix(0,1) + mMatrix(1,0)); // F1 = 2 TP/ (2 TP + FP + FN)
}

ConfusionMatrix::Scalar
ConfusionMatrix::pct_correct() const
{
  return (Scalar)(mMatrix(0,0)+mMatrix(1,1))/(Scalar)(mMatrix(2,2));
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
    << "    % correct = " << std::setprecision(3) << pct_correct()
    << std::endl;
}
