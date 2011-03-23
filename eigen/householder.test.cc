#include "householder.h"

const int nRows (8);

void
first(Householder::Vector &x) { x[0] = 100; }

int main()
{
  Householder h(nRows,10);

  Householder::Vector X0 (nRows); X0 << 1,1,1,1,1,1,1,1;
  Householder::Vector X1 (nRows); X1 << 1,2,3,4,5,6,7,8;
  Householder::Vector X2 (nRows); X2 << 1,1,2,2,3,3,4,4;

  // You *can* change vectors passed by reference, as shown here
  //  std::cout << "Original X0 " << X0.transpose() << std::endl;
  //  first(X0);
  //  std::cout << "Post  1  X0 " << X0.transpose() << std::endl;

  // But not like this; subvectors are copies
  //  Householder::Vector X2 (X0.start(3));
  //  X2[0] = 7.7;
  //  std::cout << "Post  2  X0 " << X0.transpose() << std::endl;
  
  // test householder
  h.add_vector(X0);
  std::cout << h << std::endl;
  h.add_vector(X1);
  std::cout << h << std::endl;
  h.add_vector(X2);
  std::cout << h << std::endl;
  h.add_vector(X1);
  std::cout << h << std::endl;
  
  
  return 0;

}
