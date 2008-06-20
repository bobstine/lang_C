// $Id: dataset.test.cc,v 1.1 2005/06/13 20:47:51 bob Exp $

#include <iostream>
#include <string>

#include "dataset.h"

int main (void)
{
  Dataset data("test/dataset");

  cout << data;

  return 0;
}
    
