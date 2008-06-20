// $Id: dataset.test.cc,v 1.1 2001/12/18 13:21:34 bob Exp $

#include <iostream>
#include <string>

#include "dataset.h"

int main (void)
{
  Dataset data("test/dataset");

  cout << data;

  return 0;
}
    
