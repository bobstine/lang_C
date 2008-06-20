// $Id: tags.test.cc,v 1.1 2003/08/25 22:09:19 bob Exp $

#include "tags.h"

#include <vector>
#include "print_utils.h"

enum PredictorType { ANY, COLUMN, INTERACTION, MODEL_BASED };

int
main()
{
  
  std::cout << make_tag(ANY, std::vector<double>(2));

}

  

