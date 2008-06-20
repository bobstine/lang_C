/*
    Variable.c implementation
 */

#include "variable.h"

/*
  this function is here to locate the code for placement by the
  linker.  The virtual is commeted out since the compiler gets unhappy
  about its presence in a cc file.
*/

// virtual
Variable_abc::~Variable_abc() { };


////////////////////  Utilities  //////////////////////////


///////////////////  operator<<  //////////////////////////////

std::ostream&
operator<<(std::ostream &output, Variable const& var)
{
  var.write_to(output);
  return output;
}

///////////////////  compiler
