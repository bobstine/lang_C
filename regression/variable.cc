/*
    Variable.c implementation
 */

#include "variable.h"
#include "range_ops.h"

void
Variable::write_to (std::ostream& os) const
{
  os << "Variable " << mName
     << ":  " << mRange << std::endl;
}


///////////////////  operator<<  //////////////////////////////

std::ostream&
operator<<(std::ostream &output, Variable const& var)
{
  var.write_to(output);
  return output;
}


