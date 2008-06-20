/*
 *  gsl_model.cc
 *  seq_regr
 *
 *  Created by Robert Stine on 12/4/07.
 *  Copyright 2007. All rights reserved.
 *
 */

#include "gsl_model.h"

std::ostream&
operator<< (std::ostream& os, LinearModel<gslData,olsEngine> const& m)
{
  m.print_to(os);
  return (os);
}

std::ostream&
operator<< (std::ostream& os, LinearModel<gslData,wlsEngine> const& m)
{
  m.print_to(os);
  return (os);
}

std::ostream&
operator<< (std::ostream& os, LogisticModel<gslData> const& m)
{
  m.print_to(os);
  return (os);
}

