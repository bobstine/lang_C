// $Id: variable.h,v 1.14 2003/12/03 16:41:02 bob Exp $

/*

 Variables wrap an anonymous range with some statistical covering.

  2 Dec 03 ... Created to work with anonymous iterators.
*/

#ifndef _VARIABLE_H_
#define _VARIABLE_H_

#include "anonymous_iterator.h"

#include <iostream>
#include <vector>
#include <string>

class Variable
{
 public:
  typedef range<anonymous_iterator_envelope<std::random_access_iterator_tag,double> > range;

 private:
  std::string mName;
  range mRange;
  
 public:
  ~Variable() { }
  
  Variable()
    : mName("nil"), mRange(make_anonymous_range(double *0, double *0)) { };
  Variable(std::string const& name, range r)
    : mName(name), mRange(r) { };

  std::string  name()  const { return mName; }
  operator     range() const { return mRange; }

  void write_to (std::ostream& os) const;
};

std::ostream&
operator<<(std::ostream &output, Variable const& var);


#endif
