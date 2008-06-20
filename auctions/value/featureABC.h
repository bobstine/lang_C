// $Id: featureABC.h,v 3.0 2004/11/19 18:58:36 foster Exp $

#ifndef _FEATUREABC_H_
#define _FEATUREABC_H_

/* 
  Features are basically named ranges, with optional information about
  the use of that data in a model or other application. Features do
  not hold any data, so they are 'lightweight'.  That said, someone
  else must preserve the data pointed to by the feature.

  Feature source is a collection of features that must implement the
  function 'features' which returns an object of type 'Vector' that
  supports indexed access.  The type 'Vector' is defined in the
  feature source class.

   9 Apr 04 ... Separate ABC version from rest; beef up name structure, <, ==
   5 Apr 04 ... Read/Write to support virtual constructor
   1 Apr 04 ... ABC version
  22 Mar 04 ... Cleaner version 
   9 Mar 04 ... Created to support abstraction of auction.
*/

#include "range_ops.h"
#include "anonymous_iterator.h"

#include <iostream>
#include <map>
///

class FeatureABC
{
 public:
  typedef range<anonymous_iterator_envelope<std::random_access_iterator_tag,double> > Range;
  typedef std::map<FeatureABC const*, int>                                            Arguments;
  
 public:
  virtual ~FeatureABC() { }

  FeatureABC () { }

  bool                operator== (FeatureABC const* f)   const { return name() == f->name(); }
  bool                operator<  (FeatureABC const* f)   const;

  virtual FeatureABC* clone()                            const = 0;                 // pure virtual
  virtual std::string name()                             const = 0;                 //
  virtual std::string operator_name()                    const = 0;                 //
  virtual Arguments   arguments()                        const = 0;                 //
  
  virtual Range       range ()                           const = 0;                 //
  virtual double      center ()                          const = 0;                 //
  virtual double      scale ()                           const = 0;                 //
  
  virtual void        write_to (std::ostream& os)        const = 0;                 //
	  
  virtual double      average()                          const;                     // virtual
  virtual double      std_dev()                          const;                     // 
  virtual bool        is_dummy()                         const;                     //

 protected:
  Arguments           join_arguments(Arguments const& a1, Arguments const& a2) const;
};


class EnvelopeABC
{
 public:
  virtual FeatureABC* clone_feature_ptr() const  = 0;
};


inline
std::ostream&
operator<< (std::ostream& os, FeatureABC::Arguments const& args)
{
  for (FeatureABC::Arguments::const_iterator it=args.begin(); it!=args.end(); ++it)
    os << it->first->name() << " " << it->second << " ";
  return os;
}

#endif

