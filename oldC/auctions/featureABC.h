// $Id: featureABC.h,v 1.12 2004/08/19 02:49:37 bob Exp $

#ifndef _FEATUREABC_H_
#define _FEATUREABC_H_

/* 
  Features are basically named ranges, with optional information about
  the use of that data in a model or other application. Features do
  not hold any data, so they are 'lightweight'.  That said, someone
  else must preserve the data pointed to by the feature.

   9 Apr 04 ... Separate ABC version from rest; beef up name structure, <, ==
   5 Apr 04 ... Read/Write to support virtual constructor
   1 Apr 04 ... ABC version
  22 Mar 04 ... Cleaner version 
   9 Mar 04 ... Created to support abstraction of auction.
*/

#include "range_stats.h"
#include "anonymous_iterator.h"

#include <iostream>
#include <map>
#include <set>


///

class FeatureABC
{
 public:
  typedef range<anonymous_iterator_envelope<std::random_access_iterator_tag,double> > Range;
  typedef std::map<FeatureABC const*, int>                                            Arguments;
  typedef std::set<std::string>                                                       Attributes;

 private:
  int            mSize;         // Of underlying range
  Attributes     mAttributes;
  bool           mTried;        // Has this feature been tried in the model?
  bool           mInModel;      // Is this feature a predictor in the model?
  double         mEntryPValue;  // What p-value did the feature get?
  
 public:
  virtual ~FeatureABC() { }
  
  FeatureABC (int size)
    : mSize(size), mAttributes(), mTried(false), mInModel(false), mEntryPValue(0.0) { }

  FeatureABC (std::istream& is)
    : mSize(0), mAttributes(), mTried(false), mInModel(false), mEntryPValue(0.0) { read_from(is); }

  bool                operator== (FeatureABC const* f)          const { return name() == f->name(); }
  bool                operator<  (FeatureABC const* f)          const;

  int                 size()                                    const { return mSize; }
  virtual std::string class_name()                              const { return "FeatureABC"; }
  Attributes          attributes()                              const { return mAttributes; }
  bool                has_attribute(std::string const& a)       const;
  void                add_attribute(std::string const& s)             { mAttributes.insert(s); }
  
  bool                was_tried_in_model ()                     const { return mTried; }
  bool                is_used_in_model ()                       const { return mInModel; }
  double              entry_p_value ()                          const { return mEntryPValue; }
  void                set_model_results(bool used, double pval)       { std::cout << "FETR: " << name() << " setting results.\n";
                                                                        mTried=true; mInModel=used; mEntryPValue=pval; }

  virtual std::string name()                                    const = 0;                 // pure virtual
  virtual Arguments   arguments()                               const = 0;                 //
  
  virtual Range       range ()                                  const = 0;                 //
  virtual double      center ()                                 const = 0;                 //
  virtual double      scale ()                                  const = 0;                 //

  virtual double      average()                                 const  { return range_stats::average(range(), mSize); }
  virtual double      std_dev()                                 const  { return range_stats::standard_deviation(range(), average(), mSize); }
  virtual bool        is_dummy()                                const;                     //

  virtual void        write_to (std::ostream& os)               const;                     //  
  virtual void        print_to (std::ostream& os)               const;                     //
          void        read_from (std::istream& is);                                  

 protected:
  Arguments           join_arguments(Arguments const& a1, Arguments const& a2) const;
};

inline
std::ostream&
operator<< (std::ostream& os, FeatureABC const* feature)
{
  feature->print_to(os);
  return os;
}

inline
std::ostream&
operator<< (std::ostream& os, std::set<std::string> const& attributes)
{
  os << " { " ;
  std::copy (attributes.begin(), attributes.end(), std::ostream_iterator<std::string>(os," "));
  os << "}";
  return os;
}

inline
std::ostream&
operator<< (std::ostream& os, FeatureABC::Arguments const& args)
{
  for (FeatureABC::Arguments::const_iterator it=args.begin(); it!=args.end(); ++it)
    os << it->first->name() << " " << it->second << " ";
  return os;
}

#endif

