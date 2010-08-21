#ifndef _FEATUREABC_H_
#define _FEATUREABC_H_

/* 

   Features are named anonymous ranges, with optional information
   about the use of that range in a model or other application. This
   information is held in attributes, which are stored as a map of
   strings with the retrival protocol defined in this base class.

   Features do *not* hold data; they must be 'lightweight'. Any data
   referenced by a descendant of this ABC must be held elsewhere and
   not kept in the feature itself.
   
   9 Nov 09 ... Attributes as a map of two strings.
  17 Apr 09 ... Arguments hold name; (name, power) setup
   9 Apr 04 ... ABC version; read-write; beef up name structure, <, ==
  22 Mar 04 ... Cleaner version 
   9 Mar 04 ... Created to support abstraction of auction.
*/

#include "range_stats.h"
#include "anonymous_iterator.h"

#include <iostream>
#include <map>
#include <set>


class Feature;

class FeatureABC
{
  friend class Feature;
  
 public:
  typedef anonymous_iterator_envelope<std::random_access_iterator_tag,double> Iterator;
  typedef Ranges::range< Iterator >                                           Range;
  typedef std::map<std::string, int>                                          Arguments;   // map sorts names, second is power
  typedef std::map<std::string, std::set<std::string> >                       Attributes;  // allow multiple occurances of the attr 
  typedef std::map<std::string, std::set<std::string> >::const_iterator       AttrIter;
  
 private:
  int            mRefCount;
  int            mSize;           // of underlying range
  Attributes     mAttributes;
  bool           mTried;          // Has this feature been tried in the model?
  bool           mInModel;        // Is this feature a predictor in the model?
  double         mEntryBid;       // Bid on the feature when offered in auction
  
 public:
  virtual ~FeatureABC() { }
  
  FeatureABC (int size)
    : mRefCount(1), mSize(size), mAttributes(), mTried(false), mInModel(false), mEntryBid(0.0) { }

  FeatureABC (std::istream& is)
    : mRefCount(1), mSize(0), mAttributes(), mTried(false), mInModel(false), mEntryBid(0.0) { read_from(is); }

  bool                operator== (FeatureABC const* f)          const { return name() == f->name(); }

  int                 size()                                    const { return mSize; }
  Attributes          attributes()                              const { return mAttributes; }
  bool                has_attribute(std::string attr)           const;
  void                add_attribute(std::string name, std::string value);
  void                add_attributes_from_paired_list (std::string list);

  std::set<std::string> attribute_str_value(std::string attr)   const;
  std::set<     int   > attribute_int_value(std::string attr)   const;
  std::set<   double  > attribute_dbl_value(std::string attr)   const;
  
  bool                was_tried_in_model ()                     const { return mTried; }
  bool                is_used_in_model ()                       const { return mInModel; }
  double              entry_bid ()                              const { return mEntryBid; }
  void                set_model_results(bool used, double bid)        { mTried=true; mInModel=used; mEntryBid=bid; }

  virtual std::string class_name()                              const { return "FeatureABC"; }
  virtual std::string name()                                    const = 0;                 // pure virtual, must maintain const
  virtual Arguments   arguments()                               const = 0;                 //
  
  virtual Iterator    begin ()                                  const = 0;                 //
  virtual Iterator    end ()                                    const = 0;                 //
  virtual Range       range ()                                  const = 0;                 //
  virtual double      average ()                                const = 0;                 // mean value
  virtual double      center ()                                 const = 0;                 // may or may not be average, easier to compute
  virtual double      scale ()                                  const = 0;                 // 0 must mean constant
  virtual bool        is_dummy()                                const;
  virtual bool        is_constant()                             const { return (0.0 == scale()); }
  
  virtual void        write_to       (std::ostream& os)         const;                     // description of the type of feature
          void        write_values_to(std::ostream& os)         const;                     // all of the data values
  virtual void        print_to       (std::ostream& os)         const;                     // just the first few values

protected:
	  Arguments   join_arguments(Arguments const& a1, Arguments const& a2) const;
private:
          void        read_from (std::istream& is);                                  
	  void        initialize_moments();
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
operator<< (std::ostream& os, std::set<std::string> const& s)                 // default for *any* set
{
  for(std::set<std::string>::const_iterator it=s.begin(); it!=s.end(); ++it)
  {
    if(it == s.begin())
      os << *it;
    else
      os << " " << *it;
  }
  return os;
}


inline
std::ostream&
operator<< (std::ostream& os, std::set<int> const& s)                 // default for *any* set
{
  for(std::set<int>::const_iterator it=s.begin(); it!=s.end(); ++it)
    os << " " << *it;
  return os;
}


inline
std::ostream&
operator<< (std::ostream& os, std::set<double> const& s)                 // default for *any* set
{
  for(std::set<double>::const_iterator it=s.begin(); it!=s.end(); ++it)
    os << " " << *it;
  return os;
}


inline
std::ostream&
operator<< (std::ostream& os, std::map<std::string,std::set<std::string> > const& attributes)
{
  os << " { ";
  for (FeatureABC::Attributes::const_iterator it = attributes.begin(); it !=attributes.end(); ++it)
    os << " [" << it->first << " (" << it->second << ")]";
  os << "}";
  return os;
}


inline
std::ostream&
operator<< (std::ostream& os, FeatureABC::Arguments const& args)
{
  for (FeatureABC::Arguments::const_iterator it=args.begin(); it!=args.end(); ++it)
    os << it->first << " " << it->second << " ";
  return os;
}


inline
std::ostream&
operator<<(std::ostream& os, std::vector<FeatureABC*> const& fv)
{
  std::cout << "      ";
  std::copy(fv.begin(), fv.end(), std::ostream_iterator<FeatureABC*>(os, "\n      "));
  return os;
}


#endif
