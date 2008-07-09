// $Id$
#ifndef _PROPERTY_LISTS_H_
#define _PROPERTY_LISTS_H_

/*

  Property lists are keyed collections of arbitrary objects,
  at least arbitrary in that they are decended from the ABC
  of a property.

  Properties are copied into the list (via references) so
  make sure that these are light weight.

  30 Jun 08 ... Created.

*/

#include <string>
#include <map>
#include <iostream>   // only debug

class PropertyEnvelope;

// Shell base class that supplies common result for clone

class PropertyABC {
  // Friend class propertyEnvelope;
  
 public:                                                 // protected constructors for envelope only
  virtual ~PropertyABC() {}
  virtual PropertyABC* clone() const = 0;
 public:
  virtual void         print_to (std::ostream&) const = 0;
};


// Generic class of properties

template<typename T>
class Property : public PropertyABC {
  //friend class propertyEnvelope;
 private:
  T const mValue;
  
 public:
  ~Property() {}

  Property (T const& value)    : PropertyABC(), mValue(value) {}
  Property (Property const& p) : PropertyABC(), mValue(p.mValue) {}
  
  virtual PropertyABC*  clone()           const { return new Property<T>(mValue); }  // whoever *calls* clone resp for delete

 public:
  virtual void print_to(std::ostream& os) const { os << " " << mValue << " "; }
  T            value()                    const { return mValue; }         

 private:
  Property& operator= (Property const&);
 };



// Envelope for abc pointer, handles new(via clone)/delete

class PropertyEnvelope {
 private:
  PropertyABC* mpProperty;

 public:
  ~PropertyEnvelope() { // std::cout << "PRPE: deleteing property " << mName << std::endl;
                        if (mpProperty) delete mpProperty; }
  PropertyEnvelope() : mpProperty(0) {}

  // from other properties
  PropertyEnvelope  (PropertyEnvelope const& pe)  : mpProperty(pe.clone_property()) {}
  PropertyEnvelope& operator= (PropertyEnvelope const& pe) { this->mpProperty=pe.clone_property(); return *this; }

  // from data
  template <typename T>
    PropertyEnvelope (T const& value): mpProperty(new Property<T>(value)) {}

  PropertyABC const* property()      const { return mpProperty; }

 private:
  PropertyABC* clone_property()      const { return (mpProperty) ? mpProperty->clone() :  0; }
};


std::ostream&
operator<< (std::ostream &os, PropertyEnvelope const& p)
{
  os << "property envelope (";  p.property()->print_to(os);  os << ")  ";
  return os;
}


// Convenience collection class

class PropertyList: public std::map<const std::string, PropertyEnvelope> {};

std::ostream&
operator<< (std::ostream &os, PropertyList const& pl)
{
  os << "PRPL:  size " << pl.size() << std::endl;
  for (PropertyList::const_iterator it (pl.begin()); it != pl.end(); ++it)
    os << "      " << it->first << std::endl;
  return os;
}

// Convenience accessor

template <typename T>
T
extract_value_of_property(std::string const& name, T const&, PropertyList const& properties)
{
  PropertyList::const_iterator it (properties.find(name));
  if (it == properties.end()) {
    std::cout << "PRPL: Warning.  Property " << name << " was not found.\n";
    return T(0);
  }
  PropertyABC const* p  (it->second.property());
  Property<T> const* px (0);
  //  std::cout << "PRPL: Type of src is " << typeid(*p).name();
  try
    {
      px = dynamic_cast< Property<T> const* >(p);
    }
  catch (std::bad_cast)
    { std::cout << "BAD_CAST: Error attempting cast; return 0.\n";
    }
  catch (...)
    { std::cout << "ERROR: unknown exception encountered in extracting value from property list.\n";
    }
  if(px)
    return px->value();
  else {
    std::cout << "PRPL: Cast in extraction from property list failed; return 0.\n";
    return T(0);
  }
}

  


#endif
