// -*-c++-*- 
#ifndef _PROPERTY_H_
#define _PROPERTY_H_

/*

  Properties are arbitrary objects,
  albeit objects that are decended from the ABC.

  Properties are copied into the list (via references) so
  make sure that these are light weight.

  30 Jun 08 ... Created.

*/

#include <string>
#include <typeinfo>
#include <iostream>   // only debug

class PropertyEnvelope;

// Shell base class that supplies common result for clone

class PropertyABC {
  
 public:                                                 // protected constructors for envelope only
  virtual ~PropertyABC() {}
  virtual PropertyABC* clone() const = 0;

  bool operator<(PropertyABC const* p)    const {
    std::cout << "Checking for less than\n";
    if (typeid(this).before(typeid(p))) return true;
    if (typeid(p).before(typeid(this))) return false;
    return before(p); }
  
  bool operator==(PropertyABC const* p)    const {
    std::cout << "Checking for equality\n";
    if (typeid(this).before(typeid(p))) return false;
    if (typeid(p).before(typeid(this))) return false;
    return equal(p); }

  virtual bool before(PropertyABC const* p) const = 0;
  virtual bool equal (PropertyABC const* p) const = 0;
  
  virtual void print_to (std::ostream&) const = 0;
};


// Generic class of properties

template<typename T>
class Property : public PropertyABC {
  
private:
  T const mValue;
  
public:
  ~Property() {}
  
  Property (T const& value)    : PropertyABC(), mValue(value) {}
  Property (Property const& p) : PropertyABC(), mValue(p.mValue) {}
  
  virtual PropertyABC*  clone()                   const { return new Property<T>(mValue); }  // whoever *calls* clone resp for delete

  virtual void print_to(std::ostream& os)         const { os << " Type: {" << typeid(mValue).name() << "} = " << mValue << " "; }
  T            value()                            const { return mValue; }
  
 private:

  Property& operator= (Property const&);
  
  virtual bool before(PropertyABC const* p)       const {
    Property<T> const* px = dynamic_cast< Property<T> const* > (p);
    if(px)
      return mValue < px->mValue;
    else {
      std::cout << "ERROR: bad cast in before method of Property\n";
      return false;
    }}
      
  virtual bool equal(PropertyABC const* p)       const {
    Property<T> const* px = dynamic_cast< Property<T> const* > (p);
    if(px)
      return mValue == px->mValue;
    else {
      std::cout << "ERROR: bad cast in equal method of Property\n";
      return false;
    }}

};

template<typename T>
std::ostream&
operator<< (std::ostream &os, Property<T> const& p)
{
  p.print_to(os);
  return os;
}

// Envelope for abc pointer, handles new(via clone)/delete

class PropertyEnvelope {
 private:
  PropertyABC* mpProperty;

 public:
  ~PropertyEnvelope() { // std::cout << "PRPE: deleteing property " << mName << std::endl;
                        if (mpProperty) delete mpProperty; }
  PropertyEnvelope() : mpProperty(0) {}

  PropertyEnvelope  (PropertyEnvelope const& pe)  : mpProperty(pe.clone_property()) {}
  PropertyEnvelope& operator= (PropertyEnvelope const& pe) { this->mpProperty=pe.clone_property(); return *this; }

  template <typename T>
    PropertyEnvelope (T const& value): mpProperty(new Property<T>(value)) {}

  
  PropertyABC const* property()               const { return mpProperty; }

  bool operator< (PropertyEnvelope const& pe) const { return mpProperty <  pe.mpProperty; }
  bool operator==(PropertyEnvelope const& pe) const { return mpProperty == pe.mpProperty; }
  
 private:
  PropertyABC* clone_property()              const { return (mpProperty) ? mpProperty->clone() :  0; }
};

std::ostream&
operator<< (std::ostream &os, PropertyEnvelope const& p)
{
  os << "property envelope (";  p.property()->print_to(os);  os << ")  ";
  return os;
}


/*

  
// Illustrative convenience collection class

class TaggedPropertyList: public std::map<const std::string, PropertyEnvelope> {};

std::ostream&
operator<< (std::ostream &os, TaggedPropertyList const& pl)
{
  os << "TPLS:  size " << pl.size() << std::endl;
  for (TaggedPropertyList::const_iterator it (pl.begin()); it != pl.end(); ++it)
    os << "      " << it->first << std::endl;
  return os;
}

// Convenience accessor

template <typename T>
T
extract_value_of_property(std::string const& name, T const&, TaggedPropertyList const& properties)
{
  TaggedPropertyList::const_iterator it (properties.find(name));
  if (it == properties.end()) {
    std::cout << "TPLS: Warning.  Property " << name << " was not found.\n";
    return T(0);
  }
  PropertyABC const* p  (it->second.property());
  Property<T> const* px (0);
  //  std::cout << "PRPL: Type of src is " << typeid(*p).name();
  try
    {
      px = dynamic_cast< Property<T> const* >(p);
    }
  // never called since above returns 0 for any thing it does not like
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

*/


#endif
