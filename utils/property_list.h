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
#include <utility>
#include <set>
#include <iostream>   // only debug


// Shell base class that supplies common result for clone

class PropertyABC {
  
public: 
  virtual ~PropertyABC() {}
  
  virtual PropertyABC* clone() const = 0;
  
  bool operator<(PropertyABC const& p)    const {
    if (typeid(*this).before(typeid(p))) return true;
    if (typeid(p).before(typeid(*this))) return false;
    return before(p); }
  
  bool operator==(PropertyABC const& p)    const {
    if (typeid(this).before(typeid(p))) return false;
    if (typeid(p).before(typeid(this))) return false;
    return equal(p); }
  
  virtual bool type_matches(std::type_info const& type) const = 0;
  
  virtual bool before(PropertyABC const& p) const = 0;
  virtual bool equal (PropertyABC const& p) const = 0;
  
  virtual void print_to (std::ostream&) const = 0;
};


// Generic properties, each holding an object of type <T>
/*
  Methods before and equal compare properties, *assuming* that the
  contents are of equal or conformable types.  You get an error
  message if they are not. Called by ABC class.
*/

template<typename T>
class Property : public PropertyABC {
  
private:
  T const mValue;
  
public:
  typedef T ValueType;
  
  ~Property() {}
  
  Property (T const& value)    : PropertyABC(), mValue(value) {}
  Property (Property const& p) : PropertyABC(), mValue(p.mValue) {}
  
  virtual PropertyABC*  clone()                   const { return new Property<T>(mValue); }  // whoever *calls* clone resp for delete
  
  virtual void print_to(std::ostream& os)         const { os << "Type: {" << typeid(mValue).name() << "} = " << mValue; }
  T            value()                            const { return mValue; }
  
private:
  
  Property& operator= (Property const&);
  
  virtual bool type_matches (std::type_info const& type) const {
    return typeid(mValue) == type;
  }
  
  virtual bool before(PropertyABC const& p)       const {
    // throws an exception if fails
    Property<T> const& px = dynamic_cast< Property<T> const& > (p);
    return mValue < px.mValue;
  }
  
  virtual bool equal(PropertyABC const& p)       const {
    // throwa an exception if fails
    Property<T> const& px = dynamic_cast< Property<T> const& > (p);
    return mValue == px.mValue;
  }
  
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
  PropertyABC const* mpProperty;
  
public:
  ~PropertyEnvelope() { if (mpProperty) delete mpProperty; }
  PropertyEnvelope() : mpProperty(0) {}
  
  PropertyEnvelope  (PropertyEnvelope const& pe)  : mpProperty(pe.clone_property()) {}
  PropertyEnvelope& operator= (PropertyEnvelope const& pe) { mpProperty=pe.clone_property(); return *this; }
  
  template <typename T>
  PropertyEnvelope (T const& value): mpProperty(new Property<T>(value)) {}
  
  PropertyABC const& property()               const { return *mpProperty; }
  
  bool operator< (PropertyEnvelope const& pe) const { return (*mpProperty)  < *(pe.mpProperty); }
  bool operator==(PropertyEnvelope const& pe) const { return (*mpProperty) == *(pe.mpProperty); }
  
private:
  PropertyABC* clone_property()               const { return (mpProperty) ? mpProperty->clone() :  0; }
};

std::ostream&
operator<< (std::ostream &os, PropertyEnvelope const& p)
{
  os << "PropertyEnvelope [";  p.property().print_to(os);  os << "]  ";
  return os;
}

// Predicate for weak matching of envelopes

class PropertyEnvelopeContentTypeMatches : public std::binary_function<PropertyEnvelope, std::type_info, bool>
{
public:
  bool operator()(PropertyEnvelope const& pe, std::type_info const& type) {
    // std::cout << "PRPE: Type " << type.name() << " compared to " << pe << std::endl;
    return ((pe.property()).type_matches(type));
  }
};

std::pair<std::set<PropertyEnvelope>::const_iterator, std::set<PropertyEnvelope>::const_iterator>
find_properties_of_type (std::set<PropertyEnvelope> const& s, std::type_info const& type)
{
  std::set<PropertyEnvelope>::const_iterator fst = find_if(s.begin(), s.end(),
							   bind2nd(PropertyEnvelopeContentTypeMatches(), type));
  if (fst == s.end()) return std::make_pair(s.end(), s.end());
  std::set<PropertyEnvelope>::const_iterator lst = find_if(fst, s.end,
							   not1(bind2nd(PropertyEnvelopeContentTypeMatches(), type)));
  return std::make_pair(fst, lst);
}
		
		
#endif
