// $Id$
#ifndef _PROPERTY_LISTS_H_
#define _PROPERTY_LISTS_H_

/*

  Property lists are keyed collections of arbitrary objects,
  at least arbitrary in that they are decended from the ABC
  of a property.

  properties are copied into the list (via references) so
  make sure that these are light weight.

  30 Jun 08 ... Created.

*/

#include <string>
#include <set>
#include <iostream>   // only debug


// Shell base class that supplies common result for clone

class propertyABC {
 public:
  virtual ~propertyABC() {}
  virtual propertyABC* clone() const = 0;
  virtual void         print_to (std::ostream&) const = 0;
};


// Envelope adds name to abc pointer, handles new(via clone)/delete

class propertyEnvelope {
 private:
  std::string mName;
  propertyABC* mProperty;

 public:
  ~propertyEnvelope() { // std::cout << "PRPE: deleteing property " << mName << std::endl;
                        delete mProperty; }
  
  propertyEnvelope  (std::string const& name, propertyABC const& property) : mName (name), mProperty(property.clone()) {}
  propertyEnvelope  (propertyEnvelope const& pe) : mName(pe.mName), mProperty(pe.mProperty->clone()) {}
  propertyEnvelope& operator= (propertyEnvelope const& pe) { this->mName = pe.name(); this->mProperty=pe.property()->clone(); return *this; }

  std::string const& name()          const { return mName; }
  propertyABC const* property()      const { return mProperty; }
  void rename(std::string newName)         { mName = newName; }

  bool operator< (propertyEnvelope const& p) const { return mName < p.mName; }
  bool operator==(propertyEnvelope const& p) const { return mName == p.mName; }
};

std::ostream&
operator<< (std::ostream &os, propertyEnvelope const& p)
{
  os << "property envelope " << p.name() << "(";
  p.property()->print_to(os);
  os << ")  ";
  return os;
}


// generic class of properties

template<typename T>
class property : public propertyABC {
 private:
  T const mValue;
  
 public:
  ~property() {}

  property (T const& value) : mValue(value) {}
  
  virtual propertyABC*  clone()                    const { return new property<T>(mValue); }  // whoever calls clone resp for delete
  virtual void          print_to(std::ostream& os) const { os << " " << mValue << " "; }
  
 private:
  property(property const& p) : mValue(p.mValue) {}
  property& operator= (property const&);
 };


// convenience collection class

class propertyList {

 private:
  std::set<propertyEnvelope> mSet;
  property<int> mKey;
  
 public:
  typedef std::set<propertyEnvelope>::const_iterator iterator;
  
  ~propertyList () { };
  propertyList (): mSet(), mKey(0) { }

  int  size()                             const { return mSet.size(); }
  iterator begin()                        const { return mSet.begin(); }
  iterator end()                          const { return mSet.end(); }
  bool has_item (std::string const& name) const { propertyEnvelope key(name, mKey); return mSet.find(key) != mSet.end(); }
  void insert(propertyEnvelope pe)       { mSet.insert(pe); }
};

std::ostream&
operator<< (std::ostream &os, propertyList const& pl)
{
  os << "PRPL:  size " << pl.size() << std::endl;
  for (propertyList::iterator it (pl.begin()); it != pl.end(); ++it)
    os << "      " << *it << std::endl;
  return os;
}
  
  


#endif
