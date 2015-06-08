// -*- c++ -*-
#ifndef _ATTRIBUTES_H_
#define _ATTRIBUTES_H_

#include <iostream>
#include <map>
#include <string>

/*
  attributes are a comma delimited list of (left)=(right) assignments
*/

class Attributes
{
 private:
  std::map<std::string, std::string> mMap;

 public:

 Attributes(): mMap() { }
 Attributes(std::string const& str) : mMap() { parse_attributes_from_string(str); }

  bool        empty()                           const  { return mMap.empty(); }
  size_t      size()                            const  { return mMap.size(); }
  bool        has_attribute(std::string attr)   const  { return (0 < mMap.count(attr)); }
  std::string operator[](std::string attr)      const  { auto it = mMap.find(attr); if(it != mMap.end()) return it->second; else return ""; }

  void        set_attribute(std::string name, std::string value);
  void        erase_attribute(std::string name)        { auto it = mMap.find(name); if(it !=mMap.end()) mMap.erase(it); }

  std::string as_string()                       const;
  void        print_to (std::ostream& os)       const;

 private:
  
  void parse_attributes_from_string(std::string const& str);

};

inline
std::ostream&
operator<< (std::ostream& os, Attributes const& attributes)
{
  attributes.print_to(os);
  return(os);
}

#endif
