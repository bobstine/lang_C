// $Id$

#include "property_list.h"

#include <iostream>
#include <set>

int  main()
{
  std::cout << "\n\nTEST: Test program is starting... \n\n";
  
  PropertyList properties;

  std::string key;
  std::string str1 ("string 1");
  std::string str2 ("string 2");

  std::cout << "TEST: about to insert a string.\n";
  properties["STR1"] = str1;
  std::cout << "TEST: inserted one string property.\n";
  properties["STR2"] = str2;
  std::cout << "TEST: inserted two string properties.\n";

  properties["DBL0"]= (double) 0.0;
  properties["DBL1"]= (double) 1.0;
  properties["DBL2"]= (double) 2.2;
  properties["INT1"]= (int)      1;
  
  std::cout << "TEST: test property list (which is a map) has " << properties.size() << " elements.\n";
  
  std::cout << properties << std::endl;


  double xx (0.0);
  std::cout << "TEST: extracted " <<
    extract_value_of_property("NONE", xx, properties) << std::endl;

  std::cout << "TEST: extracted " <<
    extract_value_of_property("STR1", xx, properties) << std::endl;

  std::cout << "TEST: extracted " <<
    extract_value_of_property("DBL1", xx, properties) << std::endl;

    {
    PropertyList::iterator it (properties.find("DBL2"));
    if (it != properties.end()) {
      PropertyABC const* p (it->second.property());
      std::cout << "Found ";
      Property<double> const* px = dynamic_cast< Property<double> const* >(p);
      std::cout << " with value " << px->value() << std::endl;
    }
    else
      std::cout << "Property was not found.\n";
  }  

  if (properties.find("another") != properties.end())
    std::cout << "Found\n";
  else
    std::cout << "Property was not found.\n";

  return 0;
}
