// $Id$

#include "property_list.h"

#include <iostream>
#include <set>

int  main()
{
  std::cout << "\n\nTest program is starting... \n\n";
  
  property<double>      propDouble0(6.0);
  property<double>      propDouble1(6.1);
  property<double>      propDouble2(6.2);
  property<int>         propInteger(1);
  property<std::string> propString("String 1");
  
  propertyList properties;
  
  properties.insert(propertyEnvelope("str 1"   , propString));
  std::cout << "TEST: inserted first string property.\n";

  properties.insert(propertyEnvelope("double 0", propDouble0));
  properties.insert(propertyEnvelope("double 1", propDouble1));
  properties.insert(propertyEnvelope("double 2", propDouble2));
  properties.insert(propertyEnvelope("int",      propInteger));
  
  std::cout << "TEST: test property list (which is a set) has " << properties.size() << " elements.\n";
  
  std::cout << properties << std::endl;

  if (properties.has_item("double 2"))
    std::cout << "Found\n";
  else
    std::cout << "Property was not found.\n";

  if (properties.has_item("another"))
    std::cout << "Found\n";
  else
    std::cout << "Property was not found.\n";

  return 0;
}
