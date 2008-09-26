// $Id$

#include "property_list.h"

#include <iostream>
#include <set>

int  main()
{
  std::cout << "\n\nTEST: Test program is starting... \n\n\n";

  // first test direct envelope comparison
  PropertyEnvelope p1  ((double) 1.0);
  PropertyEnvelope p2  ((double) 2.0);
  PropertyEnvelope ps  ((std::string) "abc");
  PropertyEnvelope pS  ((std::string) "ABC");
  
  if (p1 < p2)
    std::cout << "TEST: p1 < p2\n";
  else 
    std::cout << "TEST: p1 >= p2\n";

  if (p2 < p1)
    std::cout << "TEST: p2 < p1\n";
  else 
    std::cout << "TEST: p2 >= p1\n";

  if (ps < pS)
    std::cout << "TEST: ps < pS\n";
  else
    std::cout << "TEST: ps >= pS\n";

  if (ps < p1)
    std::cout << "TEST: ps < p1\n";
  else
    std::cout << "TEST: ps >= p1\n";
  std::cout << std::endl;
  
  // insert into a set and print
  std::set<PropertyEnvelope> properties;
  properties.insert( PropertyEnvelope( (std::string) "bb") );
  properties.insert( PropertyEnvelope( (std::string) "cc") );
  properties.insert( PropertyEnvelope( (std::string) "aa") );
  properties.insert( PropertyEnvelope( (double) 7.0) );
  properties.insert( PropertyEnvelope( (double) 6.0) );
  properties.insert( PropertyEnvelope( (double) 9.0) );
  properties.insert( PropertyEnvelope( (double) 10.0) );
  properties.insert( PropertyEnvelope( (double) 8.0) );
 
  for(std::set<PropertyEnvelope>::iterator it=properties.begin(); it != properties.end(); ++it)
    std::cout << *it << std::endl;
  std::cout << std::endl;

  // find items of indicated type and value
  std::set<PropertyEnvelope>::iterator match (properties.find(p1));  // p1 declared way above
  if (match != properties.end())
    std::cout << "TEST: Property p1 match found.\n";
  else
    std::cout << "TEST: Property p1 match not found.\n";

  PropertyEnvelope p7  ((double) 7.0);
  match = properties.find(p7);
  if (match != properties.end())
    std::cout << "TEST: Property p7 match found.\n";
  else
    std::cout << "TEST: Property p7 match not found.\n";

  // match on class only
  if (PropertyEnvelopeContentTypeMatches()(p7, typeid((double) 2.2)))
    std::cout << "TEST: type matches.\n";
  else
    std::cout << "TEST: type match fails (error).\n";

  
  std::cout << "\n\nTEST: Test program complete.\n";
  return 0;

  /*  This part tests tagged properties
      
  TaggedPropertyList properties;

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
    TaggedPropertyList::iterator it (properties.find("DBL2"));
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
  */
  

}
