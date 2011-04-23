#include "feature_predicates.h"


bool
FeaturePredicates::mutually_exclusive_indicators_from_same_parent(Feature const& f1, Feature const& f2) 
{
  // false unless both have parents
  std::set<std::string> p1 = f1->attribute_str_value("parent");
  if (p1.empty()) return false;
  std::set<std::string> p2 = f2->attribute_str_value("parent");
  if (p2.empty()) return false;
  // false unless parents match
  if (*p1.begin() != *p2.begin()) return false;
  // false unless both have category attribute
  std::set<std::string> c1 = f1->attribute_str_value("category");
  std::set<std::string> c2 = f2->attribute_str_value("category");
  if (c1.empty() || c2.empty()) return false;
  // true if categories differ
  return *c1.begin() != *c2.begin();
}


bool
SkipIfDerived::operator()(Feature const& f) const
{
  std::string fname (f->name());
  bool b1 (    (f->is_constant())       || (f->is_dummy()));
  bool b2 (    (f->degree() > 2)  );
  bool b3 (     (fname.size() >= 4 && "cube" == fname.substr(0,4))      ||     
		(fname.size() >= 6 && "square" == fname.substr(0,6))   );
  bool b4 (    (f->has_attribute("neighborhood"))                      ||    
	       (std::string::npos != fname.find("Y_hat_"))  );
  debugging::debug("FPRD",4) << "Checking whether " << fname << " is derived;  [" << b1 << b2 << b3 << b4 << "]\n";
  return
    (f->is_constant())       || (f->is_dummy())             ||
    (f->degree() > 2)                                       ||     // composition
    (fname.size() >= 4 && "cube" == fname.substr(0,4))      ||     // avoid powers
    (fname.size() >= 6 && "square" == fname.substr(0,6))    ||
    (f->has_attribute("neighborhood"))                      ||     // already-indexed variable
    (std::string::npos != fname.find("Y_hat_"))                    // calibration variable
    ;
}



