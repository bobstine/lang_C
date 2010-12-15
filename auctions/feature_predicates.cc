#include "feature_predicates.h"


bool
FeaturePredicates::indicators_from_same_parent(Feature const& f1, Feature const& f2) 
{
  return f1->has_attribute("category")
    && f2->has_attribute("category")
    && (f1->attribute_str_value("parent")==f2->attribute_str_value("parent"));
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
  std::cout << "TEST: checking whether " << fname << " is derived;  [" << b1 << b2 << b3 << b4 << "]\n";
  return
    (f->is_constant())       || (f->is_dummy())             ||
    (f->degree() > 2)                                       ||     // composition
    (fname.size() >= 4 && "cube" == fname.substr(0,4))      ||     // avoid powers
    (fname.size() >= 6 && "square" == fname.substr(0,6))    ||
    (f->has_attribute("neighborhood"))                      ||     // already-indexed variable
    (std::string::npos != fname.find("Y_hat_"))                    // calibration variable
    ;
}



