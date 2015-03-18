#include "feature_predicates.h"


bool
FeaturePredicates::mutually_exclusive_categories_from_same_parent(Feature const& f1, Feature const& f2) 
{
  if ( (!f1->has_attribute("parent"))
       || (!f2->has_attribute("parent"))          // false unless both have parents
       || ( f1->attribute_str_value("parent") != f2->attribute_str_value("parent"))       // false unless parents match
       || (!f1->has_attribute("category"))
       || (!f2->has_attribute("category"))   )    // false unless both have category attribute
    return false;
  else
    return (f1->attribute_str_value("category") != f2->attribute_str_value("category"));
}

inline
std::ostream&
operator<<(std::ostream& os, FeatureABC::DependenceMap const& dm)
{
  for(auto p : dm)
    os << "{ " << p.first->name() << "," << p.second << " } ";
  return os;
}

/*
  Note that dependence map is used to manage those products constructed in interactions
*/
bool
FeaturePredicates::share_common_parent_in_dependence_map(Feature const& f1, Feature const& f2)      // does not do what it says!
{
  // std::cout << "DEBUG: In share_common_parent.  Features are \n"
  // 	    << "       F1: " << f1 << std::endl
  // 	    << "       F2: " << f2 << std::endl;
  FeatureABC::DependenceMap d1 (f1->dependence_map());
  FeatureABC::DependenceMap d2 (f2->dependence_map());
  // std::cout << "       Initial f1 dependence map:  " << d1 << std::endl;
  // std::cout << "               f2                  " << d2 << std::endl;
  // a feature is itself to first power
  if (d1.empty()) d1[f1]+=1;
  if (d2.empty()) d2[f2]+=1;
  // std::cout << "       Second  f1 dependence map:  " << d1 << std::endl;
  // std::cout << "               f2                  " << d2 << std::endl;
  // get parents of f1 and f2
  auto pf ([](FeatureABC::DependenceMap::value_type const& p) { std::string par=p.first->attribute_str_value("parent"); return par.empty()?p.first->name():par;});
  std::set<std::string> p1;
  std::for_each(d1.begin(), d1.end(), pf);
  std::set<std::string> p2;
  std::for_each(d2.begin(), d2.end(), pf);
  //  std::cout << "                         "
  //	    << f1->name() << " has " << p1.size() << " parents '" << p1 << "'  ---   " << "Those of " << f2->name() << " are '" << p2 << "'" << std::endl;
  for(std::set<std::string>::const_iterator it = p1.begin(); it != p1.end(); ++it)
    if (p2.find(*it)!=p2.end())
      return true;
  return false;
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



