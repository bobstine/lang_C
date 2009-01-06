// $Id: operator_traits.h,v 1.3 2005/09/02 02:41:31 bob Exp $

#ifndef _OPERATOR_TRAITS_H_
#define _OPERATOR_TRAITS_H_

#include <string>

template<class Op>
class operator_traits
{
 public:
  typedef Op operator_type;
	typedef typename Op::result_type result_type;

  static std::string name();
  static std::string symbol();
  static std::string parameters(Op const& f);
  
};

#endif

