//  -*- c++ -*-

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

