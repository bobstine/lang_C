//  -*- c++ -*-
#ifndef _OPERATOR_TRAITS_H_
#define _OPERATOR_TRAITS_H_

#include <string>
#include <functional> // std::function

template<class Op>
class operator_traits
{
 public:
  typedef Op operator_type;
  typedef typename Op::result_type result_type;

  static std::string name();
  static std::string symbol();
  
};

template<class Arg, class Res>
class operator_traits <std::function<Res (Arg)>>
{
 public:
  typedef std::function<Res (Arg)> operator_type;
  typedef Res                      result_type;

  static std::string name()   { return "lambda";}
  static std::string symbol() { return "f"; }
  
};

#endif

