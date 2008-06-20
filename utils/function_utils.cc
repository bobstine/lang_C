// $Id: function_utils.cc,v 1.7 2008/01/29 23:44:01 bob Exp $

#include "operator_traits.h"
#include "function_utils.h"  
#include <string>

using namespace Function_Utils;

template <>
std::string
operator_traits< Square >::name()                    { return "square"; }

template <>
std::string
operator_traits< Square >::symbol()                  { return "^"; }

template <>
std::string
operator_traits< Square >::parameters(Square const&) { return ""; }


template <>
std::string
operator_traits< Cube >::name()                    { return "cube"; }

template <>
std::string
operator_traits< Cube >::symbol()                  { return "^"; }

template <>
std::string
operator_traits< Cube >::parameters(Cube const&)   { return ""; }


template <>
std::string
operator_traits< Power >::name()                    { return "power"; }

template <>
std::string
operator_traits< Power >::symbol()                  { return "^"; }

template <>
std::string
operator_traits< Power >::parameters(Power const&)   { return ""; }


template <>
std::string
operator_traits< LogisticPos >::name()                         { return "logistic+"; }

template <>
std::string
operator_traits< LogisticPos >::symbol()                       { return "L"; }

template <>
std::string
operator_traits< LogisticPos >::parameters(LogisticPos const&) { return ""; }


template <>
std::string
operator_traits< Logit >::name()                   { return "logit"; }

template <>
std::string
operator_traits< Logit >::symbol()                 { return "l"; }

template <>
std::string
operator_traits< Logit >::parameters(Logit const&) { return ""; }

