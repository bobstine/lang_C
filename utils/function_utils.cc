#include "operator_traits.h"
#include "function_utils.h"  
#include <string>

using namespace Function_Utils;

// why does this not compile when put into .h???
// template <class T> std::string operator_traits< Function_Utils::Square<T> >::name()   { return "square"; }
// template <class T> std::string operator_traits< Function_Utils::Square<T> >::symbol() { return "^"; }
  
template <>
std::string operator_traits< Function_Utils::Square >::name()   { return "square"; }

template <>
std::string operator_traits< Function_Utils::Square >::symbol() { return "^"; }
  



template <>
std::string
operator_traits< Cube >::name()                    { return "cube"; }

template <>
std::string
operator_traits< Cube >::symbol()                  { return "^"; }



template <>
std::string
operator_traits< Power >::name()                    { return "power"; }

template <>
std::string
operator_traits< Power >::symbol()                  { return "^"; }



template <>
std::string
operator_traits< LogisticPos >::name()                         { return "logistic+"; }

template <>
std::string
operator_traits< LogisticPos >::symbol()                       { return "L"; }



template <>
std::string
operator_traits< Logit >::name()                   { return "logit"; }

template <>
std::string
operator_traits< Logit >::symbol()                 { return "l"; }



using Function_Utils::CenteredSquare;

template<> 
std::string
operator_traits< CenteredSquare >::name()   { return "square"; }

template<> 
std::string
operator_traits< CenteredSquare >::symbol() { return "^2"; }



using Function_Utils::CenteredCube;

template<> 
std::string
operator_traits< CenteredCube >::name()   { return "cube"; }

template<> 
std::string
operator_traits< CenteredCube >::symbol() { return "^3"; }



using Function_Utils::CenteredQuad;

template<> 
std::string
operator_traits< CenteredQuad >::name()   { return "fourth"; }

template<> 
std::string
operator_traits< CenteredQuad >::symbol() { return "^4"; }



using Function_Utils::CenteredQuint;

template<> 
std::string
operator_traits< CenteredQuint >::name()   { return "fifth"; }

template<> 
std::string
operator_traits< CenteredQuint >::symbol() { return "^5"; }


