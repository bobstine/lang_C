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




using Function_Utils::CenteredSquare;

template<> 
std::string
operator_traits< CenteredSquare >::name()   { return "square"; }

template<> 
std::string
operator_traits< CenteredSquare >::symbol() { return "^2"; }

template<> 
std::string
operator_traits< CenteredSquare >::parameters(CenteredSquare const&) { return ""; }


using Function_Utils::CenteredCube;

template<> 
std::string
operator_traits< CenteredCube >::name()   { return "cube"; }

template<> 
std::string
operator_traits< CenteredCube >::symbol() { return "^3"; }

template<> 
std::string
operator_traits< CenteredCube >::parameters(CenteredCube const&) { return ""; }


using Function_Utils::CenteredQuad;

template<> 
std::string
operator_traits< CenteredQuad >::name()   { return "fourth"; }

template<> 
std::string
operator_traits< CenteredQuad >::symbol() { return "^4"; }

template<> 
std::string
operator_traits< CenteredQuad >::parameters(CenteredQuad const&) { return ""; }

using Function_Utils::CenteredQuint;

template<> 
std::string
operator_traits< CenteredQuint >::name()   { return "fifth"; }

template<> 
std::string
operator_traits< CenteredQuint >::symbol() { return "^5"; }

template<> 
std::string
operator_traits< CenteredQuint >::parameters(CenteredQuint const&) { return ""; }


/*
using Function_Utils::CenteredPower;

namespace {
#include <sstream>
  std::string size_to_string (size_t i)
  {
    std::ostringstream oss;
    oss << i;
    return oss.str();
  }
}

template<> 
std::string
operator_traits< CenteredPower >::name()   { return "power^" + size_to_string; }

template<> 
std::string
operator_traits< CenteredPower >::symbol() { return "^3"; }

template<> 
std::string
operator_traits< CenteredPower >::parameters(CenteredCube const&) { return ""; }

*/
