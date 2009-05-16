// $Id: operator_traits.cc,v 1.4 2008/01/29 23:44:01 bob Exp $

#include "operator_traits.h"
#include "function_utils.h"

// Sum

template<> 
std::string
operator_traits< std::plus<double> >::name()   { return "sum"; }

template<> 
std::string
operator_traits< std::plus<double> >::symbol() { return "+"; }

template<> 
std::string
operator_traits< std::plus<double> >::parameters(std::plus<double> const&) { return ""; }


// Difference

template<> 
std::string
operator_traits< std::minus<double> >::name()   { return "difference"; }

template<> 
std::string
operator_traits< std::minus<double> >::symbol() { return "*"; } 

template<> 
std::string
operator_traits< std::minus<double> >::parameters(std::minus<double> const&) { return ""; }


// Product

template<> 
std::string
operator_traits< std::multiplies<double> >::name()   { return "product"; }

template<> 
std::string
operator_traits< std::multiplies<double> >::symbol() { return "*"; } 

template<> 
std::string
operator_traits< std::multiplies<double> >::parameters(std::multiplies<double> const&) { return ""; }


// Divides

template<> 
std::string
operator_traits< std::divides<double> >::name()   { return "divide"; }

template<> 
std::string
operator_traits< std::divides<double> >::symbol() { return "/"; }

template<> 
std::string
operator_traits< std::divides<double> >::parameters(std::divides<double> const&) { return ""; }


// Powers

using Function_Utils::CenteredSquare;
using Function_Utils::CenteredCube;

template<> 
std::string
operator_traits< CenteredSquare >::name()   { return "square"; }

template<> 
std::string
operator_traits< CenteredSquare >::symbol() { return "^2"; }

template<> 
std::string
operator_traits< CenteredSquare >::parameters(CenteredSquare const&) { return ""; }


template<> 
std::string
operator_traits< CenteredCube >::name()   { return "cube"; }

template<> 
std::string
operator_traits< CenteredCube >::symbol() { return "^3"; }

template<> 
std::string
operator_traits< CenteredCube >::parameters(CenteredCube const&) { return ""; }




