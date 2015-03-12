// $Id: operator_traits.cc,v 1.4 2008/01/29 23:44:01 bob Exp $

#include "operator_traits.h"
#include "function_utils.h"

typedef float Scalar;

// Sum

template<> 
std::string
operator_traits< std::plus<Scalar> >::name()   { return "sum"; }

template<> 
std::string
operator_traits< std::plus<Scalar> >::symbol() { return "+"; }


// Difference

template<> 
std::string
operator_traits< std::minus<Scalar> >::name()   { return "difference"; }

template<> 
std::string
operator_traits< std::minus<Scalar> >::symbol() { return "*"; } 


// Product

template<> 
std::string
operator_traits< std::multiplies<Scalar> >::name()   { return "product"; }

template<> 
std::string
operator_traits< std::multiplies<Scalar> >::symbol() { return "*"; } 


// Divides

template<> 
std::string
operator_traits< std::divides<Scalar> >::name()   { return "divide"; }

template<> 
std::string
operator_traits< std::divides<Scalar> >::symbol() { return "/"; }


