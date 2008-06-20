// $Id: cache.h,v 3.0 2004/11/19 18:58:36 foster Exp $

#ifndef _CACHE_H_
#define _CACHE_H_

/*
  CacheFunctions implement the visitor pattern and use a map to
  cache the various properties of features that they visit.

   8 Apr 04 ... Visitor policy for call-back.
  30 Mar 04 ... Created to factor feature properties out of the features themselves.
*/

#include "featureABC.h"

#include <map>

  
namespace Cache {
  
  class Center: std::unary_function<FeatureABC const*, double>
    {
      static std::map<FeatureABC const*, double> sCache;
      
    public:
      double operator()(FeatureABC const* f)
	{
	  if (sCache.count(f))
	    return sCache[f];
	  else // compute and memorize it
	  { std::cout << "CACH: Finding center of " << f->name() << std::endl;
	    double ave (f->center());
	    sCache[f] = ave;
	    return ave;
	  }
	}
    }; 

  class Scale: std::unary_function<FeatureABC const*, double>
    {
      static std::map<FeatureABC const*, double> sCache;
      
    public:
      double operator()(FeatureABC const* f)
	{
	  if (sCache.count(f))
	    return sCache[f];
	  else // compute and memorize it
	  { std::cout << "CACH: Finding scale of " << f->name() << std::endl;
	    double sd (f->scale());
	    sCache[f] = sd;
	    return sd;
	  }
	}
    }; 

}

/*

  Should this be a map, or have a map?
  -- Has one.  See below.

  How do we find the collection of properties of features?
  -- Its an operator class with a constructor.  With some sort of
     static map that holds its accumulated knowledge.
  
  How will features be identified?
  -- Need a better notion of the operator== function.

  Why lots of maps, rahter than a more global map that gives a variety
  of properties of a feature rather than a whole new tree for each?
  -- Easy, you might have the mean easily, but not some other property.
     Think of its map as a cache.
*/

#endif
