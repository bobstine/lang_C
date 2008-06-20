/*
 *  polyshrink.h
 *  utils
 *
 *  Created by Robert Stine on 11/9/07.
 *  Copyright 2007. All rights reserved.
 *

 Polyshrink implements the polyshrink estimator over a range specified by an iterator.
 
 */

#ifndef _POLYSHRINK_H_
#define _POLYSHRINK_H_



template <class Container>
void
polyshrink (Container const& input, Container & output);



#include "polyshrink.Template.h"

#endif  _POLYSHRINK_H_