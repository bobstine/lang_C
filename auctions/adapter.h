/* $Id: adapter.h,v 3.2 2008/06/19 22:55:08 bob Exp $
 *  adapter.h
 *  auctions
 *
 *  Created by Robert Stine on 2/22/08.
 *  Copyright 2008. All rights reserved.
 *
 */
#ifndef _ADAPTER_H_
#define _ADAPTER_H_


#include <gsl/gsl_matrix.h>
#include "my_features.h"

gsl_matrix *
ConvertFeaturesIntoMatrix(Features::FeatureVector v);

#endif

