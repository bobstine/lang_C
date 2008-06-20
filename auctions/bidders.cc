// $Id: bidders.cc,v 3.6 2008/01/20 00:00:41 bob Exp $

#include "bidders.h"

// universal PDF 
#include "coding.h"

#include <math.h>


//  BidHistory    BidHistory    BidHistory    BidHistory    BidHistory    BidHistory    BidHistory   

void               
BidHistory::append_bid_outcome (bool success)     
{ 
  int result ((success) ? 1 : 0); 
  mResults.push_back(result); 
  if (success) 
  { mBidsSinceLastSuccess = 0;
    ++mNumberSuccessfulBids;
  }
  else
    ++mBidsSinceLastSuccess;
}

