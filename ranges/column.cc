#include "column.h"

//   IntegerColumn      IntegerColumn      IntegerColumn      IntegerColumn      IntegerColumn      IntegerColumn      IntegerColumn      

void
IntegerColumn::print_to(std::ostream &os) const
{
  int counter (mData->size());
  std::string more ("");
  // print up to 10 elements
  if (counter > 10)
  { counter = 10;
    more = "...";
  }
  os << mData->name() << "[" << mData->size() << "]";
  int * pInt (mData->begin());
  while(counter--) os << " " << *pInt++;
  os << more;
}

void
IntegerColumn::transfer_from_double (double *pDouble)
{
  int counter (mData->size());
  int *pDest  (mData->begin()); 
  while(counter--) *pDest++ = (int) floor(*pDouble++);
}


void
IntegerColumn::transfer_from_float (float *pFloat)
{
  int counter (mData->size());
  int *pDest  (mData->begin()); 
  while(counter--) *pDest++ = (int) floor(*pFloat++);
}

