// $Id: queue.h,v 1.2 2001/09/26 12:19:00 bob Exp $

/*
  Abstract object that returns a sequence of doubles.

   9 Sep ... Created.
   
*/

#ifndef _QUEUE_
#define _QUEUE_

typedef struct aQue
{
  char name[32];
  double p, s, x;
  long count;
  double (*f)(aQue *);
} aQueue;

typedef aQueue *Queue;


///////////////////////////////////

Queue
NewConstantQueue (double value);

Queue
NewCodingQueue (double geoProb, double geoShare);
/*
  geoProb  - Probability of coding next argument, as in a geometric
  geoShare - Proportion of probability devoted to the geometric
*/

double
GetNextValueFromQueue (Queue queue);

void
PrintQueue (Queue queue);

#endif
