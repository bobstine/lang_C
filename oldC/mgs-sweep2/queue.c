// $Id: queue.c,v 1.2 2001/09/26 12:19:00 bob Exp $

/*

  9 Sep 01 ... Implement first version
   
*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "queue.h"
#include "memory.h"
#include "coding.h"

///////------------------------------------------------------


double
constantQueueFunction(Queue queue)
{
  return queue->x;
}

Queue
NewConstantQueue (double value)
{
  Queue queue;

  queue = (Queue) Allocate( sizeof(aQueue), "ConsQue", "CQue");
  strcpy(queue->name, "Constant");
  queue->x = value;
  queue->count = 0;
  queue->f = constantQueueFunction;
  return queue;
}

///////////////////////////////////////////////////////////////////////////


double
codingQueueThreshold(Queue queue)
{
  return fStatisticThreshold (queue->s, queue->p, queue->count);
}

Queue
NewCodingQueue (double p, double s)
{
  Queue queue;

  queue = (Queue) Allocate( sizeof(aQueue), "CodeQue", "CQue");
  strcpy(queue->name, "Coding Threshold");
  queue->x = 0.0;
  queue->p = p;
  queue->s = s;
  queue->count = 0;
  queue->f = codingQueueThreshold;
  return queue;
}

///////////////////////////////////////////////////////////////////////////

double
GetNextValueFromQueue (Queue queue)
{
  ++ queue->count;
  queue->x = queue->f(queue);
  return queue->x;
} 

void
PrintQueue (Queue queue)
{
  printf("QUEUE: %s with value %8.2f (p=%6.3f, s=%6.3f) @ %ld\n",
	 queue->name, queue->x, queue->p, queue->s, queue->count);
}
