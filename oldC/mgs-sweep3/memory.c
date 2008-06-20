/*
	Dynamic memory manager

	8 Oct 99 ... Created for debugging sweeper application.
*/

#include <stdlib.h>
#include <stdio.h>
#include "memory.h"

#define ExitOnNull(ptr,str,result) if(NULL==(ptr)){fprintf(stderr,"\nMEMORY: %s\n",(str));return (result);}
#define ExitOn(c, str, val) if (c){fprintf(stderr, "\nMEMORY: %s\n", (str)); return (val);}

#define LENGTH_CREATOR 16
#define LENGTH_TAG          4

typedef struct memrec
{	void *ptr;
	long nBytes;
	char tag[LENGTH_TAG];
	char creator[LENGTH_CREATOR];
	struct memrec *next;
} aMemrec;

typedef aMemrec *Memrec;


// ----------   Internal variables  --------------------------------

static Memrec theMemory = NULL;
static Memrec theSentinel = NULL; 
static long theMemoryUse = 0;
static long theMemoryCounter = 0;

//  --------    Local routines   -----------------------------------------------

static void InitializeMemory ();
static void CopyTag (char *to, char *from);
static int  AssignCreator (Memrec mr, char *str);
static void PrintMemrec (Memrec mr, int i);
static int  MatchesTag (Memrec mr, char *tag);


static void InitializeMemory ()
{
  theMemory  = new aMemrec;
  theSentinel = new aMemrec;
  theMemory->ptr = NULL;
  theMemory->nBytes=0;
  theMemory->creator[0] = '\0';
  CopyTag (theMemory->tag, "HEAD");
  theSentinel->ptr = NULL;
  theSentinel->nBytes = 0;
  theSentinel->creator[0] = '\0';
  CopyTag (theSentinel->tag, "SENT");
  theMemory->next = theSentinel;
  theSentinel->next = NULL;
}

static void CopyTag (char *to, char *from)
{
  int i;
  for (i=0; i<LENGTH_TAG; ++i)
    to[i] = from[i];
}

static int AssignCreator (Memrec mr, char *str)
{
  int i = 0;
  char c;
  
  while ((c = *str++) && i<LENGTH_CREATOR-1)
    mr->creator[i++] = c;
  mr->creator[i] = '\0';
  return i;
}

static char tagStr[1+LENGTH_TAG];

static void PrintMemrec (Memrec mr, int i)
{
  tagStr[LENGTH_TAG] = '\0';
  CopyTag (tagStr, mr->tag);
  printf ("[%4d]    %11ld  %11ld    %4s  %16s\n",
	  i, (long)mr->ptr, mr->nBytes, tagStr, mr->creator);
}

static int  MatchesTag (Memrec mr, char *tag)
{	
  int i=0;

  while ((i<LENGTH_TAG) && tag[i]==mr->tag[i])
    ++i;
  return (i == LENGTH_TAG);
}


//  ------------------------------------------------------------------------

void *Allocate(long nBytes, char *creator, char nameTag[4])
{
  Memrec mr;

  // check for initialization
  if (NULL == theMemory) InitializeMemory();
  // alloc memory record
  mr = new aMemrec;
  ExitOnNull(mr, "could not allocate memory record", NULL);
  // alloc memory to be returned
  mr->ptr = malloc(nBytes);
  ExitOnNull(mr->ptr, "could not allocate memory pointer", NULL);
  // fill in the record
  mr->nBytes = nBytes;
  AssignCreator(mr, creator);
  CopyTag (mr->tag, nameTag);
  mr->next = theMemory->next;
  theMemory->next = mr;
  ++theMemoryCounter;
  theMemoryUse += nBytes;
  return mr->ptr;
}


int Release (void *ptr)
{
  Memrec mr, toDelete;
  
  // find the record
  mr = theMemory;
  toDelete = NULL;
  while ((NULL==toDelete) && mr->next)
  { if (ptr == mr->next->ptr)
    { toDelete = mr->next;
      mr->next	= mr->next->next;
      -- theMemoryCounter;
      theMemoryUse -= toDelete->nBytes;
      free(toDelete->ptr);
      CopyTag(toDelete->tag, "NULL");
      delete(toDelete);
    }
    mr = mr->next;
  }
  if (NULL==toDelete)
  { fprintf(stderr, "MEMORY:  ran off end of list searching for %ld\n", (long)ptr);
    return 0;
  } else
    return 1;
}

int CheckMemoryUse (int print)
{
  Memrec mr;
  int i=0;
  long use=0;
  
  if (theMemory)
    mr = theMemory->next;
  else 
  { if (print) printf ("MEMORY:  no need to check; it is empty.\n");
    return 0;
  }
  while (mr->next)
  { use += mr->nBytes;
    ++i;
    if (i > theMemoryCounter)
    { printf (" *** Uh, oh.  Got a problem in the memory chain! *** \n");
      return 1;
    }
    mr = mr->next;
  }
  if ((use != theMemoryUse) || (i != theMemoryCounter))
  {
    if (print) 
    { printf ("MEMORY: problem found.\n");
	printf ("          Chain: ptr ct= %d with use %ld\n", i, use);
	printf ("          Check: ptr ct=%ld with use= %ld\n",
		theMemoryCounter, theMemoryUse);
    }
    return 1;
  }
  else
  { if (print) printf("MEMORY: checks OK with %d pointers using %ld bytes.\n", i, use);
    return 0;
  }
}

int PrintMemoryUse (void)
{
  long count;
  
  count = theMemoryCounter;
  return PrintRecentMemoryUse(count);
}

int PrintRecentMemoryUse (int count)
{
  Memrec mr;
  int i=0, limit;
  
  // check that we have been used
  if (theMemory)
    mr = theMemory->next;
  else 
  { printf ("MEMORY: has not been used.\n");
    return 0;
  }
  // put null last to terminate string for printing
  if (count < theMemoryCounter)
    printf ("=================  Memory Use  (top %d) ===================\n", count);
  else
    printf ("===================  Memory Use  =========================\n");
  printf ("  %ld terms use %ld bytes:\n",  theMemoryCounter, theMemoryUse);
  printf ("  age            addr        bytes     tag          creator\n");
  limit = (count < theMemoryCounter) ? count : theMemoryCounter;
  while (mr->next  && (i < limit))
  { PrintMemrec(mr, i);
    ++i;
    if (i > theMemoryCounter)
    {	printf ("Uh, oh.  Got a problem in the memory chain!\n");
	return 0;
    }
    mr = mr->next;
  }
  if ((count >= theMemoryCounter) && (i != theMemoryCounter))
    printf("MEMORY: print counter %d does not match referece count %ld\n",
	   i, theMemoryCounter);
  printf ("============================================================\n");
  return i;
}


int PrintMemoryUseByTag (char *tag)
{
  Memrec mr;
  int i=0;
  
  // check that we have been used
  if (theMemory)
    mr = theMemory->next;
  else 
  { printf ("MEMORY: has not been used.\n");
    return 0;
  }
  // put null last to terminate string for printing
  printf ("===================  Memory Use  (by tag %s) ==================\n", tag);
  printf ("  All %ld terms use total of %ld bytes:\n",  theMemoryCounter, theMemoryUse);
  printf ("  age            addr        bytes     tag          creator\n");
  while (mr->next  && (i < theMemoryCounter))
  {	if (MatchesTag(mr, tag)) PrintMemrec(mr, i);
	++i;
	if (i > theMemoryCounter)
	{	printf ("Uh, oh.  Got a problem in the memory!\n");
		return 0;
	}
	mr = mr->next;
  }
  if (i != theMemoryCounter)
    printf("MEMORY: list counter %d does not match referece count %ld\n",
	   i, theMemoryCounter);
  printf ("============================================================\n");
  return i;
}


int CheckPointer (void *ptr)
{
  Memrec mr;
  
  if (theMemory)
    mr = theMemory->next;
  else 
  { printf ("MEMORY: has not been used.\n");
    return 0;
  }
  mr = theMemory;
  while (mr->next)
  {
    if (ptr == mr->next->ptr)
      return 1;
    mr = mr->next;
  }
  fprintf(stderr, "MEMORY:  ran off end of list searching for pointer\n");
  return 0;
}

//////////////////////////  EOF ///////////////////////////////////////////
