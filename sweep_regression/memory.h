/*
	Dynamic memory manager

	  8 Oct 99 ... Created for debugging sweeper application.
*/
#ifndef _MEMORY_
#define _MEMORY_


void *Allocate(long nBytes, char *creator, char nameTag[4]);

int Release (void *ptr);

int CheckMemoryUse (int print);
/*
	Returns 1 if a problem is found, zero otherwise.  Prints information
	if argument is non-zero.
*/

int PrintMemoryUse (void);
int PrintRecentMemoryUse (int count);
int PrintMemoryUseByTag (char *tag);

int CheckPointer (void *ptr);

#endif
