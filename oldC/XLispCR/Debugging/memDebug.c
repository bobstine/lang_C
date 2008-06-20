/*	17 Jul 89 ... Created from old array def.*/#include "Debug.h"#include "memDebug.h"#include <stdio.h>/*	_____ Debugging memory chain to check allocations _____	*/static int newList = 1;  /* 1 = TRUE */static int totalBytes = 0;struct MEMREC {	struct MEMREC *next;	char *ptr;	int nBytes;		/* total allocated */	int length;		/* number of elements allocated, each of size nBytes/length */	} MEMREC;struct MEMREC *headPtr, last;/*  ______________  Local Functions  ______________  */int CheckPtrIndex (char *ptr, long index);int CheckPtrIndex (ptr, index)char *ptr;long index;{	struct MEMREC *mp;	int i=0;		mp = headPtr;	while (mp)	{	++i;		if (mp->ptr ==  ptr)		{	if ((index >= 0) && (index < mp->length))				return(1);			else printf (" --- Pointer index range violation\n");				return(0);		}		mp = mp->next;	}	printf (" --- Match to pointer NOT found...\n");	Wait();	return(-1);}char *AllocateVec(length, size)long length, size;{	struct MEMREC *new;	char *ptr;	long nBytes;		if (newList)	/* Initialize the memory list */	{	newList = 0;		headPtr = &last;		last.next = NULL;		last.nBytes = -1;		last.ptr = NULL;	}	new = (struct MEMREC *) NewPtr(sizeof(struct MEMREC));	nBytes = length * size;	ptr = NewPtr (nBytes);	new->next = headPtr;	new->nBytes = nBytes;	new->length = length;	new->ptr = ptr;	headPtr = new;	totalBytes += nBytes;	return (ptr);}int FreeVec (ptr)char *ptr;{	struct MEMREC *mp, *nextRec;		if (ptr == headPtr->ptr)		/* check for first */	{	totalBytes -= headPtr->nBytes;		DisposPtr (headPtr->ptr);		nextRec = headPtr->next;		DisposPtr ( (char *) headPtr);		headPtr = nextRec;		return (1);	}	mp = headPtr;	while (mp->next)	{	if (ptr == mp->next->ptr)		/* find match */		{	totalBytes -= mp->next->nBytes;			DisposPtr (mp->next->ptr);			nextRec = mp->next->next;			DisposPtr ( (char *)mp->next );			mp->next = nextRec;			return (1);		}		mp = mp->next;	}	fprintf (stderr,"Record NOT found; suggest terminating if should be!\n");	Wait();	return (-1);}void LongAssign (ptr, index, value)long *ptr,index,value;{	if (CheckPtrIndex ((char*) ptr, index))		ptr[index] = value;}void LongMatAssign (mat, j, i, value)long **mat, j, i, value;{	if (CheckPtrIndex ((char *) mat, j) && CheckPtrIndex( (char *)mat[j], i))		mat[j][i] = value;}		long LongIndex (ptr, index)long *ptr, index;{	if (CheckPtrIndex ((char*) ptr, index))		return (ptr[index]);}	double  DoubleIndex  (ptr, index)double *ptr; long index;{	if (CheckPtrIndex ((char*) ptr, index))		return (ptr[index]);}void DoubleAssign (ptr, index, value)double *ptr, value; long index;{	if (CheckPtrIndex ((char *) ptr, index))		ptr[index] = value;}void DoubleMatAssign (mat, j, i, value)double **mat, value;long i, j;{	if (CheckPtrIndex ((char *) mat, j) && CheckPtrIndex( (char *)mat[j], i))		mat[j][i] = value;}void PrintMemChain(){	struct MEMREC *mp;	int i=0;		mp = headPtr;	fprintf (stderr,"\nDump of current memory list totaling %d bytes.\n", totalBytes);	while (mp)	{	fprintf (stderr,"Record #%3d with %4d bytes @location %lx\n",							++i, mp->nBytes, mp->ptr);		mp = mp->next;	}	Wait();}void CheckMemPointer (ptr, str)char *ptr, *str;{	struct MEMREC *mp;	int i=0;		fprintf (stderr,str);	mp = headPtr;	while (mp)	{	++i;		if (mp->ptr ==  ptr)		{	 fprintf (stderr," --- pointer match found in record %d with %d bytes./\n", i, mp->nBytes);			return;		}		mp = mp->next;	}	fprintf (stderr," --- match to pointer NOT found...\n");	Wait();}