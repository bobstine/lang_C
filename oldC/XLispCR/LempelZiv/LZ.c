/* 				LZ code	24 Apr 96 ... Markov code initiated.	12 Apr 96 ... Initial debugged version complete and tested.	10 Apr 96 ... Started to speed lisp and help experimentation.	*/// Debug does more printing#define PRINT 0#define DEBUG 0//#include <stdlib.h>#include <math.h>#if PRINT#include <stdio.h>#endif#include "LZ.h"//==========================================================================//     Markov //==========================================================================//-------  Globals describing current Markov chain -------------------------#define MAX_MARKOV_DIM 10		// Change the size manually to keep in sync#define MARKOV_ARRAY_SIZE 1024#define LOG_OF_2    		0.693147181#define ONE_OVER_LOG_OF_2	1.442695041#define PI                  3.1415926#define LOG2(x)  (log (x) * ONE_OVER_LOG_OF_2)#define ABS(x)   (((x) < 0) ? (-x) : x)#define MIN(x,y) (((x) < (y)) ? (x) : (y))int     gMarkovArray[MARKOV_ARRAY_SIZE][2]; //  counts, with MSB of index the most recent value#define ONE		0#define COUNT	1int		gN;									//  length of the input dataint		gPenaltyMethod;						//  type of parameter penalty//-------  internal prototypes  --------------------------------------------// Utilitiesint		TwoToThe  (int i);double 	Entropy(int nOne, int n);double 	nEntropy(int nOne, int n);		// count times entropyvoid	PrintScales(void);// Markov arrayvoid 	InitMarkovArray(int size);double	MarkovArrayBits(int size);void	FindMinimalEntropy(int start, int size, 						int *ones, int *count,  int *nodeCount, double *bits);// Coding integersint		CauchyBits (int j);					int		RissanenBits (int j);int			RissBitsMinus1 (int j);int 	BitsForPositiveInteger(int n);int		BitsToCodeInteger (int n);int		PenaltyBits(double parentP, int parentSEBits, int childOnes, int childCount);void	CodingBits(int method, int size, int *pNodeCount, double *pBits);// Parameter conversiondouble	PFromTheta (double theta);double	ThetaFromP (double p);double	RoundTheta (double theta, int bits);int 	ThetaAccuracy (int count);//  Utilities int TwoToThe  (int i){	int power = 1, j;		for(j = 0; j<i; ++j)		power *= 2;	return(power);}double 	Entropy(int nOne, int n){	if((nOne == 0) || (nOne == n))			// thus returns zero if input n == 0		return(0.0);			{	double p;		p = nOne;		p /= n;		return( - ONE_OVER_LOG_OF_2 * (p * log(p) + (1-p) * log(1-p)) );	}}double nEntropy (int nOne, int n){	if (n == 0)		return(0.0);	else		return(n * Entropy(nOne, n));}//  Markov Arrayvoid InitMarkovArray(int size){	int i;		for (i=0; i<size; ++i)	{	gMarkovArray[i][0] = 0;		gMarkovArray[i][1] = 0;	}}#if PRINTvoid PrintMarkovArray(int size){	int i;		printf("\n  --- Markov Array --- \n");	for(i=0; i<size; ++i)	{	printf("[%3d] %3d %3d -- h --> %8.3f \n", i, 			gMarkovArray[i][ONE],  gMarkovArray[i][COUNT], 			Entropy(gMarkovArray[i][ONE], gMarkovArray[i][COUNT]));	}	printf("----------------------\n");}#endifdouble MarkovArrayBits(int size)			// Bits with no penalty for parameters{	int i;	double bits = 0.0;		for (i=0; i<size; ++i)	{	bits += Entropy(gMarkovArray[i][ONE],  gMarkovArray[i][COUNT]) *						(double) gMarkovArray[i][COUNT];	}	return (bits);}//  Coding integersint BitsToCodeInteger (int n){	if (n == 0)		return(1);	else 		return(1 + 1 + RissanenBits(ABS(n)));		// (sign) plus (not zero)}int BitsForPositiveInteger (int n){	return( ((int) LOG2((double)n)) + 1);}int CauchyBits (int j){#if PRINT	if (j <= 0)		printf("Called CauchyBits with arg %d\n", j);#endif	return(2 * BitsForPositiveInteger(j) + 1);}int RissanenBits (int j){	if (j <= 0)	{	#if PRINT		printf("Called Rissanen Bits with arg %d\n", j);#endif		return 1;	}	return(1 + RissBitsMinus1(j));}int RissBitsMinus1 (int j){	if (j == 1)		return(0);	else if ((j == 2) || (j == 3))		return(2);	else 	{	int len=BitsForPositiveInteger(j);		return(RissBitsMinus1(len-1) + len);	}}//  Variance stabilized transformationdouble PFromTheta (double theta){	double p;		p = sin(theta);	return (p * p);}double ThetaFromP (double p){	return( asin(sqrt(p)) );}int ThetaAccuracy (int count){	if (0 == count)	{	#if DEBUG		printf ("Warning: ThetaAccuracy given count of zero...\n");#endif		return(0);	} else		return((int) (0.5 * LOG2(count))); 	// floor(log2(sqrt(n)))}	double RoundTheta (double theta, int bits){	double scale=1.0;	int i;	int intTheta;	//	printf("Rounding theta input %8.3f to %d bits gives", theta, bits);	for(i=0; i<bits; ++i)		scale *= 2.0;	// round it off based on [0,1] scale	intTheta = (int) (2 * (theta)/ PI * scale);//	printf(" int theta= %d ", intTheta);	// convert back to point in middle of interval	theta = (PI /2.0* ((double)intTheta + 0.5)/scale);//	printf(" %8.3f \n", theta);	return(theta);}#if PRINTvoid PrintScales(void){  int bits;  for(bits = 1; bits <= 5;bits++)    {      double last_theta = -10;      double p;      double theta;      double first_theta;      printf("\n\n\nTo %d accuracy\n\n",bits);      printf("p \t-->\tTheta\t-->\tRound\t-->\tp\t|\t2theta/PI   --> 2Round/PI\n");      for(p = 0;p <= 1;p += .0001)	{	  theta = RoundTheta(ThetaFromP(p),bits);	  if (theta != last_theta)	    {	      last_theta = theta;	      printf("%.5f\t\t%.4g\t\t%.4g\t\t%.5f\t|\t%.5f\t%.5f\n",		     p,ThetaFromP(p),theta,PFromTheta(theta),2.0*ThetaFromP(p)/PI,2.0*theta/PI);	    };	};          };}#endif//  Calculate penalty for encoding next level below.int PenaltyBits(double parentTheta, int parentAccuracy, int childOnes, int childCount){		int childAccuracy, bits;	double z, scale, roundedParentTheta, roundedChildTheta, childTheta;		childAccuracy = ThetaAccuracy(childCount);					// check for empty node	if (childAccuracy == 0)	{		 	return(RissanenBits(1 + parentAccuracy));				// - childAccuracy (which is zero)	}	else	{	roundedParentTheta = RoundTheta(parentTheta, parentAccuracy);		// to parent's scale		roundedParentTheta = RoundTheta(roundedParentTheta, childAccuracy);	// to child's scale		childTheta = ((double) childOnes)/((double) childCount);		roundedChildTheta =  RoundTheta(childTheta, childAccuracy);			scale = (1.0 / PI) * TwoToThe(childAccuracy);					// 2 over 2 = 1; theta in [0,pi/2]		z = (roundedChildTheta - roundedParentTheta) * scale;#if PRINT		if ((double)(int)z != z)			printf("Non-integer z encounted %f\n", z);#endif		bits = BitsToCodeInteger ((int)z);			if (bits < 0)		{#if PRINT			printf ("Negative bits: %7.2f - %7.2f --> z = %8.3f", roundedChildTheta, roundedParentTheta, z);#endif			return(1);		}#if PRINT		if (1 + parentAccuracy - childAccuracy < 1)			 printf("2 Calling .. %4d, child=%d\n", 1 + parentAccuracy - childAccuracy, childAccuracy);#endif	 	bits += RissanenBits (1 + parentAccuracy - childAccuracy);			// "non-neg" Riss bits		return (bits);	}}/*	 	Recursive entropy function using leaves computed in gEntropyArray	and counts in gMarkovArray.  Requires global nObs to be set prior to first	call.		Entropy computed as total number of bits to encode the given leaf. Bits computed	under the assumption of a test with known null p.*/void FindMinimalEntropy(int start, int size, 						int *ones, int *count,  int *nodeCount, double *bits){	if (1 == size)		// compute optimal bits treating p={# 1}/n as known	{	*ones  = gMarkovArray[start][ONE];		*count = gMarkovArray[start][COUNT];		*nodeCount = 1;		*bits = 1.0 + Entropy(*ones, *count) * *count;  // 1.0 -> rounding error		if (gPenaltyMethod == 0)			*bits -= 1.0;	}	else 				// recurse	{	int newSize = size/2;		int leftCount, leftOnes, rightCount, rightOnes, myAccuracy, leftNodes, rightNodes;		double myTheta, myBits, leftBits, rightBits;				FindMinimalEntropy(start        , newSize,  &leftOnes, &leftCount , &leftNodes, &leftBits);		FindMinimalEntropy(start+newSize, newSize, &rightOnes, &rightCount, &rightNodes,&rightBits);		*count = leftCount + rightCount;		*ones  = leftOnes  + rightOnes;							myBits = 1.0 + nEntropy(*ones, *count);		// 1.0 for rounding p 														switch (gPenaltyMethod)		{	case 0: 				myBits += 10000.0;					// force whole tree				leftBits -= 1.0;					// force to equal leaf nodes				break;											case 1:		// 1 is Foster-Stine				myAccuracy = ThetaAccuracy(*count) ; 					myTheta    = ThetaFromP (((double) *ones) / ((double) *count));				leftBits  += PenaltyBits (myTheta, myAccuracy,  leftOnes,  leftCount);				rightBits += PenaltyBits (myTheta, myAccuracy, rightOnes, rightCount);				break;			case 2:		// 2 is Rissanen				 leftBits += 2 + LOG2(gN);					// shortcut to adding half this to each.				break;		}		if (myBits <= leftBits + rightBits)		{	*bits = 1.0 + myBits;					// 1 to indicate whether children			*nodeCount = 1;		} else		{	*bits = 1.0 + leftBits + rightBits;		// 1 to indicate whether children			*nodeCount = 1 + leftNodes + rightNodes;		}#if DEBUG			{	int i;				printf(" %3d %3d %7.3f \n", *ones, *count, myBits);				for (i = 1; i < size; i *= 2) printf("....");				printf ("Node bits = %7.3f; Leaf bits = %7.3f --> %7.3f \n", 					myBits, leftBits + rightBits, *bits);			}#endif	}}void CodingBits(int method, int size, int *pNodeCount, double *pBits){	int ones, count;	double myBits, theirBits;			gPenaltyMethod = method;	switch (method)	{	case -1 : *pBits =  MarkovArrayBits(size);				  *pNodeCount = size;				  break;		case  0 : FindMinimalEntropy(0, size, &ones, &count, pNodeCount, pBits);				  break;		case  1 : FindMinimalEntropy(0, size, &ones, &count,  pNodeCount, pBits);				  *pBits += PenaltyBits(0.5, ThetaAccuracy(gN), ones, count);				  *pNodeCount += 1;				  break;		case  2 : FindMinimalEntropy(0, size, &ones, &count,  pNodeCount, pBits);				  myBits	=  1.0 + gN;						// 1.0 for to code or not to code				  theirBits =  1.0 + *pBits + 0.5 * LOG2(gN);   //  + parent parameter penalty				  if (myBits <= theirBits)				  {		*pBits = myBits;				  		*pNodeCount = 1;				  } 				  else				  {		*pBits = theirBits;				  		*pNodeCount += 1;				  }				  break;	}}//==========================================================================//     Exported Markov functions //==========================================================================void FitMarkov(long x[], long *pn, long *pk, long *offset, long *penaltyMethod, 				long *pNodeCount, double *pBits){	long *pXt;	unsigned int  index, limit, t, power, shift;	int nodeCount;		gN = *pn;	if (*pk > MAX_MARKOV_DIM || *pk <= 0 || *pk >= (*pn - *offset))		// check limits	{	*pBits = -7.7;		return;	}	if (*offset <= *pk) *offset = *pk + 1;		// offset > k	index = 0; power=1;							// initialize index and pointer	for(t=*offset - *pk; t < *offset; ++t)		// with closest setting MSB	{	index += x[t] * power;		power *= 2;	}											// power = 2^k = size of the underlying array	InitMarkovArray(power);	limit  = *pn - *offset;	shift = *pk - 1;	pXt   = &x[*offset];	while(limit)												{	gMarkovArray[index][ONE] += *pXt;		// increment counters in global array		++ gMarkovArray[index][COUNT];		index >>= 1;							// shift right 		index += (*pXt << shift);				// set MSB most recent value		++ pXt;		-- limit;	}	CodingBits(*penaltyMethod, power, &nodeCount, pBits);	*pNodeCount = nodeCount;#if PRINT	printf ("\n\n _____  Completed using method %d ____ \n", 				*penaltyMethod);#endif}void MarkovBits(long x[], long *pn, long *pk, long *offset, long nodeCount[4], double bits[4]){	long *pXt;	unsigned int  index, limit, t, power, shift;	int nc[4];		gN = *pn;	if (*pk > MAX_MARKOV_DIM || *pk <= 0 || *pk >= (*pn - *offset))		// check limits	{	bits[0] = bits[1] = bits[2] = bits[3] = 0.0;		return;	}	if (*offset <= *pk) *offset = *pk + 1;		// offset > k	index = 0; power=1;							// initialize index and pointer	for(t=*offset - *pk; t < *offset; ++t)		// with closest setting MSB	{	index += x[t] * power;		power *= 2;	}											// power = 2^k = size of the underlying array	InitMarkovArray(power);	limit  = *pn - *offset;	shift = *pk - 1;	pXt   = &x[*offset];	while(limit)												{	gMarkovArray[index][ONE] += *pXt;		// increment counters in global array		++ gMarkovArray[index][COUNT];		index >>= 1;							// shift right 		index += (*pXt << shift);				// set MSB most recent value		++ pXt;		-- limit;	}	CodingBits(-1,power, &nc[0], &bits[0]);	CodingBits( 0,power, &nc[1], &bits[1]);	CodingBits( 1,power, &nc[2], &bits[2]);	CodingBits( 2,power, &nc[3], &bits[3]);	for (t=0; t<4; ++t)		nodeCount[t] = nc[t];#if PRINT	PrintScales();#endif}//==========================================================================//     LZ //==========================================================================//-------  Data structures  --------------------------------------------typedef struct treeNode *TreeNodePtr;typedef struct {	int count;					// held as multiples of 1/(gTreeArraySize-1)	TreeNodePtr ptr;} CodeItem;typedef struct treeNode {	TreeNodePtr pred;	CodeItem *array;} TreeNode;//-------  Globals describing current parse tree ----------------------------TreeNodePtr	gTreeRoot;int 		gTreeArraySize;		// at least 2int			gMaxCount;#ifdef DEBUGint gDebugNodeCount = 0;#endif	//-------  internal prototypes  --------------------------------------------TreeNodePtr AllocateTreeNode (TreeNodePtr pred);void 		FreeTree (TreeNodePtr pNode);double 		AvgNodeValue (TreeNodePtr pNode);double 		TreeLength (TreeNodePtr pNode, int depth);void 		PrintTreeNode (TreeNodePtr pNode);void 		PrintTree (TreeNodePtr pNode, int codeWord[], int cwIndex);//--------------------------------------------------------------------------TreeNodePtr AllocateTreeNode (TreeNodePtr pred){	TreeNodePtr pTheNode;	int i;#if PRINT	printf ("Allocating tree node #%d\n", ++gDebugNodeCount);#endif	pTheNode = (TreeNodePtr) malloc (sizeof (struct treeNode));	pTheNode->pred  = pred;	pTheNode->array = (CodeItem *) malloc(gTreeArraySize * sizeof (CodeItem));	for (i=0; i<gTreeArraySize; ++i)	{	pTheNode->array[i].count = 1;  // multiples of 1/(gTreeArraySize-1),		pTheNode->array[i].ptr = nil;	}	return (pTheNode);}void FreeTree (TreeNodePtr pNode){	int i;		for (i=0; i < gTreeArraySize; ++i)	 	if (pNode->array[i].ptr)			FreeTree(pNode->array[i].ptr);	free(pNode->array);	free(pNode);#if DEBUG	printf ("Disposing of tree node; %d remain.\n", --gDebugNodeCount);#endif}	//----------double AvgNodeValue (TreeNodePtr pNode){	int i;	int  sum,  n;	double avg;		sum = n = 0;	for (i=0; i<gTreeArraySize; ++i)		if (pNode->array[i].count > 0)		{	n   += pNode->array[i].count;			sum += i * pNode->array[i].count;		}	avg = (double) sum;	avg *= (double) (gTreeArraySize-1);	avg /= n;	// PrintTreeNode(pNode);	// printf ("Prediction = %7.3f\n", avg);	return (avg);}//----------#if PRINTvoid PrintTreeNode (TreeNodePtr pNode){		int i;		printf ("\n------- Tree node %ld \n", pNode);	for (i=0; i<gTreeArraySize; ++i)		printf("[%2d] (%3d/%3d)  %ld \n", 			i, pNode->array[i].count, gTreeArraySize-1, pNode->array[i].ptr);}void PrintTree (TreeNodePtr pNode, int codeWord[], int cwIndex){	int i,j;		PrintTreeNode(pNode);	for (i=0; i<gTreeArraySize; ++i)	{	if (pNode->array[i].count > 1)					// initialized as 1		{	codeWord[cwIndex] = i;			printf(" [%2d]   ", pNode->array[i].count);			for (j=0; j<=cwIndex; ++j)				printf("%d", codeWord[j]);			printf("\n");			if (pNode->array[i].ptr) 				PrintTree(pNode->array[i].ptr, codeWord, cwIndex+1);		}	}}	#endif//-----------------------------------------------------------------------void LZ (long x[], long *pn, long *nQuant, long *maxCount, double xHat[]){	long 		time;	int  		countIncr, nRestarts = 0;	TreeNodePtr pCurrNode;	CodeItem 	*pCodeItem;	double 		*pPred;		if (xHat) 		pPred = xHat;	else pPred = nil;	gTreeArraySize = *nQuant;						// array size == k	countIncr = gTreeArraySize - 1;					// k-1	gMaxCount = 1 + *maxCount * (gTreeArraySize-1);	// max count in multiples of k-1						gTreeRoot = AllocateTreeNode(nil);					pCurrNode = gTreeRoot;							// initialize tree	for (time=0; time<*pn; ++time)	{	if (pPred) 									// predict time t			*pPred++ = AvgNodeValue(pCurrNode);		pCodeItem =  &pCurrNode->array[x[time]];	// look x[t] in current node		pCodeItem->count += countIncr;				// increment count by k-1		if (pCodeItem->count <= gMaxCount)			// --- found new code word		{	pCurrNode = gTreeRoot;					// go back to root			x[nRestarts++] = time;			if (pCodeItem->ptr == nil)				// allocate new node				pCodeItem->ptr = AllocateTreeNode (pCurrNode);		}		else 										// extending code word			pCurrNode = pCodeItem->ptr;	}	x[0] = nRestarts;}//-----------------------------------------------------------------------void LZSummarizeTree (long *nCodeWords, double *avgCodeLen){	int i;		*nCodeWords = 0.0;	for (i=0; i<gTreeArraySize; ++i)		*nCodeWords += gTreeRoot->array[i].count;	*avgCodeLen = TreeLength(gTreeRoot,1);	*avgCodeLen /= *nCodeWords;}double TreeLength (TreeNodePtr pNode, int depth){	int i,n;	long iLen=0;	double res;		for (i=0; i<gTreeArraySize; ++i)	{	n = pNode->array[i].count;		if (n > 0)			if (pNode->array[i].ptr)				iLen += gMaxCount * depth + TreeLength(pNode->array[i].ptr, depth+1);			else				iLen += n * depth;	}	res = (double)iLen;	return(res/(double)(gTreeArraySize-1));}//-----------------------------------------------------------------------#if PRINTvoid LZPrintTree (void){	int codeWord[20];	long n;	double len;		LZSummarizeTree(&n, &len);	printf(" - - - - - - - - - - - - - \n");	printf(" %ld code words, avg length = %10.3f\n", n, len);	printf(" Weight   Codeword \n");	PrintTree(gTreeRoot, codeWord, 0);}#endif//-----------------------------------------------------------------------void LZDisposeTree (void){	FreeTree(gTreeRoot);}//-----------------------------------------------------------------------