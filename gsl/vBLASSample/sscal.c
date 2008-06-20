/*******************************************************************************
*                                                                              *
*     File Name: sscal.c                                                       *
*                                                                              *
*     This sample illustrates the usage of 'cblas_sscal()' function and do     *
*     timing for 1000000 times executions.                                     *
*                                                                              *
*     Copyright © 2000-2002 Apple Computer, Inc.  All rights reserved.         *
*                                                                              *
*******************************************************************************/

#include <Carbon/Carbon.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <CoreServices/CoreServices.h>
#include <vecLib/vBLAS.h>

#include "JavaMode.h"
#include "main.h"

#define MAX_LOOP_NUM       1000000  // Number of iterations used in the timing loop
#define kHasAltiVecMask    ( 1 << gestaltPowerPCHasVectorInstructions )  // used in looking for a g4

void Dummy_cblas_sscal ( int N, float alpha, const float* X, int incX );


void RunSSCALSample ( void ) {
    float *X; 
    int N, i;
    float alpha;
    
    // Initialize the inputs and allocate space
    alpha = 2.0;
    N = 10;    
    X = ( float* ) malloc ( sizeof ( float ) * N );
   
    for ( i = 0; i < N; i++ ){
        X[i] = 1.0;
        }

    // testing 'cblas_sscal()'.
    printf("before scal contents in X.\n");
    for ( i = 0; i < N; i++ ){
        printf("%4.4f ", X[i]);
        }
    printf("\n");

    cblas_sscal ( N, alpha, X, 1 );
    
    printf("\nafter scal contents in X.\n");
    for ( i = 0; i < N; i++ ){
        printf("%4.4f ", X[i]);
        }
    printf("\n");
    
    // Timing section for 'cblas_sscal()'
      	{
            float time, overheadTime;
      
            // Turn Java mode off.  Otherwise, there is an extra cycle added to the vfpu.
            // WARNING:  Java mode has to be treated with care.  Some algorithms may be
            // sensitive to flush to zero and may need proper IEEE-754 denormal handling.
            TurnJavaModeOffOnG4( );
            
            StartClock ( );
            for ( i = 0; i < MAX_LOOP_NUM; i++ )
            	{
                cblas_sscal ( N, alpha, X, 1 );  
                }
            StopClock ( &time );
            
            // Restore Java mode.
            RestoreJavaModeOnG4();
            
            // Measure and take off the calling overhead of 'cblas_sscal()'        
            StartClock();
            for ( i = 0; i < MAX_LOOP_NUM; i++ )
            	{
                Dummy_cblas_sscal ( N, alpha, X, 1 );
                }
            StopClock ( &overheadTime );
            
            time -= overheadTime;   
            time /= MAX_LOOP_NUM;
            
            printf ( "\nTime for performing 'cblas_sscal()' for 1000000 times is %4.4f µsecs.", time );
            
       	}

    // deallocate space
    free ( X );
}

void Dummy_cblas_sscal ( int N, float alpha, const float* X, int incX ){
    #pragma unused( N )
    #pragma unused( X )
    #pragma unused( alpha )
    incX = 1;
    }


