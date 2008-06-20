/*******************************************************************************
*                                                                              *
*     File Name: sswap.c                                                       *
*                                                                              *
*     This sample illustrates the usage of 'cblas_sswap()' function and do     *
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

void Dummy_cblas_sswap ( int N, const float* X, int incX, const float* Y, int incY );


void RunSSWAPSample ( void ) {
    float *X, *Y; 
    int N, i;
    
    // Initialize the inputs and allocate space
    N = 10;    
    X = ( float* ) malloc ( sizeof ( float ) * N );
    Y = ( float* ) malloc ( sizeof ( float ) * N );

    for ( i = 0; i < N; i++ ){
        X[i] = 1.0;
        Y[i] = 2.0;
        }
        
    // testing 'cblas_sswap()'
    printf("before swap contents in X.\n");
    for ( i = 0; i < N; i++ ){
        printf("%4.4f ", X[i]);
        }
    printf("\n");
    
    printf("\nbefore swap contents in Y.\n");
    for ( i = 0; i < N; i++ ){
        printf("%4.4f ", Y[i]);
        }
    printf("\n");
    
    cblas_sswap ( N, X, 1, Y, 1 );
    
    printf("\nafter swap contents in X.\n");
    for ( i = 0; i < N; i++ ){
        printf("%4.4f ", X[i]);
        }
    printf("\n");
    
    printf("\nafter swap contents in Y.\n");
    for ( i = 0; i < N; i++ ){
        printf("%4.4f ", Y[i]);
        }
    printf("\n");

    // Timing section for 'cblas_sswap()'
      	{
            float time, overheadTime;
      
            // Turn Java mode off.  Otherwise, there is an extra cycle added to the vfpu.
            // WARNING:  Java mode has to be treated with care.  Some algorithms may be
            // sensitive to flush to zero and may need proper IEEE-754 denormal handling.
            TurnJavaModeOffOnG4( );
            
            StartClock ( );
            for ( i = 0; i < MAX_LOOP_NUM; i++ )
            	{
                cblas_sswap ( N, X, 1, Y, 1 );  
                }
            StopClock ( &time );
            
            // Restore Java mode.
            RestoreJavaModeOnG4();
            
            // Measure and take off the calling overhead of 'cblas_sswap()'        
            StartClock();
            for ( i = 0; i < MAX_LOOP_NUM; i++ )
            	{
                Dummy_cblas_sswap ( N, X, 1, Y, 1 );
                }
            StopClock ( &overheadTime );
            
            time -= overheadTime;   
            time /= MAX_LOOP_NUM;
            
            printf ( "\nTime for performing 'cblas_sswap()' for 1000000 times is %4.4f µsecs.", time );
            
       	}


    // deallocate space
    free ( X );
    free ( Y );
}

void Dummy_cblas_sswap ( int N, const float* X, int incX, const float* Y, int incY ){
    #pragma unused( N )
    #pragma unused( X )
    #pragma unused( Y )
    incX = 1;
    incY = 1;
    }


