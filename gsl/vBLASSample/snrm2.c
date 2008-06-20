/*******************************************************************************
*                                                                              *
*     File Name: snrm2.c                                                       *
*                                                                              *
*     This sample illustrates the usage of 'cblas_snrm2()' function and do     *
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

void Dummy_cblas_snrm2 ( int N, const float* X, int incX );


void RunSNRM2Sample ( void ) {
    float *X; 
    int N, i;
    float resultFloat;
    
    // Initialize the inputs and allocate space
    N = 10;    
    X = ( float* ) malloc ( sizeof ( float ) * N );
    
    for ( i = 0; i < N; i++ ){
        X[i] = 2.0;
        }
    
    // test 'cblas_snrm2()'.
    resultFloat = cblas_snrm2 ( N, X, 1 );
    printf("the result of cblas_snrm2 is %4.4f\n", resultFloat);
    
     // Timing section for 'cblas_snrm2()'
      	{
            float time, overheadTime;
      
            // Turn Java mode off.  Otherwise, there is an extra cycle added to the vfpu.
            // WARNING:  Java mode has to be treated with care.  Some algorithms may be
            // sensitive to flush to zero and may need proper IEEE-754 denormal handling.
            TurnJavaModeOffOnG4( );
            
            StartClock ( );
            for ( i = 0; i < MAX_LOOP_NUM; i++ )
            	{
                resultFloat = cblas_snrm2 ( N, X, 1 );  
                }
            StopClock ( &time );
            
            // Restore Java mode.
            RestoreJavaModeOnG4();
            
            // Measure and take off the calling overhead of 'cblas_snrm2()'        
            StartClock();
            for ( i = 0; i < MAX_LOOP_NUM; i++ )
            	{
                Dummy_cblas_snrm2 ( N, X, 1 );
                }
            StopClock ( &overheadTime );
            
            time -= overheadTime;   
            time /= MAX_LOOP_NUM;
            
            printf ( "\nTime for performing 'cblas_snrm2()' for 1000000 times is %4.4f µsecs.", time );
            
       	}
   
    //deallocate space
    free ( X );

}

void Dummy_cblas_snrm2 ( int N, const float* X, int incX ){
    #pragma unused( N )
    #pragma unused( X )
    incX = 1;
    }
