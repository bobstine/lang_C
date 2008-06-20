/*******************************************************************************
*                                                                              *
*     File Name: sdot.c                                                        *
*                                                                              *
*     This sample illustrates the usage of 'cblas_sdot()' function and do      *
*     timing for 1000000 times executions.                                     *
*                                                                              *
*     Copyright © 2000-2002 Apple Computer, Inc.  All rights reserved.         *
*                                                                              *
*******************************************************************************/

// #define  _TEST_GSL_

#include <Carbon/Carbon.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <CoreServices/CoreServices.h>

#ifndef _TEST_GSL_
#include <vecLib/vBLAS.h>
#include "main.h"
#endif

#ifdef _TEST_GSL_

void TurnJavaModeOffOnG4(void);  // otherwise in main.h
void RestoreJavaModeOnG4(void);
void StartClock( void );
void StopClock( float *call_time );

#include <gsl/gsl_errno.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_sf_erf.h>
#define  GSL_RANGE_CHECK_OFF

#endif

#include "JavaMode.h"


#define MAX_LOOP_NUM       1000000  // Number of iterations used in the timing loop
#define kHasAltiVecMask    ( 1 << gestaltPowerPCHasVectorInstructions )  // used in looking for a g4


void Dummy_cblas_sdot ( int N, const float* X, int incX, const float* Y, int incY );

float 
dot(int n, const float *x, const float *y)
{
  float dp;
  int i;
  dp = 0.0;
  for (i=0; i<n; ++i)
  { dp += x[i] * y[i];
  }
  return dp;
}

float 
dot2(int n, const float *x, const float *y)
{
  float dp;
  dp = 0.0;
  while (n--)
  { dp += *x++ * *y++;
  }
  return dp;
}


double 
ddot(int n, const double *x, const double *y)
{
  double dp;
  dp = 0.0;
  while (n--)
  { dp += *x++ * *y++;
  }
  return dp;
}



void RunSDOTSample ( void ) {
  float *X, *Y; 
  double *dX, *dY;
  int N, i;
  float resultFloat;
  double resultDouble;
#ifdef _TEST_GSL_
  gsl_vector *gX, *gY;
#endif  
    
    // Initialize the inputs and allocate space
    N = 5000;    
    X = ( float* ) malloc ( sizeof ( float ) * N );
    Y = ( float* ) malloc ( sizeof ( float ) * N );

    // Initialize double inputs and allocate space
    dX = ( double* ) malloc ( sizeof ( double ) * N );
    dY = ( double* ) malloc ( sizeof ( double ) * N );

#ifdef _TEST_GSL_    
     // Init the gsl vector  (gsl vectors are doubles)
    gX = gsl_vector_alloc( N );
    gY = gsl_vector_alloc( N );
#endif
    
    for ( i = 0; i < N; i++ ){
      X[i] = dX[i] = 1.0; 
      Y[i] = dY[i] = 2.0; 
#ifdef _TEST_GSL
      gsl_vector_set(gX,i,1.0);
      gsl_vector_set(gY,i,2.0);
#endif      
    }
    
    // test 'cblas_sdot()'.
    resultFloat = cblas_sdot ( N, X, 1, Y, 1 );
    printf("the result of cblas_sdot is %4.4f\n", resultFloat);
    
#ifndef _TEST_GSL_    
    // Timing section for 'cblas_sdot()'
      	{
            float time, overheadTime;
            // Turn Java mode off.  Otherwise, there is an extra cycle added to the vfpu.
            // WARNING:  Java mode has to be treated with care.  Some algorithms may be
            // sensitive to flush to zero and may need proper IEEE-754 denormal handling.
            TurnJavaModeOffOnG4( );
            StartClock ( );
            for ( i = 0; i < MAX_LOOP_NUM; i++ )
            	{
                resultFloat = cblas_sdot ( N, X, 1, Y, 1 );  
                }
            StopClock ( &time );
            // Restore Java mode.
            RestoreJavaModeOnG4();
            // Measure and take off the calling overhead of 'cblas_sdot()'        
            StartClock();
            for ( i = 0; i < MAX_LOOP_NUM; i++ )
            	{
                Dummy_cblas_sdot ( N, X, 1, Y, 1 );
                }
            StopClock ( &overheadTime );
            time -= overheadTime;   
            time /= MAX_LOOP_NUM;
            printf ( "\nTime for performing 'cblas_sdot()' for 1000000 times is %4.4f secs.", time );
       	}
        
        // Timing section for 'cblas_ddot()'
      	{
          float time, overheadTime;
          TurnJavaModeOffOnG4( );
          
          StartClock ( );
          for ( i = 0; i < MAX_LOOP_NUM; i++ )
          {
            resultDouble = cblas_ddot ( N, dX, 1, dY, 1 );  
          }
          StopClock ( &time );
          
          RestoreJavaModeOnG4();

          StartClock();
          for ( i = 0; i < MAX_LOOP_NUM; i++ )
          {
            Dummy_cblas_sdot ( N, X, 1, Y, 1 );
          }
          StopClock ( &overheadTime );
          time -= overheadTime;   
          time /= MAX_LOOP_NUM;
          printf ( "\nTime for performing 'cblas_ddot()' for 1000000 times is %4.4f.", time );
       	}
        
        // Timing section for 'cblas_ddot()'
      	{
          float time, overheadTime;
          // TurnJavaModeOffOnG4( );
          StartClock ( );
          for ( i = 0; i < MAX_LOOP_NUM; i++ )
          {
            resultDouble = cblas_ddot ( N, dX, 1, dY, 1 );  
          }
          StopClock ( &time );
          
          // RestoreJavaModeOnG4();
          
          StartClock();
          for ( i = 0; i < MAX_LOOP_NUM; i++ )
          {
            Dummy_cblas_sdot ( N, X, 1, Y, 1 );
          }
          StopClock ( &overheadTime );
          time -= overheadTime;   
          time /= MAX_LOOP_NUM;
          printf ( "\nTime for performing 'cblas_ddot()' without JavaMode switch for 1000000 times is %4.4f.", time );
       	}
#endif
        
        // Timing section without doing anything special at all
      	{
          float time, overheadTime;
          TurnJavaModeOffOnG4( );
          StartClock ( );
          for ( i = 0; i < MAX_LOOP_NUM; i++ )
          {
            resultFloat = dot ( N, X, Y );  
          }
          StopClock ( &time );
          // Restore Java mode.
          RestoreJavaModeOnG4();
          // Measure and take off the calling overhead         
          StartClock();
          for ( i = 0; i < MAX_LOOP_NUM; i++ )
          {
            Dummy_cblas_sdot ( N, X, 1, Y, 1 );
          }
          StopClock ( &overheadTime );
          time -= overheadTime;   
          time /= MAX_LOOP_NUM; 
          printf ( "\nTime for performing dot() for 1,000,000 times switching java mode is %4.4f.", time );
       	}
        
 
      	{
          float time, overheadTime;
           StartClock ( );
          for ( i = 0; i < MAX_LOOP_NUM; i++ )
          {
            resultFloat = dot2 ( N, X, Y );  
          }
          StopClock ( &time );
          // Measure and take off the calling overhead         
          StartClock();
          for ( i = 0; i < MAX_LOOP_NUM; i++ )
          {
            Dummy_cblas_sdot ( N, X, 1, Y, 1 );
          }
          StopClock ( &overheadTime );          
          time -= overheadTime;   
          time /= MAX_LOOP_NUM;
          printf ( "\nTime for performing dot2() for 1,000,000 times is %4.4f.", time );
       	}
        
        
      	{
          float time, overheadTime;
          StartClock ( );
          for ( i = 0; i < MAX_LOOP_NUM; i++ )
          {
            resultDouble= ddot ( N, dX, dY );  
          }
          StopClock ( &time );
          // Measure and take off the calling overhead         
          StartClock();
          for ( i = 0; i < MAX_LOOP_NUM; i++ )
          {
            Dummy_cblas_sdot ( N, X, 1, Y, 1 );
          }
          StopClock ( &overheadTime );          
          time -= overheadTime;   
          time /= MAX_LOOP_NUM;
          printf ( "\nTime for performing ddot() for 1,000,000 times is %4.4f.", time );
          
       	}
      
#ifdef _TEST_GSL_        
      	{
          float time, overheadTime;
          StartClock ( );
          for ( i = 0; i < MAX_LOOP_NUM; i++ )
          {
            gsl_blas_ddot ( gX, gY, &resultDouble );  
          }
          StopClock ( &time );
          // Measure and take off the calling overhead         
          StartClock();
          for ( i = 0; i < MAX_LOOP_NUM; i++ )
          {
            Dummy_cblas_sdot ( N, X, 1, Y, 1 );
          }
          StopClock ( &overheadTime );          
          time -= overheadTime;   
          time /= MAX_LOOP_NUM;
          printf ( "\nTime for performing gsl_vector_ddot() for 1,000,000 times is %4.4f.", time );
          
          gsl_vector_free(gX);
          gsl_vector_free(gY);
       	}
#endif        
        
    //deallocate space
    free (  X );
    free (  Y );
    free ( dX );
    free ( dY );
}


void Dummy_cblas_sdot ( int N, const float* X, int incX, const float* Y, int incY ){
    #pragma unused( N )
    #pragma unused( X )
    #pragma unused( Y )
    incX = 1;
    incY = 1;
    }
