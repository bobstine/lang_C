/*******************************************************************************
*                                                                              *
*     File Name: sgemm.c                                                       *
*                                                                              *
*     This sample illustrates the usage of 'cblas_sgemm()' function and do     *
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

void Dummy_cblas_sgemm ( CBLAS_ORDER order, CBLAS_TRANSPOSE transA, CBLAS_TRANSPOSE transB, int M, int N, int K, float alpha, const float* A, int lda, const float* B, int ldb, float beta, float* C, int ldc);

void RunSGEMMSample ( void ) {
    float *A, *B, *C; 
    int i;
    float alpha, beta, value;
        
    //////////////////////////////////////////////////
    // Level 3 single precision functions testing.
    //////////////////////////////////////////////////

    // Initialize the inputs and allocate space
    
    A = ( float * ) malloc ( sizeof ( float ) * 25 );
    B = ( float * ) malloc ( sizeof ( float ) * 25 );
    C = ( float * ) malloc ( sizeof ( float ) * 25 );
    value = 0.0;
    for ( i = 0; i < 25; i++ ){
        A[i] = value;
        B[i] = value;
        C[i] = value;
        value += 1.0;
        }
    
    // Check values of A
    printf("Value of matrix A is :\n");
    for ( i = 0; i < 25; i++ ){
        if ( i % 5 == 0 ) {
            printf("\n");
            }
        printf("%8.4f ", A[i]);
        }
    printf("\n\n");

    // Check values of B
    printf("Value of matrix B is :\n");
    for ( i = 0; i < 25; i++ ){
        if ( i % 5 == 0 ) {
            printf("\n");
            }
        printf("%8.4f ", B[i]);
        }
    printf("\n\n");
    
    // Check values of C
    printf("Value of matrix C is :\n");
    for ( i = 0; i < 25; i++ ){
        if ( i % 5 == 0 ) {
            printf("\n");
            }
        printf("%8.4f ", C[i]);
        }
    printf("\n\n");

    // initialize two constants.
    alpha = 1.0;
    beta = 2.0;
    printf("alpha = %4.4f, beta = %4.4f\n", alpha, beta);
    
    //////////////////////////////////////////////////////////////////////////////////////////////////
    //
    //   The format of inputs are the followings :
    //
    //   A = B = C = |  0  1  2  3  4 |
    //               |  5  6  7  8  9 |
    //               | 10 11 12 13 14 |
    //               | 15 16 17 18 19 |
    //               | 20 21 22 23 24 |
    //
    //  Since Level 3 single precision function takes 3 matrices comparing to Level 2 function that 
    //  takes 1 matrix, the kinds of arguments are different from Level 2 function. 
    //  1st argument is same as Level2.
    //  2nd and 3rd are to indicate if you want 'cblas_sgemm()' to use the transpose of A or B.
    //  4th, 5th, and 6th are the dimensions for the matrices A, B, C. The dimension of A is (4th X 5th).
    //  The dimension of B is (5th X 6th). The dimension of C is (4th X 6th). 
    //  9th, 11th, and 13th are leading dimensions of each matrix.
    //  Please refer to the file, 'sgemv.c' for better explanation of transpose argument and leading
    //  dimention argument.
    //
    ///////////////////////////////////////////////////////////////////////////////////////////////////
    
    // sample call to 'cblas_sgemm()'.
    // In case of row major, no transpose for A, B.
    cblas_sgemm ( CblasRowMajor, CblasNoTrans, CblasNoTrans, 5, 5, 5, alpha, A, 5, B, 5, beta, C, 5 );
    
    // Check the result.
    printf("\n---- RESULT ----\n");
    printf("Value of matrix C after 'cblas_sgemm()' is :\n");
    for ( i = 0; i < 25; i++ ){
        if ( i % 5 == 0 ) {
            printf("\n");
            }
        printf("%10.4f ", C[i]);
        }
    printf("\n\n");
    
    // Timing section for 'cblas_sgemm()'
      	{
            float time, overheadTime;
      
            // Turn Java mode off.  Otherwise, there is an extra cycle added to the vfpu.
            // WARNING:  Java mode has to be treated with care.  Some algorithms may be
            // sensitive to flush to zero and may need proper IEEE-754 denormal handling.
            TurnJavaModeOffOnG4( );
            
            StartClock ( );
            for ( i = 0; i < MAX_LOOP_NUM; i++ )
            	{
                cblas_sgemm ( CblasRowMajor, CblasNoTrans, CblasNoTrans, 5, 5, 5, alpha, A, 5, B, 5, beta, C, 5 );  
                }
            StopClock ( &time );
            
            // Restore Java mode.
            RestoreJavaModeOnG4();
            
            // Measure and take off the calling overhead of 'cblas_sgemm()'        
            StartClock();
            for ( i = 0; i < MAX_LOOP_NUM; i++ )
            	{
                Dummy_cblas_sgemm ( CblasRowMajor, CblasNoTrans, CblasNoTrans, 5, 5, 5, alpha, A, 5, B, 5, beta, C, 5 );
                }
            StopClock ( &overheadTime );
            
            time -= overheadTime;   
            time /= MAX_LOOP_NUM;
            
            printf ( "\nTime for performing 'cblas_sgemm()' 1000000 times is %4.4f µsecs.", time );
            
       	}

    
    // deallocate space
    free ( A );
    free ( B );
    free ( C );
    
}

void Dummy_cblas_sgemm ( CBLAS_ORDER order, CBLAS_TRANSPOSE transA, CBLAS_TRANSPOSE transB, int M, int N, int K, float alpha, const float* A, int lda, const float* B, int ldb, float beta, float* C, int ldc){
    #pragma unused( order )
    #pragma unused( transA )
    #pragma unused( transB )
    #pragma unused( M )
    #pragma unused( N )
    #pragma unused( K )
    #pragma unused( alpha )
    #pragma unused( A )
    #pragma unused( lda )
    #pragma unused( B )
    #pragma unused( ldb )
    #pragma unused( beta )
    #pragma unused( C )
    #pragma unused( ldc )
    }