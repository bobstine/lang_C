/*******************************************************************************
*                                                                              *
*     File Name: sgemv.c                                                       *
*                                                                              *
*     This sample illustrates the usage of 'cblas_sgemv()' function and do     *
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

void Dummy_cblas_sgemv ( CBLAS_ORDER order, CBLAS_TRANSPOSE transA, int M, int N, float alpha, const float* A, int lda, const float* X, int incX, float beta, float* Y, int incY );


void RunSGEMVSample ( void ) {
    float *X, *Y, *A, *ptrM; 
    int N, i, j;
    float alpha, beta, value;
    float M[5][5];    // unused input
    
    //////////////////////////////////////////////////
    // Level 2 single precision functions testing.
    //////////////////////////////////////////////////

    // Initialize the inputs and allocate space
    N = 5;
    X = ( float* ) malloc ( sizeof ( float ) * N );
    Y = ( float* ) malloc ( sizeof ( float ) * N );

    for ( i = 0; i < N; i++ ){
        X[i] = 1.0;
        Y[i] = 2.0;
        }
        

    A = ( float * ) malloc ( sizeof ( float ) * 25 );
    for ( i = 0; i < 25; i++ ){
        A[i] = (float)i;
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
    
    // Check values of X
    printf("Value of X is :\n");
    for ( i = 0; i < N; i++ ){
        printf("%8.4f ", X[i]);
        }
    printf("\n\n");
    
    // Check values of Y
    printf("Value of Y is :\n");
    for ( i = 0; i < N; i++ ){
        printf("%8.4f ", Y[i]);
        }
    printf("\n\n");

   
    /////////////////////////////////////////////////////////////////////////////////////
    // This portion initializes the input matrix M, but I am not using this matrix as an
    // argument for 'cblas_sgemv()'. However, if you want to use 2 dimensional array as
    // input to 'cblas_sgemv()', then you can allocate it like 'M' here, set a pointer
    // to the first element, and pass it to 'cblas_sgemv()'. Notice that you can't just 
    // pass 'M' since the type of 'M' is 'float **'. 'cblas_sgemv()' want 'float *'. If
    // you do that, you will get the warning from compiler.   
    ////////////////////////////////////////////////////////////////////////////////////
    value = 0.0;
    for ( i = 0; i < 5; i++ ){
        for ( j = 0; j < 5; j++ ){
            M[i][j] = value;
            value += 1.0;
            }
        }

    ptrM = &M[0][0];
    for ( i = 0; i < 25; i++ ){
        //printf("M[%d] = %4.4f\n", i, *ptrM);
        ptrM++;
        }
    ptrM = &M[0][0];
    ///////////////////////////////////////////////////////////////////////////////////////
    // End of preparing unused input.
    ///////////////////////////////////////////////////////////////////////////////////////
    
    // initialize two constants.
    alpha = 1.0;
    beta = 1.0;
    printf("alpha = %4.4f, beta = %4.4f\n", alpha, beta); 
    
    ////////////////////////////////////////////////////////////////////////////////////////
    // The format of the inputs are followings:
    // 
    //    A = |  0  1  2  3  4 |
    //        |  5  6  7  8  9 |
    //        | 10 11 12 13 14 |
    //        | 15 16 17 18 19 |
    //        | 20 21 22 23 24 |
    //
    //    X = | 1 |           Y = | 2 |
    //        | 1 |               | 2 |
    //        | 1 |               | 2 |
    //        | 1 |               | 2 |
    //        | 1 |               | 2 |
    //
    //   To describe some of the arguments that might cause little confusion :
    //
    //   The 1st argument, 'CBLAS_ORDER', specifies that the language you are using
    //   is row major or column major order. ex) 'C' is row major. So, you have to pass
    //   'CblasRowMajor' constant which is defined in 'vBLAS.h' as enum type. By mistake, 
    //   if you pass 'CblasColMajor', the 'cblas_sgemv()' routine will think this as column
    //   major and convert it to row major which results in using the transpose of the 
    //   original matrix.
    //   The 2nd argument, 'CBLAS_TRANSPOSE', specifies if you want 'cblas_sgemv()' to use
    //   the transpose or the original in its computation.
    //   The 3rd and 4th are just dimension of the matrix 'A', (3rd X 4th).
    //   The 6th argument is the matrix. Be careful that you shouldn't pass pointer to the 
    //   2 dimensional array since the type of this argument is 'const float *'. You have 
    //   to either allocate 1 dimensional array and pass pointer to it or allocate the storage
    //   2 dimensional array and pass the pointer to the first element, [0][0]-indexed one.
    //   If you just pass the pointer to 2 dimensional array, you will get warning from 
    //   compiler.
    //   The 7th argument is leading dimension of A which specifies invisible boundaries
    //   between the groups of elements sequentially ordered in the physical memory. So, in
    //   the row major order, this value indicates how many elements are in each row which is
    //   same number as the number column of that matrix. In column major order, it will be 
    //   number of rows.
    // 
    //////////////////////////////////////////////////////////////////////////////////////////// 
    
    // sample call to 'cblas_sgemv()'
    // In case of row major and no transpose to be used.
    // Dimension of A is 5X5. So, the leading dimension will be 5 which is number of columns in A.  
    cblas_sgemv ( CblasRowMajor, CblasNoTrans, 5, 5, alpha, A, 5, X, 1, beta, Y, 1 ); 
    
    // Check the result.
    printf("\n---- RESULT ----\n");
    printf("Value of Y after 'cblas_segmv()' is :\n");
    for ( i = 0; i < N; i++ ){
        printf("%8.4f ", Y[i]);
        }
    printf("\n\n");

    // Timing section for 'cblas_sgemv()'
      	{
            float time, overheadTime;
      
            // Turn Java mode off.  Otherwise, there is an extra cycle added to the vfpu.
            // WARNING:  Java mode has to be treated with care.  Some algorithms may be
            // sensitive to flush to zero and may need proper IEEE-754 denormal handling.
            TurnJavaModeOffOnG4( );
            
            StartClock ( );
            for ( i = 0; i < MAX_LOOP_NUM; i++ )
            	{
                cblas_sgemv ( CblasRowMajor, CblasNoTrans, 5, 5, alpha, A, 5, X, 1, beta, Y, 1 );  
                }
            StopClock ( &time );
            
            // Restore Java mode.
            RestoreJavaModeOnG4();
            
            // Measure and take off the calling overhead of 'cblas_sgemv()'        
            StartClock();
            for ( i = 0; i < MAX_LOOP_NUM; i++ )
            	{
                Dummy_cblas_sgemv ( CblasRowMajor, CblasNoTrans, 5, 5, alpha, A, 5, X, 1, beta, Y, 1 );
                }
            StopClock ( &overheadTime );
            
            time -= overheadTime;   
            time /= MAX_LOOP_NUM;
            
            printf ( "\nTime for performing 'cblas_sgemv()' 1000000 times is %4.4f µsecs.", time );
            
       	}


    // deallocate space
    free ( A );
    free ( X );
    free ( Y );

}

void Dummy_cblas_sgemv ( CBLAS_ORDER order, CBLAS_TRANSPOSE transA, int M, int N, float alpha, const float* A, int lda, const float* X, int incX, float beta, float* Y, int incY ){
    #pragma unused( order )
    #pragma unused( transA )
    #pragma unused( M )
    #pragma unused( N )
    #pragma unused( alpha )
    #pragma unused( A )
    #pragma unused( lda )
    #pragma unused( X )
    incX = 1;
    #pragma unused( Y )
    incY = 1;
    }


