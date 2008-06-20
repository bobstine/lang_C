/*
    File: main.h
    
    Description:
        main driver routine declarations for the vBLAS AltiVec samples.

    Copyright:
        © Copyright 2001-2002 Apple Computer, Inc. All rights reserved.
    
    Disclaimer:
        IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc.
        ("Apple") in consideration of your agreement to the following terms, and your
        use, installation, modification or redistribution of this Apple software
        constitutes acceptance of these terms.  If you do not agree with these terms,
        please do not use, install, modify or redistribute this Apple software.

        In consideration of your agreement to abide by the following terms, and subject
        to these terms, Apple grants you a personal, non-exclusive license, under Apple’s
        copyrights in this original Apple software (the "Apple Software"), to use,
        reproduce, modify and redistribute the Apple Software, with or without
        modifications, in source and/or binary forms; provided that if you redistribute
        the Apple Software in its entirety and without modifications, you must retain
        this notice and the following text and disclaimers in all such redistributions of
        the Apple Software.  Neither the name, trademarks, service marks or logos of
        Apple Computer, Inc. may be used to endorse or promote products derived from the
        Apple Software without specific prior written permission from Apple.  Except as
        expressly stated in this notice, no other rights or licenses, express or implied,
        are granted by Apple herein, including but not limited to any patent rights that
        may be infringed by your derivative works or by other works in which the Apple
        Software may be incorporated.

        The Apple Software is provided by Apple on an "AS IS" basis.  APPLE MAKES NO
        WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED
        WARRANTIES OF NON-INFRINGEMENT, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
        PURPOSE, REGARDING THE APPLE SOFTWARE OR ITS USE AND OPERATION ALONE OR IN
        COMBINATION WITH YOUR PRODUCTS.

        IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL OR
        CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
        GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
        ARISING IN ANY WAY OUT OF THE USE, REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION
        OF THE APPLE SOFTWARE, HOWEVER CAUSED AND WHETHER UNDER THEORY OF CONTRACT, TORT
        (INCLUDING NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN IF APPLE HAS BEEN
        ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    Change History (most recent first):
        Wed, Apr 10, 2001 -- created
*/


#ifndef __MAIN__
#define __MAIN__

#include <CoreServices/CoreServices.h>
#include <vecLib/vBLAS.h>


#ifdef __cplusplus
extern "C" {
#endif


	/* HasAltiVec returns true if the AltiVec is available for use */
Boolean HasAltiVec(void);



	/* TurnJavaModeOffOnG4 and RestoreJavaModeOnG4 can be used for
	turning off the processor's java mode on a G4.  These routines call through
	to the lower level TurnJavaModeOff and RestoreJavaMode routines, but only
	after testing for the presence of the altivec unit. */
void TurnJavaModeOffOnG4(void);
void RestoreJavaModeOnG4(void);


	/* routines used for timing.  To start the timer, call Start_Clock,
	and to stop the timer call Stop_Clock.  Stop_Clock will return
	the number of microseconds since Start_Clock was called, or
	-1.0f if that number cannot be calculated. */
void StartClock( void );
void StopClock( float *call_time );


        /* Run****Sample calls the level 1, 2, and 3 single precision functions
        named, '****', defined in the file '****'.c */
void RunSDOTSample(void);
void RunSNRM2Sample(void);
void RunSASUMSample(void);
void RunISAMAXSample(void);
void RunSSWAPSample(void);
void RunSCOPYSample(void);
void RunSAXPYSample(void);
void RunSROTSample(void);
void RunSSCALSample(void);
void RunSGEMVSample(void);
void RunSGEMMSample(void);
	
#ifdef __cplusplus
}
#endif

#endif
