/*
    File: main.c
    
    Description:
        main driver routines for the vBLAS AltiVec samples.
    
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


#include <CoreServices/CoreServices.h>
#include <vecLib/vDSP.h>
#include <sys/param.h>
#include <sys/sysctl.h>


#include "main.h"
#include "JavaMode.h"
#include <StdIO.h>


Boolean HasAltiVec(void) {
	int mib[2], gHasAltivec;
	size_t len;

	mib[0] = CTL_HW;
	mib[1] = HW_VECTORUNIT;
	len = sizeof(gHasAltivec);
	sysctl(mib, 2, &gHasAltivec, &len, NULL, 0);
	return (gHasAltivec != 0);
}


void TurnJavaModeOffOnG4(void) {
      if ( HasAltiVec ( ) )
		TurnJavaModeOff();
}

void RestoreJavaModeOnG4(void)  {
      if ( HasAltiVec ( ) )
		RestoreJavaMode();
}


	/* globals used for caching the time */
static UnsignedWide startTime, endTime;

void StartClock ( void ) {
      Microseconds( &startTime );
}


void StopClock( float *call_time ) {
      Microseconds( &endTime );
      
      if ( endTime.hi == startTime.hi )
            *call_time = ( float ) ( endTime.lo - startTime.lo );
      else
            *call_time = -1.0f;
}



int main(void) {

    printf("\n\n   BEGIN RunSDOTSample() defined in sdot.c\n\n");
    RunSDOTSample();
    printf("\n\n   END RunSDOTSample()\n\n\n");
    
    return 0;
   
    printf("\n\n   BEGIN RunSNRM2Sample() defined in snrm2.c\n\n");
    RunSNRM2Sample();
    printf("\n\n   END RunSNRM2Sample()\n\n\n");
    
    printf("\n\n   BEGIN RunSASUMSample() defined in sasum.c\n\n");
    RunSASUMSample();
    printf("\n\n   END RunSASUMSample()\n\n\n");

    printf("\n\n   BEGIN RunISAMAXSample() defined in isamax.c\n\n");
    RunISAMAXSample();
    printf("\n\n   END RunISAMAXSample()\n\n\n");

    printf("\n\n   BEGIN RunSSWAPSample() defined in sswap.c\n\n");
    RunSSWAPSample();
    printf("\n\n   END RunSSWAPSample()\n\n\n");

    printf("\n\n   BEGIN RunSCOPYSample() defined in scopy.c\n\n");
    RunSCOPYSample();
    printf("\n\n   END RunSCOPYSample()\n\n\n");

    printf("\n\n   BEGIN RunSAXPYSample() defined in saxpy.c\n\n");
    RunSAXPYSample();
    printf("\n\n   END RunSAXPYSample()\n\n\n");

    printf("\n\n   BEGIN RunSROTSample() defined in srot.c\n\n");
    RunSROTSample();
    printf("\n\n   END RunSROTSample()\n\n\n");

    printf("\n\n   BEGIN RunSSCALSample() defined in sscal.c\n\n");
    RunSSCALSample();
    printf("\n\n   END RunSSCALSample()\n\n\n");

    printf("\n\n   BEGIN RunSGEMVSample() defined in sgemv.c\n\n");
    RunSGEMVSample();
    printf("\n\n   END RunSGEMVSample()\n\n\n");

    printf("\n\n   BEGIN RunSGEMMSample() defined in sgemm.c\n\n");
    RunSGEMMSample();
    printf("\n\n   END RunSGEMMSample()\n\n\n");

    return 0;

}