
#include <stdio.h>
#include <gsl/gsl_sf_erf.h>

int
main (void)
{
  double z;
  int i;

    for (i = 0; i < 5; i++)
      {
	z = (double) i;
	printf("P(z=%d) = %+.18f %+.18f \n", i, gsl_sf_erf (z), gsl_sf_erf_Q (z) );
      }

    return 0;
}
