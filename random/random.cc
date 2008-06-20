// $Id: random.cc,v 1.5 2007/11/13 00:21:15 bob Exp $

/* Based on code used to generate random values in XLisp-Stat            */
/* xlrand.c - random number generators                                   */
/* Copyright (c) 1989, by David Michael Betz.                            */


#include <iterator>

#include <time.h>
#include <math.h>

#include "random.h"


/*

0 -- The original XLISP-STAT generator, Marsaglia's portable generator
from CMLIB. This is a lagged Fibonacci generator.

1 -- L'Ecuyer's version of the Wichmann Hill generator, also used in
Bratley, Fox and Schrage, 2nd ed., in program UNIFL.  L'Ecuyer,
(1986), "Efficient and portable combined random number generators,"
Communications of the ACM 31, 742-749.

2 -- Marsaglia's Super-Duper, as used in S.

3 -- Combined Tausworthe generator. Tezuka and L'Ecuyer, (1991),
"Efficient and portable combined Tauseworthe random number
generators," ACM Transactions on Modeling and Computer Simulation 1,
99-112.

All four implementations should work on machines where longs are at
least 32 bits.

*/


#define DFLTGEN 1
#define MPFIX32 2147483647

#define HPART(x) (((unsigned long) (x)) >> 16)
#define LPART(x) (((unsigned long) (x)) & 0xFFFF)
#define SETULONG(x,h,l) ((x) = ((h) << 16) + (l))


static
unsigned long system_tick_count()
{
  std::clog << "Random generator set from system clock\n";
  return((unsigned long) time((time_t *) 0));  
}

///////// Marsaglia uniform  //////////////////////////

void
MarsagliaGenerator::write_to (std::ostream& output) const
{
  output << "Marsaglia) ";
  std::copy (mState, mState+24, std::ostream_iterator<long>(output, " "));
}

MarsagliaGenerator::MarsagliaGenerator (unsigned long seed)
{
  if (0 == seed) seed = system_tick_count();
  // original Marsaglia portable generator from CMLIB
  int MDIG = 32;
  int M1   = 20;
  int M2   = 21;
  int I    = 22;
  int J    = 23;
  long m1, m2, k0, k1, j0, j1;
  
  m1 = (1L << (MDIG - 2)) + ((1L << (MDIG - 2)) - 1);
  m2 = (1L << (MDIG / 2));
  
  long *d = mState;
  d[0]  = MPFIX32;
  d[1]  = 0;
  d[2]  = (long) seed;
  d[M1] = m1;
  d[M2] = m2;
  d[I]  = 7;
  d[J]  = 19;
  
  if (seed % 2 == 0) seed--;
  k0 = 9069 % m2;
  k1 = 9069 / m2;
  j0 = seed % m2;
  j1 = seed / m2;
  for (int i = 3; i <= 19; i++) {
    seed = j0 * k0;
    j1 = ((seed / m2) + j0 * k1 + j1 * k0) % (m2 / 2);
    j0 = seed % m2;
    d[i] = j0 + m2 * j1;
  }
}

double
MarsagliaGenerator::operator()(void)
{
  const int M1   = 20;
  const int I    = 22;
  const int J    = 23;
  long k, i, j, m1;

  long *d=mState;
  m1 = d[M1];
  i =  d[I];
  j =  d[J];
  
  k =  d[i] - d[j];
  if (k < 0) k = k + m1;
  d[j] = k;
  i = i - 1;
  if (i <= 2) i = 19;
  j = j - 1;
  if (j <= 2) j = 19;
  d[I] = i;
  d[J] = j;
  
  d[1] = k;
  return ((double) k) / m1;
}

/////////////////  Super Duper Generator  ////////////////////////////

void
SuperDuperGenerator::write_to (std::ostream& output) const
{
  output << "Super Duper) " ;
  for (int i=0; i<6; ++i)
    output << mState[i] << " ";
}

SuperDuperGenerator::SuperDuperGenerator(unsigned long seed)
{
  if (0 == seed) seed = system_tick_count();
  unsigned long Mask2 = 0x10000;
  unsigned long JC, JT;
  long *d = mState;
            
  JC = seed / (1L << 16);
  JT = seed % (1L << 16);
  if (JC <= 0 || JC >= Mask2) JC = 1;
  if (JT <= 0 || JT >= Mask2) JT = 1;
  if (JC % 2 == 0) JC--;
  
  d[0] = MPFIX32;
  d[1] = 0;
  d[2] = HPART(JC);
  d[3] = LPART(JC);
  d[4] = HPART(JT);
  d[5] = LPART(JT);
}

double
SuperDuperGenerator::operator()(void)
{
  const double Norm=4.656612873E-10;
  unsigned long JC, JT;
  long k;
  long *d = mState;
  
  SETULONG(JC, d[2], d[3]);
  SETULONG(JT, d[4], d[5]);
	
  JC = (JC * 69069) & 0xFFFFFFFF;	/* congruential part */
  JT ^= JT >> 15;				/* tausworthe part */
  JT ^= (JT << 17) & 0xFFFFFFFF;
  k = ((JT ^ JC) >> 1);
  
  d[1] = k;
  d[2] = HPART(JC);
  d[3] = LPART(JC);
  d[4] = HPART(JT);
  d[5] = LPART(JT);
  
  return k * Norm;
}


///////////////////  Wichman Hill  ////////////////////////////////

void
WichmanHillGenerator::write_to (std::ostream& output) const
{
  output << "Wichman-Hill) " ;
  for (int i=0; i<4; ++i)
    output << mState[i] << " ";
}

WichmanHillGenerator::WichmanHillGenerator(unsigned long seed)
{
  // L'Ecuyer's variant of Wichman-Hill 
  unsigned long Mask1=2147483562, Mask2=2147483563, Mask3 = 2147483399;
  unsigned long J2, J3;  // should this be unsigned, not in LT's code
  long *d = mState;

  J2 = seed / (1L << 16);
  J3 = seed % (1L << 16);
  if (J2 <= 0 || J2 >= Mask2) J2 = 1;
  if (J3 <= 0 || J3 >= Mask3) J3 = 1;
      
  d[0] = Mask1;
  d[1] = 0;
  d[2] = J2;
  d[3] = J3;
}

double
WichmanHillGenerator::operator()(void)
{
  static unsigned long Mask1=2147483562;
  static unsigned long Mask2=2147483563;
  static unsigned long Mask3 = 2147483399;
  static double Norm = 4.6566130573E-10;
  long k, J2, J3;
  long *d = mState;
  
  J2 = d[2];
  J3 = d[3];
	
  /* Get next term in 40014 * J2 MOD Mask2 */
  k = J2 / 53668;
  J2 = 40014 * (J2 - k * 53668) - k * 12211;
  if (J2 < 0) J2 = J2 + Mask2;
  
  /* Get next term in 40692 * J3 MOD Mask3 */
  k = J3 / 52774;
  J3 = 40692 * (J3 - k * 52774) - k * 3791;
  if (J3 < 0) J3 = J3 + Mask3;
  
  /* Set J1 = ((J3 + Mask1 - J2) MOD Mask1) + 1 */
  k = J2 - J3;
  if (k < 1) k = k + Mask1;
  
  /* Store results */
  d[1] = k;
  d[2] = J2;
  d[3] = J3;
  
  /* Put it on the interval (0,1) */
  return k * Norm;
}

////////////////////  Tausworthe  /////////////////////////////////

void
TauswortheGenerator::write_to (std::ostream& output) const
{
  output << "Tausworthe) ";
  for (int i=0; i<4; ++i)
    output << mState[i] << " ";
}

TauswortheGenerator::TauswortheGenerator (unsigned long seed)
{
  /* L'Ecuyer and Tezuka combined Tausworthe generator */

  unsigned long I1, I2, Mask1=2147483647, Mask2=536870911;
  long *d = mState;

  I1 = seed / (1L << 16);
  I2 = seed % (1L << 16);
  if (I1 <= 0 || I1 > Mask1) I1 = 1;
  if (I2 <= 0 || I2 > Mask2) I1 = 1;
      
  d[0] = Mask1;
  d[1] = 0;
  d[2] = I1;
  d[3] = I2;
}

double
TauswortheGenerator::operator()(void)
{
  long *d = mState;

  const long Q1=13, Q2=2, S1=17, S2=17, P1mS1=19, P2mS2=12, P1mP2=2;
  const unsigned long Mask1=2147483647, Mask2=536870911;
  const double Norm=4.656612873E-10;
  unsigned long I1, I2;
  unsigned long b;
	
  I1 = d[2];
  I2 = d[3];
  
  b=((I1 << Q1) ^ I1) & Mask1;
  I1 = ((I1 << S1) ^ (b >> P1mS1)) & Mask1;
  b = ((I2 << Q2) ^ I2) & Mask2;
  I2 = ((I2 << S2) ^ (b >> P2mS2)) & Mask2;
  b = I1 ^ (I2 << P1mP2);
  
  d[1] = b;
  d[2] = I1;
  d[3] = I2;
  
  return  b * Norm;
}

#define ROOT2OVERE 0.8577638849607068

double
RandomGenerator::normal(void)
{
  double x, y, u, u1, v;

  /* ratio of uniforms with linear pretest */
  do {
    u = (*pUniformGenerator)();
    u1 = (*pUniformGenerator)();
    v = ROOT2OVERE * (2 * u1 - 1);
    x = v / u;
    y = x * x / 4.0;
  } while(y > (1 - u) && y > - log(u));
  return(x);
}


double
RandomGenerator::cauchy(void)
{
  double u1, u2, v1, v2;
  /* ratio of uniforms on half disk */
  do {
    u1 = (*pUniformGenerator)();
    u2 = (*pUniformGenerator)();
    v1 = 2.0 * u1 - 1.0;
    v2 = u2;
  } while(v1 * v1 + v2 * v2 > 1.0);
  return(v1 / v2);
}

#define EXP1 2.718281828459045
#define TRUE 1
#define FALSE 0

double
RandomGenerator::gamma(double a)
{
  double x, u0, u1, u2, v, w, c, c1, c2, c3, c4, c5;
  int done;
  if (a < 1.0) {
    /* Ahrens and Dieter algorithm */
    done = FALSE;
    c = (a + EXP1) / EXP1;
    do {
      u0 = (*pUniformGenerator)();
      u1 = (*pUniformGenerator)();
      v = c * u0;
      if (v <= 1.0) {
	x = exp(log(v) / a);
	if (u1 <= exp(-x)) done = TRUE;
      }
      else {
	x = -log((c - v) / a);
	if (x > 0.0 && u1 < exp((a - 1.0) * log(x))) done = TRUE;
      }
    } while(! done);
  }
  else if (a == 1.0) x = -log((*pUniformGenerator)());
  else {
    /* Cheng and Feast algorithm */
    c1 = a - 1.0;
    c2 = (a - 1.0 / (6.0 * a)) / c1;
    c3 = 2.0 / c1;
    c4 = 2.0 / (a - 1.0) + 2.0;
    c5 = 1.0 / sqrt(a);
    do {
      do {
	u1 = (*pUniformGenerator)();
	u2 = (*pUniformGenerator)();
	if (a > 2.5) u1 = u2 + c5 * (1.0 - 1.86 * u1);
      } while (u1 <= 0.0 || u1 >= 1.0);
      w = c2 * u2 / u1;
    } while ((c3 * u1 + w + 1.0/w) > c4 && (c3 * log(u1) - log(w) + w) > 1.0);
    x = c1 * w;
  }
  return(x);
}

