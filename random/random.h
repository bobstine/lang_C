/* $Id: random.h,v 1.4 2007/12/13 23:30:01 bob Exp $

   Random number generators

    1 Jan 02 ... Templates
   19 Dec 01 ... Use C++ classes.
   
*/

#ifndef _RANDOM_H_
#define _RANDOM_H_

////////////////////////////////////////////////////////////////////////////////

#include <utility>
#include <iostream>
#include <string>
#include <math.h>

//  Note: all initialize seed from system clock if seed is zero.

class UniformGenerator {
 public:
  virtual ~UniformGenerator() { };
  virtual double operator()(void) = 0;
  virtual void write_to (std::ostream &) const = 0;
};

class BuiltinGenerator: public UniformGenerator {

 public:
  BuiltinGenerator (unsigned long seed);

  double operator()(void);
  void write_to (std::ostream& output) const { output << "Built-in) " ; }
};


class MarsagliaGenerator: public UniformGenerator {
  long mState[24];
 public:
  MarsagliaGenerator (unsigned long seed);

  double operator()(void);
  void write_to (std::ostream& output) const;
};


class SuperDuperGenerator:public UniformGenerator {
  long mState[6];
 public:
  SuperDuperGenerator (unsigned long seed);

  double operator()(void);
  void write_to (std::ostream& output) const;
};

class WichmanHillGenerator: public UniformGenerator {
  long mState[4];
 public:
  WichmanHillGenerator (unsigned long seed);

  double operator()(void);
  void write_to (std::ostream& output) const;
};


class TauswortheGenerator: public UniformGenerator {
  long mState[4];
 public:
  TauswortheGenerator (unsigned long seed);

  double operator()(void);
  void write_to (std::ostream& output) const;
};


inline std::ostream&
operator<<(std::ostream &output, const UniformGenerator& gen)
{ output << "Uniform("; gen.write_to(output); return output; }

////////////////////////////////////////////////////////////////////////////////
  
class RandomGenerator {
  bool mNeedToFree;
  UniformGenerator* pUniformGenerator;
 public:
  ~RandomGenerator() { if(mNeedToFree) delete pUniformGenerator; }
  RandomGenerator (unsigned long seed=0)
    : mNeedToFree(true), pUniformGenerator(new MarsagliaGenerator(seed)) { }
  RandomGenerator (UniformGenerator* gen)
    : mNeedToFree(false), pUniformGenerator(gen) { }
  
  void write_to (std::ostream& output) const
    { output << "Random generator with base " << *pUniformGenerator; }

  double uniform(void) { return pUniformGenerator->operator()(); }
   
  double normal(void);
  double cauchy(void);
  double gamma(double alpha);

  double chi_square(int df)
    { return 2.0 * gamma((double)df/2.0) ; }
  double t(int df)
    { return normal()/sqrt(chi_square(df)); }
  double F(int nDF, int dDF)
    { return (dDF * chi_square(nDF)) / (nDF * chi_square(dDF)); }
  double beta(double alpha, double beta)
    { double x(gamma(alpha)); return x/(x+gamma(beta)); }
  
};

inline std::ostream&
operator<< (std::ostream& output, const RandomGenerator& gen)
{
  gen.write_to(output);
  return output;
}

#endif
