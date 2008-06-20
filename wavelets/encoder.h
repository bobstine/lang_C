// $Id: encoder.h,v 1.8 2000/04/25 15:35:55 bob Exp $-*- c++ -*-
#ifndef _encoder_
#define _encoder_

#include <vector.h>

// Key function is the 'friend' encode_vector, and all input coefs
// are assumed to be on a z-score scale.

class Encoder
{
public:
  ~Encoder();
  Encoder(double prob0);

  void print();
  
  // find coded estimate and loop index
  pair<double,int> encode_as_pair (const double coef) const;

  // encode an estimate
  double operator()(const double coef) const;

  // Find the best encoder for a vector of estimates
  friend vector<double>
  encode_vector (const vector<double> coefs);

  // find bits required to code a vector of estimates
  double bits_to_code( const        double   coef ) const;
  double bits_to_code( const vector<double> &coef ) const;

private:
  double mProb0;
  double mLogP0, mLogPNZ;                           // log probs for zero, not zero
  vector < double > mLoopBaseBits;                  // bits at base of each loop
  vector < pair<double,double> > mLoopCenterLimit;  // loop center z and boundary
  pair<double,double> mLastLoop;                    // last loop

  // private empty constructor
  Encoder();
  
  // initializer functions
  double loop_base_bits(int j) const;
  vector < double > loop_base_bits() const;
  vector < pair<double,double> > loop_center_limit() const;
  
  // shift of z to right for monotone code
  double shift_z (double len0, double len1) const;

  // intersection of loops
  double boundary_z (double deltaZ, double deltaBits) const;
};

  
#endif

//////////////////////// eof ///////////////////////////////




