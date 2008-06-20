// $Id: encoder.cc,v 1.12 2000/04/25 17:30:33 bob Exp $-*- c++ -*-

#include <iostream.h>
#include <vector.h>
#include <algo.h>

#include <assert.h>
#include <math.h>

// debugging only
#include "utils.h"

#include "encoder.h"
#include "coding.h"


///////////////////////  Useful constants  ///////////////////////////////

// divisor for the loopie
static double const quadk = 2.0 * log(2.0);


///////////////////////  Encode a vector /////////////////////////////////

// This subroutine generates the probabilities used in the search for
// the best loopie code.

vector<double>
prob_vector (int nProbs)
{
  vector<double> result(nProbs,0.0);
  double scale = 1.0/((double) nProbs-1);

  result[0] = scale * scale;
  for (int i=1; i<nProbs-1; ++i)
    result[i] = scale * (double) i;
  result[nProbs-1] = 1.0 - result[0];
  return result;
}


vector<double>
encode_vector (const vector<double> coefs)
{
  int p = coefs.size();

  // define the probabilities for coding a coefficient
  vector<double> zeroProbGrid(prob_vector(p+1));

  // initialize length for starting search
  double minBits = 10000000000.0, bits = 0.0;  // how to set a max pos value ???
  double minProbZero = -2.0;
  Encoder minCoder;    // empty encoder via friend status, but dies if minCoder()???
  
  // search for best coding probability from offered grid
  for(int j=0; j < (int)zeroProbGrid.size(); ++j)
  {
    Encoder coder(zeroProbGrid[j]);  
    bits = cauchy_bit_length(j) + coder.bits_to_code(coefs);
    // cout << "        bits " << bits << " @ p(0) = " << zeroProbGrid[j] << endl;
    if (bits < minBits)
    {
      minBits = bits;
      minProbZero = zeroProbGrid[j];
      minCoder = coder;
    }
  }
  // cout << "Optimal bits " << minBits << " @ p(0) = " << minProbZero << endl;
  vector<double> result (p);
  transform(coefs.begin(), coefs.end(), result.begin(), minCoder);
  return result;
}



////////////////////////   Encoder Object   ////////////////////////////////

Encoder::~Encoder()
{
}

Encoder::Encoder():
  mProb0(0.0),
  mLogP0 (0.0),
  mLogPNZ(0.0)
{
}

Encoder::Encoder(double prob0):
  mProb0(prob0),
  mLogP0 (- log_2(prob0)),
  mLogPNZ(- log_2(1.0-prob0)),
  mLoopBaseBits(loop_base_bits()),
  mLoopCenterLimit(loop_center_limit()),
  mLastLoop(mLoopCenterLimit.back())
{
  assert(prob0 > 0.0);        // cannot have zero prob of coding
  assert((1.0-prob0) < 1.0);  // cannot code them all
}

void
Encoder::print()
{
  cout << endl << "-----------------------" << endl;
  cout << "Monotone loopie encoder:" << endl;
  cout << "  P(zero) = "  << mProb0
       << "   Table uses " << mLoopCenterLimit.size() << " loops with zero ";
  if (mLoopCenterLimit[0].second < 0.0)
    cout << "shadowed and not coded." << endl << endl;
  else
    cout << " boundary at z = "
	 << mLoopCenterLimit[0].second << endl << endl;
}

/////////////////////  initialization code  ///////////////////////////

static int const nLoops = 25;

double
Encoder::shift_z (double len0, double len1) const
{
  double diff;
  
  assert(len1>=len0);
  diff = quadk * (len1-len0);
  if (diff <= 1.0)
    return 1.0;
  else
    return sqrt(diff);
}

double
Encoder::boundary_z (double deltaZ, double deltaBits) const
{
  return (quadk * deltaBits + deltaZ * deltaZ)/(2.0 * deltaZ);
}

double
Encoder::loop_base_bits(int j) const
{
  assert (j > 0);
  // sign + (not zero bit) + universal
  return 1 + mLogPNZ + monotone_universal_length(j);
}

vector <double>
Encoder::loop_base_bits() const
{
  vector<double> result (nLoops+1);   // room for comparison for last

  result[0] = mLogP0;
  for (int j=1; j<nLoops+1; ++j)
  {
    result[j] = loop_base_bits(j);
  }
  // cout << "Base bits: " << endl << result << endl;
  return result;
}

vector < pair<double,double> >
Encoder::loop_center_limit() const
{
  vector< pair<double,double> > result;
  double boundary, shift, codedZ;
  int j;

  if (mLoopBaseBits[0] <= mLoopBaseBits[1])  // "normal" case
  {
    boundary = shift_z(mLoopBaseBits[0],mLoopBaseBits[1]);
    result.push_back(make_pair(0.0, boundary));
  }
  else // zero is shadowed
  {
    boundary = 0.5;
    result.push_back(make_pair(0.0, -2.0));   // nothing will be coded as zero
  }
  // start loopie for z=1 at boundary for z=0
  for(j=1, codedZ=boundary; j<nLoops; ++j)
  { 
    shift = shift_z(mLoopBaseBits[j],mLoopBaseBits[j+1]);
    if (shift > 1.0)
    {
      boundary = codedZ + shift;
      result.push_back(make_pair(codedZ, boundary));
      codedZ = boundary;
    }
    else // in steady state and want spacing of one SE
    {
      boundary = codedZ + boundary_z(1.0, mLoopBaseBits[j+1]-mLoopBaseBits[j]);
      result.push_back(make_pair(codedZ, boundary));
      codedZ += 1.0;
    }
  }
  // cout << endl << "Center/Limit table completed as " << endl << result;
  return result;
}



/////////////////////  coding functions  //////////////////////////////////

double
Encoder::operator()(const double coef) const
{
  return encode_as_pair(coef).first;
}  

pair<double,int>
Encoder::encode_as_pair(const double coef) const
{
  double absCoef = fabs(coef);
  double sign;
  
  if (coef < 0.0)
    sign = -1.0;
  else
    sign = 1.0;
  if (absCoef > mLastLoop.second) // out of table
  {
    int diff = (int) floor(0.5 + absCoef - mLastLoop.first);
    return (make_pair(sign*(mLastLoop.first+diff), nLoops-1+diff));
  }
  else
  {
    int j = 0;
    while (absCoef > mLoopCenterLimit[j].second) ++j;
    return (make_pair(sign*mLoopCenterLimit[j].first, j));
  }
}


double
Encoder::bits_to_code( const double coef ) const
{
  double absCoef = fabs(coef);
  double codedZ, loopBits;

  if (absCoef > mLastLoop.second) // out of table
  {
    int diff = (int) floor(0.5 + absCoef - mLastLoop.first);
    codedZ = mLastLoop.first + diff;
    loopBits = loop_base_bits(nLoops-1 + diff);
  }
  else
  {
    int j = 0;
    while (absCoef > mLoopCenterLimit[j].second) ++j;
    codedZ = mLoopCenterLimit[j].first;
    loopBits = mLoopBaseBits[j];
  }
  /*
    cout << "Bits->  RE = " << codedZ << "-" << absCoef
       << " +  loop " << loopBits << endl;
  */
  return relative_entropy(codedZ, absCoef) + loopBits;
}



namespace
{
  class Accum_Bits
  {
  public:
    Accum_Bits(const Encoder &encoder):
      mEncoder(encoder)
      {
      }
    double operator()(double sum, double coef) const
      {
	return (sum + mEncoder.bits_to_code(coef));
      }
  private:
    Encoder mEncoder;
  };
}

double
Encoder::bits_to_code( const vector<double> &coef ) const
{
  return accumulate( coef.begin(), coef.end(), 0.0, Accum_Bits(*this) );
}
  


///////////////////////  EOF  ////////////////////////////////////
