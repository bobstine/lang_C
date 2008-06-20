// $Id: coding.cc,v 1.2 2000/04/21 16:41:28 bob Exp $-*- c++ -*-

#include <iostream.h>
#include <vector.h>
#include <algo.h>

#include <assert.h>
#include <math.h>

#include "utils.h"
#include "coding.h"

int binary_bit_length (int j)
{
  assert (j >= 0);
  if (j == 0)
    return 1;
  else
  {
    int result(0);
    while (j > 0)
    {
      ++result;
      j /= 2;
    }
    return result;
  }
}
vector<bool> binary_bits (int j)
{
  assert (j >= 0);
  
  vector<bool> result;

  if (j == 0)
  {
    result.push_back(0);
    return result;
  }
  else
  {
    while (j > 0)
    {
      result.push_back(j % 2);
      j /= 2;
    }
    reverse(result.begin(), result.end());
    return result;
  }
}


int signed_cauchy_bit_length (int j)
{
  if (j == 0)
    return cauchy_bit_length (j);
  return 1+cauchy_bit_length (abs(j));
}
int cauchy_bit_length(int j)
{
  assert (j >=0 );
  if (j == 0)
    return 1;
  else
    return (2*binary_bit_length(j));
}
vector<bool> cauchy_bits (int j)
{
  assert (j >= 0);

  if (j == 0)
    return binary_bits(0);
  else
  {
    vector<bool> result;
    while (j > 0)
    {
      result.push_back(1);
      result.push_back(j % 2);
      j /= 2;
    }
    result[0] = 0;  // mark end
    reverse(result.begin(), result.end());
    return result;
  }
}

int signed_geometric_bit_length (int j)
{
  if (j == 0)
    return geometric_bit_length(0);
  return 1+geometric_bit_length(abs(j));
}
int geometric_bit_length (int j)
{
  assert (j >= 0);

  return j+1;
}
vector<bool> geometric_bits (int j)
{
  assert (j >= 0);
  
  if (j == 0)
    return binary_bits(0);
  else
  {
    vector<bool> result;
    while (j > 0)
    {
      result.push_back(1);
      j -= 1;
    }
    result.push_back(0);  // mark end
    return result;
  }
}


int signed_universal_bit_length(int j)
{
  if (0 == j)
    return universal_bit_length (0);
  return 1+universal_bit_length(abs(j));
}
int universal_bit_length(int j)
{
  assert (j >= 0);

  if (j == 0)
    return 1;
  else
  {
    int result(1);
    int nBits(0);
    j += 1;
    while (j > 1)
    {
      result += nBits = binary_bit_length(j);
      j = nBits-1;
    }
    return result;
  }
}
vector<bool> universal_bits(int j)
{
  assert (j >= 0);

  if (j == 0)
    return binary_bits(0);
  else
  {
    vector<bool> result;
    result.push_back(0);  // terminator
    j += 1;
    while (j > 1)
    {
      vector<bool> bits;
      bits = binary_bits(j);
      for (vector<bool>::reverse_iterator it=bits.rbegin();
	   it != bits.rend(); ++it)
	result.push_back(*it);
      j = bits.size()-1;
    }
    reverse(result.begin(), result.end());
    return result;
  }
}

static const double rissConst ((log (2.865))/(log (2.0)));

double
monotone_universal_length(int j)
{
  assert (j >= 1);

  return (rissConst + log_star(j));
}



//////////////////////////  Log functions ////////////////////

double
log_star (const double x)
{
  double xx = x;
  double result = 0.0;

  assert (x > 0.0);

  while (xx > 1.0)
  { xx = log_2(xx);
    result += xx;
  }
  return result;
}

  

static const double log2 (log (2.0));

double
log_2 (const double x)
{
  assert (x > 0.0);
  return ( (log (x))/log2 );
}


//////////////  bits to code a boolean vector //////////////////

double entropy (const int n, const double p)
{
  return ( n *
	   ( -p * log_2(p) - (1.0-p) * log_2(1.0-p))  );
}

int compressed_bit_length(double p, int n)
{
  assert (0 <= p);
  assert (p <= 1);
  assert (n > 0);
  // ??? can we really get away with with for a boolean sequence?
  if (p == 0.0) return 2;
  if (p == 1.0) return 2;
  double idealLen = 0.5 * log_2((double)n) + entropy(n, p);
  return ( (int)floor(1.0 + idealLen) );
}

int compressed_bit_length(int x, int n)
{
  assert (x >= 0);
  assert (n >= x);
  assert (n > 0);
  return compressed_bit_length ( (double) x/ (double) n, n);
}

  
int compressed_bit_length(const vector<bool> &b)
{
  int n = b.size();
  int sum = accumulate(b.begin(), b.end(), 0);
  return compressed_bit_length (sum, n);
}


////////////////  Length for coding a -0+ integer vector  /////////////

namespace
{
  class Accum_univ
  {
  public:
    Accum_univ (int minSize): mMinSize(minSize)
      {
      }
    int operator()(int sum, int x) const
      {
	assert (x >= 0);
	if (x < mMinSize)
	  return sum;
	return (sum + signed_universal_bit_length (x - mMinSize));
      }
  private:
    int mMinSize;
  };
}
/*
  Format of coded bits is
    [1 bit for any compression?]
      if not compressed: sequence of universal codes
      if compressed    : [univ for #compressed] [compressed bits for these]
                              [universal codes for x-compressed levels]
*/
int optimal_block_bit_length (const vector<int> &x)
{
  vector<int> bitLength;            // total number bits for number of levels
  vector<int> compressedBitLength;  // bits for cum number of boolean levels
  int univBitLength;                // num of bits for universal code
  int cumBooleanBitLength;          // num bits for compressed boolean rows
  int prefixBitLength;              // num bits for telling number of boolean rows

  // sort the abs value of the input integers
  int n (x.size());
  vector<int> sortAbsX (n);
  transform (x.begin(), x.end(), sortAbsX.begin(), abs);
  sort (sortAbsX.begin(), sortAbsX.end());

  // initialize with no compressed rows (ie, all universal)
  prefixBitLength = 1;
  univBitLength = accumulate (sortAbsX.begin(), sortAbsX.end(), 0, Accum_univ(0));
  bitLength.push_back (prefixBitLength + univBitLength);
  
  // now compress those at "level" and recompute universal
  int level(0), nAtLevel, nAvailable(n);
  double sqrt_nH (sqrt(univBitLength));   // bound entropy using initial univ length
  cumBooleanBitLength = 0;
  vector<int>::iterator startAt = sortAbsX.begin();
  while (nAvailable>3 && level<sqrt_nH)
  {
    prefixBitLength = 1 + universal_bit_length(level);  // y/n? + max level compressed
    nAtLevel = count (startAt, sortAbsX.end(), level);
    cumBooleanBitLength += compressed_bit_length(nAtLevel, nAvailable);
    univBitLength = accumulate (startAt, sortAbsX.end(), 0, Accum_univ(level+1));
    bitLength.push_back(prefixBitLength + cumBooleanBitLength + univBitLength);
    nAvailable -= nAtLevel;
    ++level;
    startAt += nAtLevel;
  }
  // cout << "Vector of bits[level]: " << bitLength;
  // Note that bit length has values for compressing none, row 0, rows (0,1),...
  return (*min_element(bitLength.begin(), bitLength.end()));
}


///////////////////////////////////////////////////////////////////
namespace
{
  const double kREDivisor = 2.0 * log(2.0);
}

int normal_data_bit_length (const double ss)  // ????
{
  /*
    Does not include the leading "constant" term (-n/2) log (2\pi\sigma^2)
    and treats sigma^2 as one.
  */
  return (int) floor(1.0 + ss/kREDivisor);
}

double relative_entropy (const double est, const double z)
{
  double diff = est - z;
  
  return ( diff * diff / kREDivisor );
}


///////////////////////  EOF  ////////////////////////////////////




