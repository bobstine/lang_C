/*
  21 Sep 01 ...   Created to encapsulate coding-related functions.
*/
#ifndef _CODING_H_
#define _CODING_H_

#include <vector>
#include <functional>
#include <utility>

double
gaussian_data_length (int n, double RSS);

double
change_in_code_length (int n, double RSS, int q, double dRSS);  // negative means gets shorter

// Coders

class CauchyCoder : public std::unary_function<int,double>
{
 public:

  double
    operator()(int j) const;
};

class SignedCauchyCoder : public std::unary_function<double,double>
{
 public:

  double
    operator()(double z) const;
};


// Run length code

template <class IndexCoder, class ZCoder>
class RunLengthCoder : public std::unary_function< std::vector< std::pair<int,double> >, double>
{
  IndexCoder mIndexCoder;
  ZCoder     mZCoder;
 public:
  RunLengthCoder(IndexCoder const& ic, ZCoder const& zc)
    : mIndexCoder(ic), mZCoder(zc) { }
  double
    operator()(std::vector< std::pair<int,double> > const& j_z) const
    { double len (0.0);
      for (unsigned int i=0; i<j_z.size(); ++i)
      { double iBits (mIndexCoder(j_z[i].first));
	double zBits (mZCoder(j_z[i].second));
	// std::clog << "CODE: j=" << j_z[i].first << " adds " << iBits << " and z="
	//	  << j_z[i].second << " adds " << zBits << " bits " << std::endl;
	len += 1 + iBits + zBits; // continuation bit
      }
      return len;
    }
};

template<class IndexCoder, class ZCoder>
  RunLengthCoder<IndexCoder,ZCoder>
  make_run_length_coder(IndexCoder const& ic, ZCoder const& zc)
  {
    return RunLengthCoder<IndexCoder,ZCoder>(ic,zc);
  }



double
codeIndexLength(double gShare, double gProb, int k);

double
fStatisticThreshold (double gShare, double gProb, int k);

/*
  These are pdf's for discrete, positive density functions.
  The range for k should be restricted to k=1,2,3...
*/

double
geometricPDF (double p, int k);

double
cauchyPDF (int k);

double 
universalPDF (int k);                // k must be positive or you're hosed.

double
slowUniversalPDF (int k);            // skips over the initial five terms

#endif
