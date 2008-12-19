// $Id: speed.test.cc,v 1.8 2004/03/21 23:04:57 bob Exp $

#include "range_traits.h"
#include "range.h"
#include "range_ops.h"

#include "random.h"
#include "function_utils.h"
#include "function_iterators.h"

#include <boost/tuple/tuple.hpp>

#include <vector>
#include <list>
#include <time.h>
#include <iostream>
#include <algorithm>
#include <numeric>
#include <functional>


class GenOp {
   RandomGenerator& mGenerator;
 public:
   GenOp(RandomGenerator& rand)
     : mGenerator(rand) { }
   double operator()() { return mGenerator.uniform(); }
};

class Square : public std::unary_function<double,double> {
  public:
    double operator()(double x) const { return x*x; }
  };


class AccumCenterSquare : public std::unary_function<double,double> {
  double mCenter;
  double mTotal;
public:
  AccumCenterSquare(double c)
    : mCenter(c),mTotal(0) { }
  double total() { return mTotal; }
  void operator()(double x) 
  {
    double dev = x - mCenter;
    mTotal = mTotal + dev*dev;
  }
};



class DoNothing {
public:
    std::string class_name() const{return "s2 NOTHING";};
  double operator()(std::vector<double>&, int , double ) const
  {
    return 0.0;
  }
};

class ForIndex {
public:
    std::string class_name() const{return "s2 FOR[i]";};

  double operator()(std::vector<double>& x, int length, double xBar) const
  {
    Function_Utils::CenterSquare c(xBar);
    double total = 0;
    for (int i=0; i<length; ++i) {
      total = total + c(x[i]);
    }
    return total / length;
  }
};

class ForIteratorIndex {
public:
  std::string class_name() const{return "s2 FOR */index";};
  double operator()(std::vector<double>& x, int length, double xBar) const
  {
    Function_Utils::CenterSquare c(xBar);
    double ss = 0.0;
    std::vector<double>::const_iterator it=x.begin();
    for (int i=0; i<(x.end() - x.begin()); ++i) {
      ss = ss + c(*it); ++ it;
    }
    return ss / length;
  }
};

class ForIterator {
public:
  std::string class_name() const{return "s2 FOR *";};
  double operator()(std::vector<double>& x, int length, double xBar) const
  {
    Function_Utils::CenterSquare c(xBar);
    double ss = 0.0;
    for (std::vector<double>::const_iterator it=x.begin(); it != x.end(); ++it) {
      ss = ss + c(*it); 
    }
    return ss / length;
  }
};


class ForIteratorUnrolled4 {
public:
  std::string class_name() const{return "s2 FOR *4";};

  double operator()(std::vector<double>& x, int length, double xBar) const
  {
    Function_Utils::CenterSquare c(xBar);
    double ss = 0.0;
    for (std::vector<double>::const_iterator it=x.begin();
	 it != x.end(); ++it) {
      ss = ss + c(*it);      ++ it;
      ss = ss + c(*it);      ++ it;
      ss = ss + c(*it);      ++ it;
      ss = ss + c(*it);
    }
    return ss / length;
  }
};

class ForIteratorUnrolled10 {
public:
  std::string class_name() const{return "s2 FOR *10";};

  double operator()(std::vector<double>& x, int length, double xBar) const
  {
    Function_Utils::CenterSquare c(xBar);
    double ss = 0.0;
    for (std::vector<double>::const_iterator it=x.begin();
	 it != x.end(); ++it) {
      ss = ss + c(*it);      ++ it;
      ss = ss + c(*it);      ++ it;
      ss = ss + c(*it);      ++ it;
      ss = ss + c(*it);      ++ it;
      ss = ss + c(*it);      ++ it;
      ss = ss + c(*it);      ++ it;
      ss = ss + c(*it);      ++ it;
      ss = ss + c(*it);      ++ it;
      ss = ss + c(*it);      ++ it;
      ss = ss + c(*it);
    }
    return ss / length;
  }
};
  
class Transform {
public:
  std::string class_name() const{return "s2 TRANSFORM";};

  double operator()(std::vector<double>& x, int length, double xBar) const
  {
    std::vector<double> sqrs (length);
    transform(x.begin(), x.end(), sqrs.begin(), Function_Utils::CenterSquare(xBar));
    double ss = accumulate(sqrs.begin(), sqrs.end(), 0.0);
    return ss / length;
  }
};

class ForEach {
public:
  std::string class_name() const{return "s2 FOR_EACH";};

  double operator()(std::vector<double>& x, int length, double xBar) const
  {
    AccumCenterSquare accum = for_each(x.begin(), x.end(), AccumCenterSquare(xBar));
    return accum.total() / length;
  }
};  

class Range {
public:
  std::string class_name() const{return "s2 RANGE";};

  double operator()(const std::vector<double>& x, int length, double xBar) const
  {
    double ss  = range_ops::accumulate(make_unary_range(Function_Utils::CenterSquare(xBar), make_range(x)),0.0);
    return ss / length;
  }
};


template<class F>
void
time_calculation_of_s2(const F& f, char *label, RandomGenerator& rand)            // Timer
{
  const int factor= 10;  // length (nreps) grows (shrinks) by this factor for each of main loop iterations
  int length=      200;  // initial series length
  int nReps =   300000;  
  double s2 = 0.0;
  int cache_killer = 1;  // a value of 1000 seems to be enough to kill the cache on besov

  for (int series = 0; series < 3; ++ series)
    {
      std::vector<std::vector<double> > xs;
      std::vector<double> xbars;
      for (int j=0; j <= cache_killer; ++j)
      {
	std::vector<double> x(length);
	generate(x.begin(), x.end(), GenOp(rand));
	xs.push_back(x);
	double xBar = accumulate(x.begin(), x.end(), 0.0)/length;
	xbars.push_back(xBar);
      }
      
      clock_t start = clock();
      for (int j=0; j < nReps; ++j)
	{
	  std::vector<double>& x = xs[j - cache_killer*(j/cache_killer)];
	  double xBar = xbars[j - cache_killer*(j/cache_killer)];
	  s2 += f(x, length, xBar);
	}
      std::cout << " " << label << ": Time for length " << length << " was "
		<< double(clock() - start)/CLOCKS_PER_SEC << std::endl;
      length *= factor;
      nReps  /= factor;
    }     
  std::cout << std::endl;
}


//////////////////////////////  Inner Products  //////////////////////////////

class ForLoopIP2{
  public:
  std::string class_name() const{return "ip FOR LOOP";};

  double operator()(const std::vector<double>& x,
		    const std::vector<double>& y) const
    {
      double dp = 0.0;
      std::vector<double>::const_iterator xi = x.begin();
      std::vector<double>::const_iterator yi = y.begin();
      for ( ;  xi != x.end();  ++xi, ++yi) {
	dp = dp + *xi * *yi;
      }
      return dp;
    }
};

class IP2{
  public:
  std::string class_name() const{return "ip STL_IP";};

  double operator()(const std::vector<double>& x,
		    const std::vector<double>& y) const
    {
      return inner_product(x.begin(), x.end(), y.begin(), 0.0);
    }
};


class AccumIP2 {
  public:
  std::string class_name() const{return "ip Accum BIN_RANGE_IP (not unrolled)";};

  double operator()(const std::vector<double>& x,
		    const std::vector<double>& y) const
  {
    return range_ops::simple_accumulate (make_binary_range(std::multiplies<double>(),
							   make_range(y),
							   make_range(x)  ),
					 0.0);
  }
};


template <class F>
void
time_calculation_of_inner_product(const F& f, char *label, RandomGenerator& rand)
{
  const int factor=10;
  int length=     200;
  int nReps =  300000;

  for (int series = 0; series < 3; ++ series)
    {
      double ip = 0.0;
      std::vector<std::vector<double> > xs;
      std::vector<std::vector<double> > ys;
      for (int j=0; j < 101; ++j)
	{
	  std::vector<double> x(length), y(length);
	  generate(x.begin(), x.end(), GenOp(rand));
	  generate(y.begin(), y.end(), GenOp(rand));
	  xs.push_back(x); ys.push_back(y);
	}
      
      clock_t start = clock();
      for (int j=0; j < nReps; ++j)
	{
	  std::vector<double>& x = xs[j - 100*(j/100)];
	  std::vector<double>& y = ys[j - 100*(j/100)];
	  ip += f(x, y);
	}
      std::cout << f.class_name() << " " << label << ": Time for length " << length << " was "
 		<< double(clock() - start)/CLOCKS_PER_SEC << " giving avg " << ip/nReps << std::endl;
      length *= factor;
      nReps  /= factor;
    }     
  std::cout << std::endl;
}

 
///////////////////////////////////  Main  /////////////////////////////////

int main()
{
  using namespace range_ops;
  const int factor=10;
  RandomGenerator rand(12742);

  std::cout << "\nSpeed tests for computing s2\n\n";
  time_calculation_of_s2(DoNothing(),           "NOTHING",   rand);
  time_calculation_of_s2(ForIndex(),            "FOR[i]",    rand);
  time_calculation_of_s2(ForIteratorIndex(),    "FOR */idx", rand);
  time_calculation_of_s2(Range(),               "RANGE",     rand);
  time_calculation_of_s2(ForIterator(),         "FOR * ",    rand);
  time_calculation_of_s2(ForIteratorUnrolled4(),"FOR *4",    rand);
  time_calculation_of_s2(ForIteratorUnrolled10(),"FOR *10",  rand);
  time_calculation_of_s2(ForEach(),             "FOR_EACH",  rand);
  time_calculation_of_s2(Transform(),           "TRANSFORM", rand);
  
  std::cout << "\n\n Inner product loops... \n\n";
  time_calculation_of_inner_product (AccumIP2()    , "RANGE_IP (not unrolled)", rand);
  time_calculation_of_inner_product (ForLoopIP2()  , "FOR LOOP"  , rand);
  time_calculation_of_inner_product (IP2()         , "STL_IP"    , rand);


  std::cout << "\n\n Dot product for pair of vectors comparison \n";  
  { // ranges
    int length=  100;
    int nReps =  10000;
    double dp = 0.0;
    for (int series = 0; series < 3; ++ series) {
      std::vector<double> x (length);
      std::vector<double> y (length);
      generate(x.begin(), x.end(), GenOp(rand));
      clock_t start = clock();
      for (int j=0; j < nReps; ++j) {
	generate(y.begin(), y.end(), GenOp(rand));
	dp += std::inner_product(x.begin(), x.end(), y.begin(), 0.0);
      }
      std::cout << "Dot prod via INNER_PRODUCT: Time for length " << length << " was "
		<< clock() - start << std::endl;
      length *= factor;
      nReps  /= factor;
    }     
  }
  
  { // ranges
    int length=  100;
    int nReps =  10000;
    double dp = 0.0;
    for (int series = 0; series < 3; ++ series) {
      std::vector<double> x (length);
      std::vector<double> y (length);
      generate(x.begin(), x.end(), GenOp(rand));
      clock_t start = clock();
      for (int j=0; j < nReps; ++j) {
	generate(y.begin(), y.end(), GenOp(rand));
	dp += accumulate(make_binary_range(std::plus<double>(),
					   make_range(x),
					   make_range(y)),
			 0.0);
      }
      std::cout << "Dot prod via RANGE: Time for length " << length << " was "
		<< clock() - start << std::endl;
      length *= factor;
      nReps  /= factor;
    }     
  }

  return 0;
}
