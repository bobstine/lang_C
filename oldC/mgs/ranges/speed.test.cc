// $Id: speed.test.cc,v 1.1 2002/06/16 14:59:13 bob Exp $

#include "range_traits.h"
#include "range.h"
#include "range_ops.h"

#include "utility.h"
#include "random.h"
#include "compose.h"
#include "functions.h"

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

class Center : public std::unary_function<double,double> {
  const double mC;
public:
  Center(double c) : mC(c) { }
  double operator()(double x) const { return x-mC; }
};  
  
class CenterSquare : public std::unary_function<double,double> {
  double mCenter;
public:
  CenterSquare(double c)
    : mCenter(c) { }
  double operator()(double x) const
  { double dev = x - mCenter; return dev*dev; }
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

class Shifter : public std::unary_function<double,double> {
  double mShift;
public:
  Shifter() { }
  Shifter(double s) : mShift(s) { }
  double operator()(const double x) const { return x + mShift; }
};


class DoNothing {
public:
    std::string class_name() const{return "s2 NOTHING";};
  double operator()(std::vector<double>& x, int length, double xBar) const
  {
    return 0.0;
  }
};

class ForIndex {
public:
    std::string class_name() const{return "s2 FOR[i]";};

  double operator()(std::vector<double>& x, int length, double xBar) const
  {
    CenterSquare c(xBar);
    double total = 0;
    for (int i=0; i<length; ++i) {
      total = total + c(x[i]);
    }
    return total / length;
  }
};

class ForIterator {
public:
  std::string class_name() const{return "s2 FOR *";};
  double operator()(std::vector<double>& x, int length, double xBar) const
  {
    CenterSquare c(xBar);
    double ss = 0.0;
    for (std::vector<double>::const_iterator it=x.begin();
	 it != x.end(); ++it) {
      ss = ss + c(*it); // cube it
    }
    return ss / length;
  }
};
  
class ForIteratorUnrolled4 {
public:
  std::string class_name() const{return "s2 FOR *4";};

  double operator()(std::vector<double>& x, int length, double xBar) const
  {
    CenterSquare c(xBar);
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
    CenterSquare c(xBar);
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
    transform(x.begin(), x.end(), sqrs.begin(), CenterSquare(xBar));
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
    double ss  = accumulate(make_unary_range(CenterSquare(xBar), make_range(x)),0.0);
    return ss / length;
  }
};


template<class F>
void
time_calculation_of_s2(const F& f, char *label, RandomGenerator& rand)
{
  const int factor=10;
  int length=    200;
  int nReps =  3000000;
  double s2 = 0.0;
  //  int cache_killer = 1000;  // a value of 1000 seems to be enough to kill the cache on besov
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
      std::cout << f.class_name() << " " << label << ": Time for length " << length << " was "
	   << double(clock() - start)/CLK_TCK/10000 << std::endl;
      length *= factor;
      nReps  /= factor;
    }     
  std::cout << std::endl;
}

//////////////////////////////  Weighted Inner Products  //////////////////////////////

class IteratorIP {
  public:
  std::string class_name() const{return "wip ITERATOR";};

  double operator()(const std::vector<double>& x,
		    const std::vector<double>& y,
		    const std::vector<double>& w) const
    {
      double dp = 0.0;
      std::vector<double>::const_iterator xi = x.begin();
      std::vector<double>::const_iterator yi = y.begin();
      std::vector<double>::const_iterator wi = w.begin();
      for ( ;  xi != x.end();  ++xi, ++yi, ++wi) {
	dp = dp + *xi * *yi * *wi;
      }
      return dp;
    }
};

class TransformIP {
  public:
  std::string class_name() const{return "wip TRANSFORM";};

  double operator()(const std::vector<double>& x,
		    const std::vector<double>& y,
		    const std::vector<double>& w) const
    {
      std::vector<double> wx (w.size());
      transform(x.begin(), x.end(), w.begin(), wx.begin(), std::multiplies<double>() );
      return inner_product(y.begin(), y.end(), wx.begin(), 0.0);
    }
};

class RangeIP {
  public:
  std::string class_name() const{return "wip RANGE_WIP";};

  double operator()(const std::vector<double>& x,
		    const std::vector<double>& y,
		    const std::vector<double>& w) const
    {
      using namespace range_ops;
      // faster than iterator:
      //      return inner_product(make_range(y), make_range(x) * make_range(w), 0.0);
      // faster than iterator:
      //      return accumulate(make_range(y) * make_range(x) * make_range(w), 0.0);
      // faster than iterator: 
      return simple_accumulate(make_range(y) * make_range(x) * make_range(w), 0.0);
    }
};


class RangeProdIP {
  public:
  std::string class_name() const{return "wip RANGE_PROD";};

  double operator()(const std::vector<double>& x,
		    const std::vector<double>& y,
		    const std::vector<double>& w) const
    {
      using namespace range_ops;
      return accumulate (make_range(y) * make_range(x) * make_range(w), 0.0);
    }
};

template <class F>
void
time_calculation_of_weighted_inner_product(const F& f, char *label, RandomGenerator& rand)
{
  const int factor=10;
  int length=     200;
  int nReps =  300000;

  for (int series = 0; series < 3; ++ series)
    {
      double ip = 0.0;
      std::vector<double> wts (length, 3.1);
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
	  ip += f(x, y, wts);
	}
      std::cout << f.class_name() << ":" << label << ": Time for length " << length << " was "
	   << double(clock() - start)/CLK_TCK/10000 << " giving avg " << ip/nReps << std::endl;
      length *= factor;
      nReps  /= factor;
    }     
  std::cout << std::endl;
}


//////////////////////////////  Inner Products  //////////////////////////////

class IteratorIP2{
  public:
  std::string class_name() const{return "ip ITERATOR";};

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
  std::string class_name() const{return "ip RANGE_IP (not unrolled)";};

  double operator()(const std::vector<double>& x,
		    const std::vector<double>& y) const
    {
      using namespace range_ops;
      //      return simple_accumulate (make_range(y) * make_range(x), 0.0);
      return simple_accumulate (make_range(y) * make_range(x), 0.0);
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
	   << double(clock() - start)/CLK_TCK/10000 << " giving avg " << ip/nReps << std::endl;
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
  RandomGenerator rand(12);


  std::cout << "\n\n Weighted inner product loops... \n\n";
  time_calculation_of_weighted_inner_product (IteratorIP() , "ITERATOR" , rand);
  time_calculation_of_weighted_inner_product (RangeIP()    , "RANGE_WIP" , rand);
  time_calculation_of_weighted_inner_product (RangeProdIP(), "RANGE_PROD", rand);
  // time_calculation_of_weighted_inner_product (TransformIP(), "TRANSFORM" , rand);
  
  
  std::cout << "\nSpeed tests for computing s2\n\n";
  time_calculation_of_s2(DoNothing(),           "NOTHING",   rand);
  time_calculation_of_s2(Range(),               "RANGE",     rand);
  time_calculation_of_s2(ForIndex(),            "FOR[i]",    rand);
  time_calculation_of_s2(ForIterator(),         "FOR * ",    rand);
  time_calculation_of_s2(ForIteratorUnrolled4(),"FOR *4",    rand);
  time_calculation_of_s2(ForIteratorUnrolled10(),"FOR *10",  rand);
  time_calculation_of_s2(ForEach(),             "FOR_EACH",  rand);
  time_calculation_of_s2(Transform(),           "TRANSFORM", rand);
  


  std::cout << "\n\n Inner product loops... \n\n";
  time_calculation_of_inner_product (AccumIP2()    , "RANGE_IP (not unrolled)", rand);
  time_calculation_of_inner_product (IteratorIP2() , "ITERATOR"  , rand);
  time_calculation_of_inner_product (IP2()         , "STL_IP"    , rand);






  std::cout << "\n\nTable construction and printing...\n";
  {
    std::vector< Shifter > f(4);
    f[0] = Shifter(1.0);  f[1] = Shifter(2.0);
    f[2] = Shifter(3.0);  f[3] = Shifter(4.0);
    std::vector<double> x(5);
    x[0] = 0.0; x[1] = 1.0; x[2] = 2.0; x[3] = 3.0; x[4] = 4.0;
    // std::cout << make_row_major_table(make_range(f), make_range(x));
  }


  std::cout << "\n\nFunction composition...\n";
  {
    const double avg = 5.0;
    std::cout << "Squared deviation of 3 from 5 = "
	 << boost::compose_f_gx(Square(), Center(avg))(3.0) << std::endl;

    std::vector<double> x(5);
    x[0] = 0.0; x[1] = 1.0; x[2] = 2.0; x[3] = 3.0; x[4] = 4.0;
    std::cout << "Vector of squared deviations about 5:  "
	 << make_unary_range(boost::compose_f_gx(Square(), Center(avg)),
			     make_range(x)) << std::endl;
  }
    
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
	dp += inner_product(x.begin(), x.end(), y.begin(), 0.0);
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
	dp += accumulate(make_binary_range(std::multiplies<double>(),
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
