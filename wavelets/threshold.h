// $Id: threshold.h,v 1.7 2000/04/25 16:29:52 bob Exp $-*- c++ -*-

#ifndef _threshold_
#define _threshold_

#include <vector>

class ThresholdBaseClass
{
public:
  virtual ~ThresholdBaseClass ();
  ThresholdBaseClass();

  virtual vector<double> operator()(const vector<double> &coef) const = 0;

private:
  ThresholdBaseClass(const ThresholdBaseClass &);
  void operator= (const ThresholdBaseClass &);  
};


class HardThreshold: public ThresholdBaseClass
{
 public:
  HardThreshold(double threshold);
  ~HardThreshold();
  
  void tune(const vector<double> &coef);
  vector<double> operator()(const vector<double> &coef) const;

private:
  double mThreshold;  
};


class SoftThreshold: public ThresholdBaseClass
{
 public:
  SoftThreshold(double threshold);
  ~SoftThreshold();
  
  void tune(const vector<double> &coef);
  vector<double> operator()(const vector<double> &coef) const;

private:
  double mThreshold;  
};


// Code using the best loop coder with one level optimized
class LoopThreshold: public ThresholdBaseClass
{
 public:
  LoopThreshold(double se);
  ~LoopThreshold();
  
  void tune(const vector<double> &coef);
  vector<double> operator()(const vector<double> &coef) const;

private:
  double mSE;
};


// Test implementation of the block coder with multiple levels compressed
class AdaptiveThreshold: public ThresholdBaseClass
{
 public:
  AdaptiveThreshold(double se);
  ~AdaptiveThreshold();
  
  void tune(const vector<double>  &);
  vector<double> operator()(const vector<double> &coef) const;

private:
  double mSE;  
};

// Function wrapper that allows use of these functions in STL applications
class ThresholdFunction
{
public:
  ThresholdFunction(ThresholdBaseClass *thresh);
  
  vector<double> operator()(const vector<double> &ests) const;

private:
  ThresholdBaseClass *mThresh;
};

#endif
//////////////////////////////   EOF  /////////////////////////
