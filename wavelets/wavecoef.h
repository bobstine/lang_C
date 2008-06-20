// $Id: wavecoef.h,v 1.2 2000/02/04 19:36:03 bob Exp $-*- c++ -*-
#ifndef _wave_coef_
#define _wave_coef_

#include <vector.h>
#include <set.h>
#include <string>

class Wave_coef
{

 public:
  Wave_coef(string name, int length, double* wavelet);

  const string& name() const;
  const vector<double> & hi_pass() const;
  const vector<double> & lo_pass() const;
    
  bool operator< (const Wave_coef &) const;

  static const Wave_coef& find(const string &) ;
  
 private:
  string mName;
  vector<double> mLoCoef;
  vector<double> mHiCoef;

  static set< Wave_coef > sWaveSet;
  static vector<double> Wave_coef::make_hi_pass(const vector<double> &lo);
};


const Wave_coef& wavelet_coefficient(const string& name);


extern const Wave_coef haar;
extern const Wave_coef daub2;        // has  4 coefs
extern const Wave_coef daub6;        // has  8 coefs
extern const Wave_coef daub10;       // has 12 coefs
extern const Wave_coef daub18;       // has 18 coefs

#endif
