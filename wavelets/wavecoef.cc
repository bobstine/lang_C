// $Id: wavecoef.cc,v 1.2 2000/02/04 19:36:03 bob Exp $-*- c++ -*-

#include <algo.h>

#include <assert.h>

#include "wavecoef.h"

// define the class varible -- it gets instantiated prior to main call

set< Wave_coef >  Wave_coef::sWaveSet;


Wave_coef::Wave_coef(string name, int length, double* coef)
  :
  mName(name),
  mLoCoef(coef,coef+length),
  mHiCoef(make_hi_pass(mLoCoef))
{
  sWaveSet.insert(*this);
}


const string& Wave_coef::name() const
{
  return mName;
}


const vector<double> &Wave_coef::lo_pass() const
{
  return mLoCoef;
}

const vector<double> &Wave_coef::hi_pass() const
{
  return mHiCoef;
}

vector<double> Wave_coef::make_hi_pass(const vector<double> &lo)
{
  double sign (1.0);
  vector<double> high (lo.size());

  reverse_copy(lo.begin(),lo.end(),high.begin());
  for(vector<double>::iterator it = high.begin(); it != high.end(); ++it)
  {
    *it *= sign;
    sign *= -1.0;
  }
  return high;
}


bool
Wave_coef::operator< (const Wave_coef &other) const
{
  return (mName < other.name());
}


const Wave_coef&                       // 'static' class function
Wave_coef::find(const string &name)
{
  // make a fake one to do the search
  double *null;
  Wave_coef fake(name, 0,  null);
  set< Wave_coef >::const_iterator it = sWaveSet.find(fake);
  assert (it != sWaveSet.end());
  return *it;
}


//////////////////// actual coefficients ///////////////////

double haar_data[2] = {1/sqrt(2),1/sqrt(2)};
const Wave_coef haar("haar",2,haar_data);

double daub2_data[4] = { (1+sqrt(3))/(4*sqrt(2)),
			 (3+sqrt(3))/(4*sqrt(2)),
			 (3-sqrt(3))/(4*sqrt(2)),
			 (1-sqrt(3))/(4*sqrt(2))  };
const Wave_coef daub2("daub2", 4, daub2_data);

double daub6_data[8] = { 0.2303778133088964,
			 0.7148465705529154,
			 0.6308807679398587,
			 -0.0279837694168599,
			 -0.1870348117190931,
			 0.0308413818355607,
			 0.0328830116668852,
			 -0.0105974017850690};
const Wave_coef daub6("daub6", 8, daub6_data);

double daub10_data[12] = { 0.1115407433501095,
			   0.4946238903984533,
			   0.7511339080210959,
			   0.3152503517091982,
			   -0.2262646939654400,
			   -0.1297668675672625,
			   0.0975016055873225,
			   0.0275228655303053,
			   -0.0315820393174862,
			   0.0005538422011614,
			   0.0047772575109455,
			   -0.0010773010853085 };
const Wave_coef daub10("daub10", 12, daub10_data);

double daub18_data[20] = { 0.0266700579005473,
			   0.1881768000776347,
			   0.5272011889315757,
			   0.6884590394534363,
			   0.2811723436605715,
			   -0.2498464243271598,
			   -0.1959462743772862,
			   0.1273693403357541,
			   0.0930573646035547,
			   -0.0713941471663501,
			   -0.0294575368218399,
			   0.0332126740593612,
			   0.0036065535669870,
			   -0.0107331754833007,
			   0.0013953517470688,
			   0.0019924052951925,
			   -0.0006858566949564,
			   -0.0001164668551285,
			   0.0000935886703202,
			   -0.0000132642028945 };
const Wave_coef daub18("daub18", 20, daub18_data);


///////////////////   EOF  ////////////////////////////////



