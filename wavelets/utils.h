// $Id: utils.h,v 1.6 2000/04/25 16:29:52 bob Exp $-*- c++ -*-
#ifndef _utils_
#define _utils_

#include <iostream.h>
#include <vector.h>
#include <map.h>
#include <multimap.h>
#include <numeric>

#include <string>


class print_vector
{
 public:
  void operator()(const vector<double> &vec)
    {
      cout << "[" << vec.size() << "] {";
      copy(vec.begin(),vec.end(),ostream_iterator<double>(cout," "));
      cout << "}" << endl;
    }
  void operator()(const string &str, const vector<double> &vec)
    {
      cout << str << ":" << endl;
      (*this)(vec);
    }
};

template<class T>
ostream&
operator<<(ostream& ostrm, const vector<T>& vec)
{
  ostrm << "[" << vec.size() << "] {";
  copy(vec.begin(),vec.end(),ostream_iterator<T>(ostrm," "));
  ostrm << "}" << endl;
  return ostrm;
}

template<class Key, class T>
ostream&
operator<<(ostream& ostrm, const pair<Key,T> p)
{
  ostrm  << " (" << p.first << " . " << p.second  << ") ";
  return ostrm;
}
template<class Key, class T, class Order, class Alloc>
ostream&
operator<<(ostream& ostrm, const map<Key, T, Order, Alloc>& theMap)
{
  ostrm << "[" << theMap.size() << "] { ";
  copy(theMap.begin(), theMap.end(), ostream_iterator<pair<const Key,T> >(ostrm, " "));
  ostrm << "}" << endl;
  return ostrm;
}

template<class Key, class T, class Order, class Alloc>
ostream&
operator<<(ostream& ostrm, const multimap<Key, T, Order, Alloc>& theMap)
{
  ostrm << "[" << theMap.size() << "] { ";
  copy(theMap.begin(), theMap.end(), ostream_iterator<pair<const Key,T> >(ostrm, " "));
  ostrm << "}" << endl;
  return ostrm;
}


class sum_of_squares
{
  public:
  double operator() (const vector<double> &x)
    {
      return inner_product(x.begin(), x.end(), x.begin(), 0.0);
    }
};

#endif








