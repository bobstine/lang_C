// $Id: virtual_wrapper.cc,v 1.1 2003/11/26 14:11:50 bob Exp $-*- c++ -*-


#include <iostream>
#include <assert.h>
#include <vector>
#include <list>

template<class T>
class wrapper_abc
{
public:
  virtual ~wrapper_abc(){};  // note: this shouldn't be here, but instead in the .cc file.
  virtual T operator*() const = 0;
};

template<class Iterator>
class wrapper: public wrapper_abc<double>, public Iterator
{
public:
  // use  double Iterator::operator*() const;
  double operator*() const
  {
    return Iterator::operator*();
  }
};


// less desirable solution is the following:

template<class Iterator>
class wrapper2: public wrapper_abc<double>
{
  Iterator i;
public:
  double operator*() const
  {
    return *i;
  }
};

class iter
{
public:
  double operator*() const
  {
    return 7.0;
  }
};


int
main()
{
  wrapper<iter> i;
  wrapper2<iter> j;
  std::cout << *i  << " " << *j << std::endl;

  wrapper_abc<double>& i_ref = i;
  wrapper_abc<double>& j_ref(j);
  std::cout << *i_ref  << " " << *j_ref << std::endl;

  std::list<wrapper_abc<double>*> foo;
  std::vector<wrapper_abc<double>*> bar;

  //  std::list<wrapper_abc<double>&> foo;   // wanted this, but NO!
  //  std::vector<wrapper_abc<double>&> bar;


};
