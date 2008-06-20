// $Id: copy_counter.cc,v 1.1 2005/06/13 20:47:49 bob Exp $-*- c++ -*-

#include <iostream>
#include <assert.h>
#include <vector>

class A
{
public:
  int m_i;
  std::vector<double>* m_v;
  A* m_this;
  
  virtual ~A();
  A(int i);
  A(const A&rhs);
};

A
easy_opt()
{
  return A(1);
}

template<class T>
A
hard_opt(T i)
{
  A result(i);
  return result;
}

template<class T>
A
harder_opt(T b)
{
  A result2(2*b);
  A result1(b);
  if(b > 0)
    return result1;
  else
    return result2;
}

main()
{
  {
    A x = easy_opt();
    std::cout << "x created: " << x.m_i << std::endl;
  }
  {
    A y = hard_opt(300);
    std::cout << "y created: " << y.m_i << std::endl;
    A z(y);
    std::cout << "z created: " << z.m_i << std::endl;
  }
  std::cout << "============================================================" << std::endl;
  {
    A x(harder_opt(-10));
    std::cout << "x created: " << x.m_i << std::endl;
  }
};


  
A::A(int i):
  m_i(i),
  m_v(),
  m_this(this)
  {
  }

A::A(const A&rhs)
    :
    m_i(rhs.m_i+1),
    m_v(new std::vector<double>(rhs.m_i+20)),
    m_this(this)
  {
    std::cout << "\tcopy " << rhs.m_i  << " --> " << m_i << ": " << this << std::endl;
  }

A::~A()
  {
    std::cout << "\t\t\tdeath! " << m_i << "[ " << this << " == " << m_this << "]" << std::endl;
  }
