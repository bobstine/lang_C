// $Id: virtual_wrapper_register.cc,v 1.1 2003/11/26 14:11:50 bob Exp $-*- c++ -*-


#include <iostream>
#include <assert.h>
#include <vector>
#include <list>
#include <map>

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

class iter
{
public:
  double operator*() const
  {
    return 7.0;
  }
};




/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

class type_info_less
{
public:
  bool operator()(const std::type_info* lhs,const std::type_info* rhs)
  {
    return lhs->before(*rhs);
  }
};

class Register
{
  static Register* sp_library;
public:
  std::map<int,wrapper_abc<double>*> m_map;
  std::map<const std::type_info*, int,type_info_less> m_reverse_map;

  static Register* get_singleton()
  {
    if(sp_library == 0)
      {
	sp_library = new Register();
      }
    return sp_library;
  };

  int wrapper_id(wrapper_abc<double>* w)
  {
    const std::type_info& id = typeid(*w);
    std::cout << "registering: " << id.name() << std::endl;
    int index = m_reverse_map[&id];
    if(index == 0)
      {
	index = m_reverse_map.size();
	m_reverse_map[&id] = index;
	m_map[index] = w;
      }
    return index;
  }

  int wrapper_id(const std::type_info& id)
  {
    std::cout << " looking up: " << id.name() << std::endl;
    int index = m_reverse_map[&id];
    assert(index != 0);
    return index;
  }
};

Register* Register::sp_library = 0;


template<class T>
class Register_wrapper
{
public:
  Register_wrapper()
  {
    Register::get_singleton()->wrapper_id(new T);
  }
};


template<class T1, class T2>
class Register_pair
{
public:
  Register_pair()
  {
    int id1 = Register::get_singleton()->wrapper_id(typeid(T1));
    int id2 = Register::get_singleton()->wrapper_id(typeid(T2));
    // now register a produce, difference, etc for this pair
  }
};




/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

// the following would go in the file that defines a new wrapper.  Note it could be a header file
// that gets included more than one place

namespace
{
  Register_wrapper<wrapper<iter> > construct_it;
  Register_pair<wrapper<iter>,wrapper<iter> > construct_pair;
}

namespace another_unnamed_names_space // in say a different .o file
{
  Register_wrapper<wrapper<iter> > construct_it;
}

int
main()
{
  wrapper<iter> i;
  std::cout << *i  << std::endl;

  wrapper_abc<double>& i_ref = i;
  std::cout << *i_ref << std::endl;

  std::list<wrapper_abc<double>*> foo;
  std::vector<wrapper_abc<double>*> bar;

  std::cout << Register::get_singleton()->m_map.size() << std::endl;
};
