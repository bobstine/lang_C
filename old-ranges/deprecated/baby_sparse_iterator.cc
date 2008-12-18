// $Id: baby_sparse_iterator.cc,v 1.1 2003/11/26 14:11:50 bob Exp $


// seeing what ideas need to be added to our concept


#include <map>
#include <iostream>

class const_sparse_iterator // doesn't support writing to it
{
  typedef std::map<int, double> map_type;
  typedef map_type::const_iterator const_iterator;
  typedef map_type::key_type    key_type;
  typedef map_type::mapped_type mapped_type;
  
  map_type m_map;
  const_iterator m_iter;
  key_type  m_key;
public:
  const_sparse_iterator(map_type& m, const_iterator& i, key_type k)
    :
    m_map(m),
    m_iter(i),
    m_key(k)
  {
  }

  const_sparse_iterator
  operator+(const int n)  const  {
    const_sparse_iterator result(*this);
    result += n;
    return result;
  };

  mapped_type
  operator*()  const  {
    key_type current_key = (*m_iter).first;
    if(current_key == m_key)
      return (*m_iter).second;
    else
      {
	//	mapped_type result = mapped_type();
	//	std::cout << "using default value of " << result << std::endl;
	//	return result;
	return mapped_type();
      }
  };
  
  const_sparse_iterator
  operator+=(const int n)  {
    m_key += n;
    rectify_count();
    return *this;
  };


  void
  rectify_count() // called when i changes--gets the iterator in sync with i
  {// NOTE:  m_key must be in range [begin,end] or it this generates undefined behavior
    key_type current_key = (*m_iter).first;
    if(current_key < m_key)
      {
	// m_iterator may be to the left of where it should be
	const_iterator next = m_iter;
	++next;
	key_type next_key = (*next).first;
	if(next_key <= m_key)
	  {  // oops! we aren't legal.  Lets check if we are simply walking to the next node
	    if(next_key == m_key)
	      m_iter = next;
	    else
	      // no quick fix--so just look it up (takes log(n) time)
	      m_iter = find_key(m_key);
	  }
      }
    if(current_key > m_key)
      { // m_iterator is definitely too large
	const_iterator previous = m_iter;
	--previous;
	key_type previous_key = (*previous).first;
	if(previous_key <=  m_key)
	  m_iter = previous;
	else
	  // no quick fixes--so lets just look it up (takes log(n) time)
	  m_iter = find_key(m_key);
      }
  }

  const_iterator
  find_key(key_type i)
  {
    return m_map.lower_bound(i);  // use this for maps
  }
};


int
main()
{
  typedef std::map<int,double> m_type;
  typedef m_type::const_iterator ci_type;
  m_type m;
  m[1] = 10.;
  m[2] = 20.;
  m[3] = 30.;
  m[4] = 40.;
  m[5] = 50.;
  m[6] = 60.;
  m[7] = 70.;
  m[10] = 100.;
  /*
  m.push_back(std::make_pair(1,10.));
  m.push_back(std::make_pair(2,20.));
  m.push_back(std::make_pair(3,30.));
  m.push_back(std::make_pair(4,40.));
  m.push_back(std::make_pair(5,50.));
  m.push_back(std::make_pair(6,60.));
  m.push_back(std::make_pair(7,70.));
  m.push_back(std::make_pair(10,100.));
  */

  ci_type i = m.begin();
  const_sparse_iterator j(m,i,1);
  std::cout << *j << std::endl;
  std::cout << *(j+3) << std::endl;
  std::cout << *(j+1) << std::endl;
  const_sparse_iterator k(j+4);
  std::cout << *(k+(-3)) << std::endl;
  std::cout << *(k+(-1)) << std::endl;
  std::cout << *(k) << std::endl;
  std::cout << *(k+1) << std::endl;
  std::cout << *(k+2) << std::endl;
  std::cout << *(k+3) << std::endl;
  std::cout << *(k+4) << std::endl;
  std::cout << *(k+5) << std::endl;
}
