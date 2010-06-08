// $Id: sparse_iterator.Template.h,v 1.2 2004/04/29 16:54:21 bob Exp $

/////////////////////////////////////////////////////////////////////////////////////////
//
//  Data structure layout of of how a vector<double>(10) would
//  be represented to be used by a sparse_iterator: 
//
//          index:     0   1   2    3       4   5    6   7   8      9
//           data:    [0,  0,  0,   3.141,  0,  0,   0,  0,  2.78,  0]
//
//  representation:    [<0,0>,  <3,3.141>,  <8, 2.78>,  <10,0>]
//
//  NOTE: Notice that we have a "one past end" represented as <10,0>.
//  NOTE: Notice a beginning cell of the array is included.
//
//  An iterator taking on values 0..9 would look like:
//
//  base index iterator=[vector, base_iter,offset]   *(iterator)
//     0           [representation&, 0,  0 ]            0
//     1           [representation&, 1,  2 ]            0
//     2           [representation&, 1,  1 ]            0
//     3           [representation&, 1,  0 ]            3.141
//     4           [representation&, 2,  4 ]            0
//     5           [representation&, 2,  3 ]            0 
//     6           [representation&, 2,  2 ]            0
//     7           [representation&, 2,  1 ]            0
//     8           [representation&, 2,  0 ]            2.78
//     9           [representation&, 3,  1 ]            0
//
//     10          [representation&, 3,  0 ]         illegal
//


//   OOPS!!!! is <0,0> actually used  ????



/////////////////////////////////////////////////////////////////////////////////////////

#include <vector>
#include <utility>
#include <time.h>
#include <iostream>
#include <assert.h>

template <class Key, class X>  // Key should probably be removed as a parameter and int hard wired in
class const_sparse_iterator: public std::iterator<sparse_iterator_tag, X>
{
 public:
  typedef std::vector<std::pair<Key, X> > map_type;
  typedef typename map_type::const_iterator const_iterator;
    class advance_to_next_non_zero
  {
  public:
    advance_to_next_non_zero(const const_sparse_iterator<Key,X>&)
    {
    };
    void operator()(const_sparse_iterator<Key,X>& iter)
    {
      iter.advance_to_next_non_zero();
    }
  };
  
 private:

  const map_type& m_map;
  const_iterator m_base_iter;     // iterator into underlying data map
  Key            m_offset;        // remaining until next non-zero ??? should be key difference ???

public:
  const_sparse_iterator(const map_type& m)
    : m_map(m), m_base_iter(m.begin()), m_offset(0)
  {
    m_offset = (*m_base_iter).first;
  }
  const_sparse_iterator(const map_type& m, int i)
    : m_map(m), m_base_iter(m.begin()), m_offset(0)
  {
    reset_pointers(i);
  }

  void
  operator=(const const_sparse_iterator<Key,X>& rhs)
  {
    assert(&m_map == &(rhs.m_map));
    m_base_iter = rhs.m_base_iter;
    m_offset = rhs.m_offset;
  };

  const_sparse_iterator
  operator+(const int n)  const  {
    const_sparse_iterator result(*this);
    result += n;
    return result;
  };

  const_sparse_iterator
  operator-(const int n)  const  {
    const_sparse_iterator result(*this);
    result -= n;
    return result;
  };

  int
  operator-(const_sparse_iterator<Key,X> other)  const  {
    return external_index() - other.external_index();
  };

  int
  operator<(const_sparse_iterator<Key,X> other)  const  {
    return external_index() < other.external_index();
  };

  void
  advance_to_next_non_zero()
  {
    if(m_offset == 0)
      ++m_base_iter;
    else
      m_offset = 0; // now jump to the next non-zero cell
  };

  X
  operator*()  const  {
    if (m_offset == 0)
      return (*m_base_iter).second;
    else
      return 0.0;
  };
  
  const_sparse_iterator&
  operator++()  {
    if (m_offset > 0)
      --m_offset;
    else // moved past non-zero element
      {
	Key old = (*m_base_iter).first;
	++m_base_iter;
	m_offset = (*m_base_iter).first - old - 1;
      }
    return *this;
  };
  
  const_sparse_iterator&
  operator--()  {
    const_iterator prior = (m_base_iter-1);
    if (m_offset < (*m_base_iter).first - (*prior).first - 1)
      ++m_offset;
    else
      {
	m_offset = 0;
	m_base_iter = prior;
      }
    return *this;
  }
  
  const_sparse_iterator
  operator+=(const Key n)  {
    if (m_offset >= n)
      m_offset -= n; // haven't moved past next non-zero element
    else // moved past next non-zero element
      {
	Key absolute_key = external_index() + n;
	reset_pointers(absolute_key);  // figure out how to represent this key
      }
    return *this;
  };
  
  const_sparse_iterator
  operator-=(const Key n)  {
    reset_pointers(external_index() - n);  // rare enough so that there isn't any reason to try to speed it up
    return *this;
  };

  void simple_print_on(std::ostream& out) const
  {
    //    out << (*m_base_iter).first << " " << (*m_base_iter).second << " " << m_offset << "\t";
    out << "v[" << external_index() << "] = " << *(*this);
  };

  void print_on(std::ostream& out) const
  {
    simple_print_on(out);
    out << "  raw pts: " << (m_map.begin() - m_map.begin()) << " <= "
	<< (m_base_iter - m_map.begin()) << " <= "
	<< (m_map.end()-m_map.begin());
    out << " (offset = " << m_offset << " remaining until reach "
	<< "[" << (*m_base_iter).first << "," << (*m_base_iter).second << "]) ";
  }     
  int
  external_index() const
  {
    return ((*m_base_iter).first) - m_offset;
  }

private:
  

  void
  reset_pointers(const Key key) // resets base iterator and offset to new key
  {
    Key current_key = (*m_base_iter).first;
    if(current_key < key) // we are past current location
      {
	const_iterator next = m_base_iter;
	++next;
	Key next_key = (*next).first;
	if(key <= next_key)  // are we in next interval?
	  {
	    // std::cout << "Use quick advance\n";
	    m_base_iter = next;
	    m_offset = next_key - key;
	  }
	else  // no quick fix--so just look it up (takes log(n) time)
	  find_key(key);
      }
    else
      find_key(key);
  }

  void
  find_key(Key i)
  {
    //    return m_map.lower_bound(i);  // use this for maps
    m_base_iter =  recursive_find_key(i,m_map.begin(),m_map.end());
    m_offset = (*m_base_iter).first - i;
    //    std::cout << "\nFIND KEY: " << *this << std::endl;
    assert (m_offset >= 0);
  }

  const_iterator
  recursive_find_key(Key i, const_iterator lower, const_iterator upper)
  {
    if(upper - lower <= 1)
      return upper;
    const_iterator middle = (lower + (upper - lower)/2);
    if(i <= (*middle).first)
      return recursive_find_key(i,lower,middle);
    else
      return recursive_find_key(i,middle,upper);
  }

};


template<class X>
class sparse_back_insert_iterator
{
 protected:
  typedef std::vector<std::pair<int,X> > Container_type;
  Container_type*   container;
  int               last;
  
 public:
  typedef Container_type          container_type;
  typedef std::output_iterator_tag iterator_category;
  typedef void                value_type;
  typedef void                difference_type;
  typedef void                pointer;
  typedef void                reference;


  explicit sparse_back_insert_iterator(Container_type& __x) : container(&__x),last(0)
  {
    container->push_back(std::make_pair(0,X()));
  }
  
  sparse_back_insert_iterator<X>&
    operator=(const X& __value)
  {
    typename Container_type::reverse_iterator rb = container->rbegin();
    if(__value != X())
      {
	(*rb) = std::make_pair(last,__value);
	container->push_back(std::make_pair(0,X())); // any temparary would work
	rb = container->rbegin();
      }
    ++last;
    (*rb) = std::make_pair(last,X());
    return *this;
  }
  
  sparse_back_insert_iterator<X>& operator*() { return *this; }
  sparse_back_insert_iterator<X>& operator++() { return *this; }
  sparse_back_insert_iterator<X>& operator++(int) { return *this; }
};

