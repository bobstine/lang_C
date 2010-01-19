#ifndef _ANONYMOUS_ITERATORS_TEMPLATE_H_
#define _ANONYMOUS_ITERATORS_TEMPLATE_H_

#include <iterator>
#include <assert.h>

/*

  anonymous_iterators sanitize an iterator.  They abstract out all of
  the template material and shove it into a virtual function call.
  Thus there is only one anonymous_iterator_abc class.  Everything
  talks about it and uses it.  

  This is done is by having lots of classes that inherit from
  anonymous_abc.  Each one of these classes knows about how to compute
  operator*() for the particular type of iterator it holds.  Thus by
  adding one layer of indirection all iterators can be made into a
  common interface.

  There are basically three types of classes, with versions that
  conform to the tags associated with iterators. There's the ABC, then
  an envelope and the letter classes.  The letter classes are just
  called anonymous iterators.

  Notice that the ABC carries no state; if it did, it would not be so
  anonymous.  It does have "type state" in the sense of having a <tag,value>
  type.  The envelope carries a pointer to the letter, and the letter
  carries a pointer to the actual iterator. The range of iterators
  that are possible in the letter class is parameterized by the
  templatized mBaseIter in the anonymous iterator class.

  Why do this?  Of course if you can figure out what sort of
  iterator/range you are using at compile time, there is no need to do
  this sanitization.  BUT, if you need run time identification of the
  particular type of range you are using, then this will help.

  What about speed and all that?  If there are a few common
  anonymous_iterators<Iter> that are actually being used, functions
  like accumulate() can be written to run faster on the ones it can
  figure out.  The rest will then run virtually.

*/


// ANONYMOUS_ITERATOR_ABC  ANONYMOUS_ITERATOR_ABC  ANONYMOUS_ITERATOR_ABC  ANONYMOUS_ITERATOR_ABC  

template<class tag, class value_type>
class anonymous_iterator_abc;


template<class v_type>
class anonymous_iterator_abc<std::forward_iterator_tag,v_type>:
     public std::iterator<std::forward_iterator_tag, v_type>
{
  typedef std::forward_iterator_tag tag;
public:
  //  virtual ~anonymous_iterator_abc<tag,v_type>(){};   // evil.  don't put code here
  virtual ~anonymous_iterator_abc(){};   // evil.  don't put code here

  typedef anonymous_iterator_abc<tag,v_type> type_of_this;
  
  virtual type_of_this& operator++() = 0;
  //  virtual anonymous_iterator_abc operator++(int) = 0;   avoid
  virtual bool operator!=(const type_of_this& it) const = 0;
  virtual bool operator==(const type_of_this& it) const = 0;
  virtual v_type operator*() const = 0;
  virtual type_of_this* clone() const = 0;
};


template<class v_type>
class anonymous_iterator_abc<std::bidirectional_iterator_tag,v_type>:
     public anonymous_iterator_abc<std::forward_iterator_tag,v_type>
{
  typedef std::bidirectional_iterator_tag tag;
public:
  // virtual ~anonymous_iterator_abc<tag,v_type>(){};
  virtual ~anonymous_iterator_abc(){};
  
  typedef anonymous_iterator_abc<tag,v_type> type_of_this;
  
  virtual anonymous_iterator_abc& operator--() = 0;
  virtual type_of_this* clone() const = 0;
  //  virtual anonymous_iterator_abc operator--(int) = 0;  avoid
};

template<class v_type>
class anonymous_iterator_abc<std::random_access_iterator_tag,v_type>:
     public anonymous_iterator_abc<std::bidirectional_iterator_tag,v_type>
{
  typedef std::random_access_iterator_tag tag;
public:
  //  virtual ~anonymous_iterator_abc<tag,v_type>(){};
  virtual ~anonymous_iterator_abc(){};
  
  typedef anonymous_iterator_abc<tag,v_type> type_of_this;
  
  virtual type_of_this&
    operator+=(int n)  = 0;
 
  virtual type_of_this&
    operator-=(int n)  = 0;

  virtual typename std::iterator_traits<type_of_this>::difference_type   // this "int" should be difference type
    operator-(const type_of_this& it) const = 0;

  virtual bool
    operator<(const type_of_this& it) const = 0;
  virtual bool
    operator>(const type_of_this& it) const = 0;
  virtual type_of_this* clone() const = 0;
};



//  ENVELOPE  ENVELOPE  ENVELOPE  ENVELOPE  ENVELOPE  ENVELOPE  ENVELOPE  ENVELOPE  ENVELOPE  ENVELOPE  ENVELOPE  

template<class tag, class v_type>
class anonymous_iterator_envelope;

template<class v_type>
class anonymous_iterator_envelope<std::forward_iterator_tag,v_type>:
     public std::iterator<std::forward_iterator_tag, v_type>
{
  typedef std::forward_iterator_tag tag;
  typedef anonymous_iterator_envelope<tag,v_type> type_of_this;
  anonymous_iterator_abc<tag,v_type>* mp_imple;

public:
  ~anonymous_iterator_envelope<tag,v_type>()
    {
      delete mp_imple;
    };  

  anonymous_iterator_envelope<tag,v_type>(const anonymous_iterator_abc<tag,v_type>& imple)
    :
    mp_imple(imple.clone())
    {};  

  anonymous_iterator_envelope<tag,v_type>(const type_of_this& other)
    :
    mp_imple(other.mp_imple->clone())
    {};  

  type_of_this& operator=(const type_of_this& rhs)
    {
      delete mp_imple;
      mp_imple = rhs.mp_imple->clone();
      return *this;
    };  

  type_of_this& operator++()
    {
      ++(*mp_imple);
      return *this;
    }
  type_of_this operator++(int)
    {
      type_of_this result = (*this);
      ++(*mp_imple);
      return result;
    }

  bool operator!=(const type_of_this& it) const
    {
      return (*mp_imple) != (*it.mp_imple);
    }

  bool operator==(const type_of_this& it) const
    {
      return (*mp_imple) == (*it.mp_imple);
    }

  v_type operator*() const
    {
      return (**mp_imple);
    }
};


template<class v_type>
class anonymous_iterator_envelope<std::bidirectional_iterator_tag,v_type>:
     public std::iterator<std::bidirectional_iterator_tag, v_type>
{
  typedef std::bidirectional_iterator_tag tag;
  typedef anonymous_iterator_envelope<tag,v_type> type_of_this;
  anonymous_iterator_abc<tag,v_type>* mp_imple;

public:
  //  ~anonymous_iterator_envelope<tag,v_type>() {      delete mp_imple;    };  
  ~anonymous_iterator_envelope() {      delete mp_imple;    };
  
  anonymous_iterator_envelope<tag,v_type>(const anonymous_iterator_abc<tag,v_type>& imple)
    :    mp_imple(imple.clone())    {};  

  anonymous_iterator_envelope<tag,v_type>(const type_of_this& other)
    :    mp_imple(other.mp_imple->clone())    {};  

  type_of_this& operator=(const type_of_this& rhs)
    {
      delete mp_imple;
      mp_imple = rhs.mp_imple->clone();
      return *this;
    };  

  type_of_this& operator++()
    {
      ++(*mp_imple);
      return *this;
    }
  type_of_this operator++(int)
    {
      type_of_this result = (*this);
      ++(*mp_imple);
      return result;
    }

  bool operator!=(const type_of_this& it) const
    {
      return (*mp_imple) != (*it.mp_imple);
    }

  bool operator==(const type_of_this& it) const
    {
      return (*mp_imple) == (*it.mp_imple);
    }

  v_type operator*() const
    {
      return (**mp_imple);
    }

  anonymous_iterator_envelope& operator--()
    {
      --(*mp_imple);
      return *this;
    };
};

template<class v_type>
class anonymous_iterator_envelope<std::random_access_iterator_tag,v_type>:
     public std::iterator<std::random_access_iterator_tag, v_type>
{
  typedef std::random_access_iterator_tag tag;
  typedef anonymous_iterator_envelope<tag,v_type> type_of_this;
  anonymous_iterator_abc<tag,v_type>* mp_imple;

public:
  //  ~anonymous_iterator_envelope<std::random_access_iterator_tag,v_type>()  {      delete mp_imple;    };  
  ~anonymous_iterator_envelope ()  {      delete mp_imple;    };
  
  anonymous_iterator_envelope<tag,v_type>(const anonymous_iterator_abc<tag,v_type>& imple)
    : mp_imple(static_cast<anonymous_iterator_abc<tag,v_type>*>(imple.clone()))
    {
      // For some reason the following is a syntax error.  I'm being totally blind!
      //      assert(dynamic_cast<anonymous_iterator_abc<tag,v_type>*>(imple.clone()));
    };  

  anonymous_iterator_envelope<tag,v_type>(const type_of_this& other)
    : mp_imple(static_cast<anonymous_iterator_abc<tag,v_type>*>(other.mp_imple->clone()))
    {
    };  

  type_of_this& operator=(const type_of_this& rhs)
    {
      delete mp_imple;
      mp_imple = rhs.mp_imple->clone();
      return *this;
    };  

  type_of_this& operator++()
    {
      ++(*mp_imple);
      return *this;
    }
  type_of_this operator++(int)
    {
      type_of_this result = (*this);
      ++(*mp_imple);
      return result;
    }

  bool operator!=(const type_of_this& it) const
    {
      return (*mp_imple) != (*it.mp_imple);
    }

  bool operator==(const type_of_this& it) const
    {
      return (*mp_imple) == (*it.mp_imple);
    }

  v_type operator*() const
    {
      return (**mp_imple);
    }

  anonymous_iterator_envelope& operator--()
    {
      --(*mp_imple);
      return *this;
    };


  type_of_this
  operator+(int n)  const
    {
      type_of_this result = *this;
      result += n;
      return result;
    }
  
  type_of_this
  operator-(int n)  const
    {
      type_of_this result = *this;
      result -= n;
      return result;
    }
    
  
  type_of_this&
    operator+=(int n)
    {
      (*mp_imple) += n;
      return *this;
    }
 
  type_of_this&
    operator-=(int n)
    {
      (*mp_imple) -= n;
      return *this;
    }

  typename std::iterator_traits< type_of_this >::difference_type
    operator-(const type_of_this& it) const
    {
      return (*mp_imple) - (*it.mp_imple);
    }

  bool
    operator<(const type_of_this& it) const
    {
      return (*mp_imple) < (*it.mp_imple);
    }

  bool
    operator>(const type_of_this& it) const
    {
      return (*mp_imple) > (*it.mp_imple);
    }
};



template <class BaseIter>
inline
anonymous_iterator_envelope<typename std::iterator_traits<BaseIter>::iterator_category,
			    typename std::iterator_traits<BaseIter>::value_type>
make_anonymous_iterator (const BaseIter& iter)
{
  typedef typename std::iterator_traits<BaseIter>::iterator_category tag;
  typedef typename std::iterator_traits<BaseIter>::value_type value;
  return anonymous_iterator_envelope<tag,value>(anonymous_iterator<tag,BaseIter>(iter));
}


// ANONYMOUS_ITERATOR  ANONYMOUS_ITERATOR  ANONYMOUS_ITERATOR  ANONYMOUS_ITERATOR  ANONYMOUS_ITERATOR  
//  This is the letter class

template <class tag, class BaseIter>
class anonymous_iterator;

template <class BaseIter>
class anonymous_iterator<std::forward_iterator_tag,BaseIter>
:   public anonymous_iterator_abc<typename std::iterator_traits<BaseIter>::iterator_category,
				  typename std::iterator_traits<BaseIter>::value_type>
{
 protected:  
  BaseIter mIter;
  typedef typename std::iterator_traits<BaseIter>::value_type v_type;
  typedef typename std::iterator_traits<BaseIter>::iterator_category iterator_category;
  typedef anonymous_iterator_abc<iterator_category,v_type> Parent;
  typedef anonymous_iterator_abc<std::forward_iterator_tag,v_type> forward_parent;
  typedef anonymous_iterator<iterator_category,BaseIter> type_of_this;
  
 public:
  
  virtual ~anonymous_iterator()  { } ;

  anonymous_iterator(const BaseIter& it)
    :  mIter(it) { }

  virtual Parent& operator++()
    {
      ++mIter;
      return *this;
    }
  
  virtual bool operator!=(const forward_parent& it) const
    {
      assert(dynamic_cast<const type_of_this*>(&it));
      return static_cast<const type_of_this*>(&it)->mIter != mIter;
    }

  virtual bool operator==(const forward_parent& it) const
    {
      assert(dynamic_cast<const type_of_this*>(&it));
      return static_cast<const type_of_this*>(&it)->mIter == mIter;
    }

  virtual v_type operator*() const
    {
      return *mIter; 
    }

  virtual Parent* clone() const
    {
      return new type_of_this(*static_cast<const type_of_this*>(this));
    }
};

template <class BaseIter>
class anonymous_iterator<std::bidirectional_iterator_tag,BaseIter> :
   public anonymous_iterator<std::forward_iterator_tag,BaseIter>
{
public:
  typedef typename std::iterator_traits<BaseIter>::value_type v_type;
  typedef typename std::iterator_traits<BaseIter>::iterator_category iterator_category;
  typedef anonymous_iterator_abc<iterator_category,v_type> Parent;
  typedef anonymous_iterator<iterator_category,BaseIter> type_of_this;
  
  virtual ~anonymous_iterator()    { } ;

  anonymous_iterator(const BaseIter& it)
    :  anonymous_iterator<std::forward_iterator_tag,BaseIter>(it)    { } 

  anonymous_iterator(const type_of_this& it)
    :  anonymous_iterator<std::forward_iterator_tag,BaseIter>(it.mIter)    { } 

  virtual Parent& operator--()
    {
      --anonymous_iterator<std::forward_iterator_tag,BaseIter>::mIter;
      return *this;
    }

  virtual Parent* clone() const
    {
      return new type_of_this(*static_cast<const type_of_this*>(this));
    }
};

template <class BaseIter>
class anonymous_iterator<std::random_access_iterator_tag,BaseIter> :
   public anonymous_iterator<std::bidirectional_iterator_tag,BaseIter>
{
public:
  typedef typename std::iterator_traits<BaseIter>::value_type v_type;
  typedef typename std::iterator_traits<BaseIter>::iterator_category iterator_category;
  typedef anonymous_iterator_abc<std::random_access_iterator_tag,v_type> random_parent;
  typedef anonymous_iterator_abc<iterator_category,v_type> Parent;
  typedef anonymous_iterator<iterator_category,BaseIter> type_of_this;
  
  virtual ~anonymous_iterator()  { } ;

  anonymous_iterator(const BaseIter& it)
    :  anonymous_iterator<std::bidirectional_iterator_tag,BaseIter>(it) { } 

  anonymous_iterator(const type_of_this& it)
    :  anonymous_iterator<std::bidirectional_iterator_tag,BaseIter>(it.mIter) { } 


  type_of_this&
    operator+=(int n)
    {
      anonymous_iterator<std::bidirectional_iterator_tag,BaseIter>::mIter += n;
      return *this;
    }
 
  type_of_this&
    operator-=(int n)
    {
      anonymous_iterator<std::bidirectional_iterator_tag,BaseIter>::mIter -= n;
      return *this;
    }

  typename std::iterator_traits<BaseIter>::difference_type
    operator-(const random_parent& it) const
    {
      assert(dynamic_cast<const type_of_this*>(&it));
      return
	anonymous_iterator<std::bidirectional_iterator_tag,BaseIter>::mIter
	- static_cast<const type_of_this*>(&it)->mIter;
    }

  bool
    operator<(const random_parent& it) const
    {
      assert(dynamic_cast<const type_of_this*>(&it));
      return
	anonymous_iterator<std::bidirectional_iterator_tag,BaseIter>::mIter< static_cast<const type_of_this*>(&it)->mIter;
    }
  
  bool
    operator>(const random_parent& it) const
    {
      assert(dynamic_cast<const type_of_this*>(&it));
      return anonymous_iterator<std::bidirectional_iterator_tag,BaseIter>::mIter > static_cast<const type_of_this*>(&it)->mIter;
    }

  Parent* clone() const
    {
      return new type_of_this(*this);
    }
};

inline
range<  anonymous_iterator_envelope<std::random_access_iterator_tag,double>  >
make_anonymous_range ()
{
  std::vector<double> x (0);
  return make_anonymous_range(x.begin(), x.end());
}

#endif
