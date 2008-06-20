// $Id: tags.h,v 1.1 2003/08/25 22:09:19 bob Exp $

#ifndef _TAGS_H_
#define _TAGS_H_

/*
  Tags are objects that have a type (from some enumerated list)
  and associate this type with a collection, as in a named set of
  integers.
*/

#include <ostream>

template <class Type, class Value>
class Tag
{
  Type mType;
  Value mValue;

 public:

  Tag (Type t, Value v) : mType(t), mValue(v) { }

  Type
    type () const { return mType; }

  Value
    value () const { return mValue; }

  void
    print_to (std::ostream& os) const
    { os << "TAGG: " << mType << " = " << mValue << " "; }
  
};


template <class Type, class Value>
Tag<Type,Value>
make_tag (Type n, Value v)
{
  return Tag<Type,Value>(n,v);
}


template<class Type, class Value>
inline
std::ostream&
operator<< (std::ostream& os, Tag<Type,Value> const& tag)
{
  tag.print_to(os);
  return (os);
}

#endif
