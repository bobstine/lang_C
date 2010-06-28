/*
   
  These adaptors let you call a member function as though its an operator.

   2 Jan 02 ... Created for use with random generator.

   Syntax:
     member_as_operator (object, member function)
     member_as_operator1 (object, member function, argument)
*/

#ifndef _FUNCTOR_H_
#define _FUNCTOR_H_

/////////////////////// No argument ///////////////////////

template <class T, class R>
class MemberAsOperator_t {
  T& mObj;
  R (T::*mF)();
 public:
  explicit MemberAsOperator_t (T& obj, R (T::*f)() )
    : mObj(obj), mF(f) { }
  R operator() (void) const { return (mObj.*mF)(); }
};

template <class T, class R>
MemberAsOperator_t<T,R> member_as_operator (T& obj, R (T::*f)())
{
  return MemberAsOperator_t<T,R>(obj,f);
}

////////////////////////  Unary  /////////////////////////

template <class T, class R, class A>
class MemberAsOperator1_t {
  T& mObj;
  R (T::*mF)(A);
  A mArg;
 public:
  explicit MemberAsOperator1_t (T& obj, R (T::*f)(A), A arg )
    : mObj(obj), mF(f), mArg(arg) { }
  R operator() (void) const { return (mObj.*mF)(mArg); }
};

template <class T, class R, class A>
MemberAsOperator1_t<T,R,A> member_as_operator1 (T& obj, R (T::*f)(A), A arg)
{
  return MemberAsOperator1_t<T,R,A>(obj, f, arg);
}

///////////////////////////////////////////////////////////

#endif
