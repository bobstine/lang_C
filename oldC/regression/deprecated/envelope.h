// $Id: envelope.h,v 1.1 2003/11/26 16:55:47 bob Exp $

/*
  The envelope class implements a smart pointer, and it is
  parameterized over the type of the pointer.  Alternatively, an
  'assign' function could be used rather than operator= so that a
  derived class can inherit the assignment.

  note: operator= returns void since stl does not use the return type,
  and would only use it in the case of a = b = c. Yuk.
*/

#ifndef _ENVELOPE_H_
#define _ENVELOPE_H_

#include <typeinfo>

template<class T>
class Envelope {

 protected:
    T* mPtr;

 public:
    ~Envelope ()
      { -- sCount; if (mPtr) delete(mPtr); }

    Envelope()
      : mPtr(0) { ++ sCount; }

    Envelope(T* x)
      : mPtr(x->clone()) { ++sCount; }

    Envelope (Envelope<T> const& another)
      : mPtr(another.mPtr->clone()) {  ++ sCount; }

    bool
      type_matches (std::type_info const& other_id)      const
      { return typeid(*mPtr) == other_id; }

    void
      operator=(Envelope<T> const& another) { mPtr = another.mPtr->clone(); }

    static int count() { return sCount; }    

 private:
    static int sCount;
};

template<class T>
int Envelope<T>::sCount = 0;


#endif

