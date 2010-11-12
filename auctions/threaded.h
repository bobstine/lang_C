// -*- c++ -*-

#ifndef INCLUDED_THREADS
#define INCLUDED_THREADS


#include <iostream>
#include <boost/thread.hpp>
#include <boost/shared_ptr.hpp>

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  Interface to Boost::threads
//
//  The primary value-added of the Threaded class below is that it uses creation and destruction
//  as the mechanism for controling the threads.  This avoids memory issues, dead lock, etc.
//  But it disallows some fancier threading.  So if you can write code that works with this
//  version of threads--it will natively be safe.
//
/////////////////////////////////////////////////////////////////////////////////////////////

template<class W>
class Threaded
{
public:
  ~Threaded<W>();                // Waits for thread to finish 
  Threaded<W>(W* p_worker);      // starts a new thread.
  Threaded<W>();
  void operator()(W* p_worker);  // starts a new thread.
  bool done();                   // Tells if the thread is done.
    
private:

  void set_done(bool value);
  void start_thread();
  W* mp_worker;
  boost::thread m_Thread;
  bool m_done;
  mutable boost::mutex m_lock;
};

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  To ensure your code makes sense, you should first write it using a Thread_free version.
//  It has simplier flow control since it does all the work at one time when the Thread_free
//  is created.
//
/////////////////////////////////////////////////////////////////////////////////////////////

template<class W>
class Thread_free
{
public:
    
  Thread_free<W>(W* p_worker)
    {
      (*p_worker)();
    }

  Thread_free<W>(W p_worker)
  {
    (*p_worker)();
  }
  
  Thread_free<W>()
    {
    }

  void operator()(W* p_worker)
  {
    (*p_worker)();
  }
  bool done() const{return true;}
};

#include "threaded.Template.h"

#endif
