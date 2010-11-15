// -*- c++ -*-

#ifndef INCLUDED_LIGHT_THREADS
#define INCLUDED_LIGHT_THREADS


#include <iostream>
#include <boost/thread.hpp>

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  Interface to Boost::threads
//
//  The primary value-added of the Threaded class below is that it uses creation and destruction
//  as the mechanism for controling the threads.  This avoids memory issues, dead lock, etc.
//  But it disallows some fancier threading.  So if you can write code that works with this
//  version of threads--it will natively be safe.
//
//  These are meant to be copyable.  Hence the name "light."
//
//  WORKER REQUIREMENTS:
//
//         Worker should define operator() to get its work done.  (i.e. a function object)
//         Worker then should provide an interface to the results.
//         Worker has to be copyable.  (it will be copied once per thread)
//
/////////////////////////////////////////////////////////////////////////////////////////////



/////////////////////////////////////////////////////////////////////////////////////////////
//
//  The following will turn off threads when debugging.  This will make GDB easier to work with
//  But clearly won't allow debugging issues with treads and locks.
//
/////////////////////////////////////////////////////////////////////////////////////////////
#ifdef NDEBUG
#define NOTHREAD
#endif



template<class W>
class LightThread
{
private:
  boost::shared_ptr<bool>          mp_done;
  boost::shared_ptr<W>             mp_worker;
  boost::shared_ptr<boost::thread> mp_thread;
  mutable boost::shared_ptr<boost::mutex> mp_lock;
  
public:
  ~LightThread<W>();                           // Waits for thread to finish 
  LightThread<W>(const W& worker);             // starts a new thread.
  LightThread<W>(const LightThread<W>&);       // "copy" a new thread via default
  LightThread<W>();
  
  void     operator()(const W& worker);             // starts a new thread (waits for old to finish)
  bool     done() const;                            // Tells if the thread is done.
  const W& operator()() const;                  // will wait for thread to finish if not done()
  
  const W* operator->() const;     // will wait for thread to finish if not done()
        W* operator->();           // who needs const anyway???
  
private:
  void set_done(bool value);
  void start_thread();

};

#include "light_threads.Template.h"

#endif
