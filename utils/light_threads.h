// -*- c++ -*-
#ifndef _LIGHT_THREADS_H_
#define _LIGHT_THREADS_H_

#include <iostream>
#include <thread>
#include <mutex>

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

// #define NOTHREADS


template<class W>
class LightThread
{
  
private:
  std::string                           mName;             // use to identify if there's a problem
  std::shared_ptr<bool>                 mp_notWorking;     // we are only notWorking if the lock isn't grabbed and this is true
  std::shared_ptr<W>                    mp_worker;
  std::shared_ptr<std::thread>        mp_thread;
  std::shared_ptr<std::mutex>         mp_thread_mutex;   // thread lock controls read/write values of pointers
  mutable std::mutex                    m_object_mutex;    // object lock controls read/write pointers
  
public:
  ~LightThread<W>();                                         // Waits for thread to finish 
  LightThread<W>(std::string name, const W& worker);         // starts a new thread.
  LightThread<W>(const LightThread<W>&);                     // "copy" a new thread via default
  LightThread<W>(std::string name);
  
  void     operator()(const W& worker);        // starts a new thread (waits for old to finish)
  bool     done() const;                       // indicates whether the thread is not working
  bool     has_worker() const;                 // confirm that worker is there
  const W& operator()() const;                 // will wait for thread to finish if working()
  
  const W* operator->() const;                 // blocks if thread is not done (poll status using done to avoid block)
        W* operator->();                       // who needs const anyway???
  
private:
  void start_thread();
};

#endif
