#ifndef _LIGHT_THREADS_TEMPLATE_
#define _LIGHT_THREADS_TEMPLATE_

#include "light_threads.h"
#include <assert.h>

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  The key variable here is mp_notWorking.
//
//  mp_notWorking == true:
//          we currently are in single thread mode
//          If mp_worker != 0, we should have a valid answer in the object
//          We can change values at will
//  mp_notWorking == false:
//          There exists a thread running now
//          Nothing should be touched or even read except by the thread
//
/////////////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////////////////
//
//              L O C K I N G      L O G I C
//
//   This class manipulates pointers and the value of a boolean.  So
//   both of these are protected by locks. //
//
//   Anytime a pointer is written or read, the object lock should be grabbed.
//   
//   Any time the value of the bool is written or read, the thread_mutex should be
//   grabbed.
//
/////////////////////////////////////////////////////////////////////////////////////////////

template<class W>
LightThread<W>::~LightThread() 
{
  if (mp_thread.unique() && !*mp_notWorking)
  { std::cout << "ERROR: LightThread " << mName << " attempted to delete a working thread you dummy!" << std::endl;
    mp_thread->join();
  }
  assert(!mp_thread.unique() || *mp_notWorking);
  if(mp_thread->joinable()) mp_thread->detach();   // boost different from C++11 standard on need to detach before dispose
}

template<class W>
LightThread<W>::LightThread(std::string name, const W& worker)
  : mName(name),
    mp_notWorking(new bool), // we have no work to do in the queue and no thread running
    mp_worker(),
    mp_thread(),
    mp_thread_mutex(new std::mutex()),
    m_object_mutex()
{
  //  std::cout << "LT: initialize with a worker.\n";
  (*mp_notWorking) = true;
  (*this)(worker);
}


// copy constructor operates by default via smart pointers
template<class W>
LightThread<W>::LightThread(const LightThread<W>& rhs)
  : mName(rhs.mName),
    mp_notWorking(),
    mp_worker(),
    mp_thread(),
    mp_thread_mutex(),
    m_object_mutex()
{
  // std::cout << "LT: initialize by copy construct.\n";
  // lock down both objects
  m_object_mutex.lock();
  rhs.m_object_mutex.lock();
  // copy pointers
  mp_notWorking   = rhs.mp_notWorking;
  mp_worker = rhs.mp_worker;
  mp_thread = rhs.mp_thread;
  mp_thread_mutex = rhs.mp_thread_mutex;
  // unlock
  rhs.m_object_mutex.unlock();
  m_object_mutex.unlock();
}


// default constructor
template<class W>
LightThread<W>::LightThread(std::string name)
  : mName(name),
    mp_notWorking(),
    mp_worker(),
    mp_thread(),
    mp_thread_mutex(),
    m_object_mutex()
{
  // std::cout << "LT: initialize with no worker supplied.\n";
}

template<class W>
void
LightThread<W>::operator()(W const& worker)
{
  // make sure we don't have a thread running
  assert(done());  
  // lock object, but not thread since we are about to launch it
  m_object_mutex.lock();
  // the following should all be changed "atomically"
  mp_thread_mutex = std::shared_ptr<std::mutex> (new std::mutex);
  mp_notWorking = std::shared_ptr<bool> (new bool);
  (*mp_notWorking) = false;
  mp_worker = std::shared_ptr<W>(new W(worker));  // note: counted pointers, so we don't delete it

  // RAS  HERE
  if (mp_thread && mp_thread->joinable())
  { mp_thread->join();    //  C++ differs from boost here
    mp_thread = std::shared_ptr<std::thread>(new std::thread(&LightThread<W>::start_thread,this));
  }
  else     mp_thread = std::shared_ptr<std::thread>(new std::thread(&LightThread<W>::start_thread,this));

  m_object_mutex.unlock();
#ifdef NOTHREADS
  // force thread to finish if we have been asked not to use threads.
  std::cout << "LT: force thread to finish.\n";
  mp_thread->join();
#endif
}

 
template<class W>
bool
LightThread<W>::done() const
{
  m_object_mutex.lock();
  if (mp_notWorking == 0)                              // make sure we have a non-zero pointer
  { m_object_mutex.unlock();
    return true;
  }
  if(!mp_thread_mutex->try_lock())
  { m_object_mutex.unlock();
    return false;
  }
  else  // we have the thread lock
  { bool result = (*mp_notWorking);
    mp_thread_mutex->unlock();
    m_object_mutex.unlock();
    return result;
  }
}


template<class W>
bool
LightThread<W>::has_worker() const
{
  m_object_mutex.lock();
  bool result  (mp_worker != 0);
  m_object_mutex.unlock();
  return result;
}



template<class W>
const W*
LightThread<W>::operator->() const
{
  m_object_mutex.lock();
  assert(mp_worker);
  mp_thread_mutex->lock();
  if(!*mp_notWorking)
  { mp_thread_mutex->unlock(); // Unlock mutexes so that running thread
    m_object_mutex.unlock();   // can set things, such as notWorking
    std::cout << "LT: Joining light thread " << mName << " (const)" << std::endl;
    mp_thread->join();
    m_object_mutex.lock();
    mp_thread_mutex->lock();
  }
  const W* pWorker (mp_worker.get());
  mp_thread_mutex->unlock();
  m_object_mutex.unlock();
  return pWorker;
}



template<class W>
W*
LightThread<W>::operator->() 
{
  m_object_mutex.lock();
  assert(mp_worker);
  mp_thread_mutex->lock();
  if(!*mp_notWorking)
  { mp_thread_mutex->unlock();
    m_object_mutex.unlock();
    std::cout << "LT: Joining light thread " << mName << std::endl;
    mp_thread->join();
    m_object_mutex.lock();
    mp_thread_mutex->lock();
  }
  W* pWorker (mp_worker.get());
  mp_thread_mutex->unlock();
  m_object_mutex.unlock();
  return pWorker;
}


// This function runs entirely within a separate thread
template<class W>
void
LightThread<W>::start_thread()
{
  m_object_mutex.lock();
  assert(mp_notWorking != 0);
  assert(mp_worker != 0);
  std::shared_ptr<W> pLocalWorker = mp_worker;
  std::shared_ptr<bool> pLocalNotWorking = mp_notWorking;
  m_object_mutex.unlock();
  (*pLocalWorker)(); // no one looks at mp_worker until it is "notWorking".  So we don't have to protect it further
  mp_thread_mutex->lock();
  (*pLocalNotWorking) = true;
  mp_thread_mutex->unlock();
}

#endif
