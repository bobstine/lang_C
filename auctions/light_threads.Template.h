//  g++ easy_threads.cc -lboost_thread -L/usr/local/lib;./a.out   -*- c++ -*- 

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  The key variable here is mp_done.
//
//  mp_done == true:
//          we currently are in single thread mode
//          If mp_worker != 0, we should have a valid answer in the object
//          We can change values at will
//  mp_done == false:
//          There exists a thread running now
//          Nothing should be touched or even read except by the thread
//
/////////////////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////////////////
//
//              L O C K I N G      L O G I C
//
//   This class manipulates pointers and the value of a boolean.  So
//   both of these are protected by the lock.  Ideally, we would take a
//   hit on speed and make the pointers point to other pointers--then
//   the semantics would be simplier.  But as a speed hack, we allow the
//   pointers themselves to be changed.  (YIKES!  IS THIS A LOGIC ERROR?)
//
//
//   So anytime a pointer is written or read, the lock should be grabbed.
//   Any time the value of the bool is written or read, the lock should be
//   grabbed.
//
/////////////////////////////////////////////////////////////////////////////////////////////

template<class W>
LightThread<W>::~LightThread() 
{
}

template<class W>
LightThread<W>::LightThread(const W& worker)
: mp_done(new bool), // we have no work to do in the queue and no thread running
  mp_worker(),
  mp_thread(),
  mp_thread_lock(new boost::mutex()),
  m_object_lock()
{
  std::cout << "LT: initialize with a worker.\n";
  (*mp_done) = true;
  (*this)(worker);
}


// copy constructor operates by default via smart pointers
template<class W>
LightThread<W>::LightThread(const LightThread<W>& rhs)
: mp_done(),
  mp_worker(),
  mp_thread(),
  mp_thread_lock(),
  m_object_lock()
{
  // lock down both objects
  m_object_lock.lock();
  rhs.m_object_lock.lock();
  // set pointer since both locked
  mp_done   = rhs.mp_done;
  mp_worker = rhs.mp_worker;
  mp_thread_lock = rhs.mp_thread_lock;
  mp_thread = rhs.mp_thread;
  // unlock
  rhs.m_object_lock.unlock();
  m_object_lock.unlock();
  std::cout << "LT: initialize by copy construct.\n";
}


// default constructor
template<class W>
LightThread<W>::LightThread()
: mp_done(),
  mp_worker(),
  mp_thread(),
  mp_thread_lock(),
  m_object_lock()
{
  std::cout << "LT: initialize with no worker supplied.\n";
}

template<class W>
void
LightThread<W>::operator()(W const& worker)
{
  // make sure we don't have a thread running
  assert(done());  
  // the following should all be changed "atomically"
  m_object_lock.lock();
  mp_thread_lock = boost::shared_ptr<boost::mutex> (new boost::mutex);
  mp_thread_lock->lock();
  mp_done = boost::shared_ptr<bool> (new bool);
  (*mp_done) = false;
  mp_worker = boost::shared_ptr<W>(new W(worker));  // note: counted pointers, so we don't delete it
  mp_thread = boost::shared_ptr<boost::thread>(new boost::thread(&LightThread<W>::start_thread,this));
  mp_thread_lock->unlock();
  m_object_lock.unlock();
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
  m_object_lock.lock();
  if (mp_done == 0)                              // make sure we have a non-zero pointer
  { m_object_lock.unlock();
    return true;
  }
  if(!mp_thread_lock->try_lock())
  { m_object_lock.unlock();
    return false;
  }
  else  // we have the thread lock
  { bool result = (*mp_done);
    mp_thread_lock->unlock();
    m_object_lock.unlock();
    return result;
  }
}


template<class W>
bool
LightThread<W>::has_worker() const
{
  m_object_lock.lock();
  bool result  (mp_worker != 0);
  m_object_lock.unlock();
  return result;
}



template<class W>
const W*
LightThread<W>::operator->() const
{
  m_object_lock.lock();
  assert(mp_worker);
  mp_thread_lock->lock();
  if(!*mp_done)
  { m_object_lock.unlock();   // Do we need to do this to allow thread to alter object???
    mp_thread->join();
    m_object_lock.lock();
  }
  const W* pWorker (mp_worker.get());
  mp_thread_lock->unlock();
  m_object_lock.unlock();
  return pWorker;
}



template<class W>
W*
LightThread<W>::operator->() 
{
  m_object_lock.lock();
  assert(mp_worker);
  mp_thread_lock->lock();
  if(!*mp_done)
  { m_object_lock.unlock();   // Do we need to do this to allow thread to alter object???
    mp_thread->join();
    m_object_lock.lock();
  }
  W* pWorker (mp_worker.get());
  mp_thread_lock->unlock();
  m_object_lock.unlock();
  return pWorker;
}


// This is the function that is run entirely within a separate thread
template<class W>
void
LightThread<W>::start_thread()
{
  m_object_lock.lock();
  assert(mp_done != 0);
  assert(mp_worker != 0);
  boost::shared_ptr<W> pLocalWorker = mp_worker;
  boost::shared_ptr<bool> pLocalDone = mp_done;
  m_object_lock.unlock();
  mp_thread_lock->lock();
  (*pLocalWorker)();
  (*pLocalDone) = true;
  mp_thread_lock->unlock();
}

