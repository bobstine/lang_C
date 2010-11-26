//  g++ easy_threads.cc -lboost_thread -L/usr/local/lib;./a.out

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


template<class W>
LightThread<W>::~LightThread() 
{
}

template<class W>
LightThread<W>::LightThread(const W& worker)
: mp_done(new bool), // we have no work to do in the queue and no thread running
  mp_worker(),
  mp_thread(),
  mp_lock(new boost::mutex())
{
  std::cout << "LT: initialize with a worker.\n";
  (*mp_done) = true;
  (*this)(worker);
}


// copy constructor operates by default via smart pointers
template<class W>
LightThread<W>::LightThread(const LightThread<W>& rhs)
: mp_done(rhs.mp_done), // we have no work to do in the queue and no thread running
  mp_worker(rhs.mp_worker),
  mp_thread(rhs.mp_thread),
  mp_lock(rhs.mp_lock)
{
  std::cout << "LT: initialize by copy construct.\n";
}


// default constructor
template<class W>
LightThread<W>::LightThread()
: mp_done(new bool),
  mp_worker(),
  mp_thread(),
  mp_lock(new boost::mutex())
{
  std::cout << "LT: initialize with no worker supplied.\n";
  (*mp_done) = true;
}

template<class W>
void
LightThread<W>::operator()(W const& worker)
{
  assert(done());  // make sure we don't have a thread running
  set_done(false);
  mp_worker = boost::shared_ptr<W>(new W(worker));  // note: counted pointers, so we don't delete it
  mp_thread = boost::shared_ptr<boost::thread>(new boost::thread(&LightThread<W>::start_thread,this));

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
  bool result;
  assert(mp_lock);                              // make sure we have a non-zero pointer
  if(mp_lock->try_lock())
  { result = (*mp_done);
    assert(mp_worker || ((*mp_done) == true));  // either have worker or done
    mp_lock->unlock();
  }
  else result = false;
  return result;
}

template<class W>
bool
LightThread<W>::has_worker() const
{
  return (mp_worker != 0);
}

template<class W>
const W&
LightThread<W>::operator()() const
{
  if(!done())
    {
      mp_thread->join();
      assert(done());
    }
  return *mp_worker;
}


template<class W>
const W*
LightThread<W>::operator->() const
{
  if(!done())
    mp_thread->join();
  assert(done());
  assert(has_worker());
  return mp_worker.get();
}



template<class W>
W*
LightThread<W>::operator->() 
{
  if(!done())
    mp_thread->join();
  assert(done());
  assert(has_worker());
  return mp_worker.get();
}


template<class W>
void
LightThread<W>::set_done(bool value)
{
  assert(mp_lock);
  mp_lock->lock();
  assert((*mp_done) == !value);  // Checks for race condition.
  (*mp_done) = value;
  mp_lock->unlock();
};

template<class W>
void
LightThread<W>::start_thread()
{
  // DPF: We want to make sure that we haven't been asked to start a new thread
  // while we have one currently running.
  assert(!done());
  assert(has_worker());
  (*mp_worker)();
  set_done(true);
}

