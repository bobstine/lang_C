//  g++ easy_threads.cc -lboost_thread -L/usr/local/lib;./a.out

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  The key variable here is m_done.
//
//  m_done == true:
//          we currently are in single thread mode
//          If mp_worker != 0, we should have a valid answer in the object
//          We can change values at will
//  m_done == false:
//          There exists a thread running now
//          Nothing should be touched or even read except by the thread
//
/////////////////////////////////////////////////////////////////////////////////////////////


template<class W>
Threaded<W>::~Threaded() 
{
  m_Thread.join();
}
    
template<class W>
Threaded<W>::Threaded(W* p_worker)
: mp_worker(),
  m_Thread(),
  m_done(true), // we have no work to do in the queue and no thread running
  m_lock()
{
  (*this)(p_worker);
}

template<class W>
Threaded<W>::Threaded()
: mp_worker(0),
  m_Thread(),
  m_done(true),
  m_lock()
{
}

template<class W>
void
Threaded<W>::operator()(W* p_worker)
{
  assert(done());  // make sure the previous thread either doesn't exist or is done
  set_done(false);
  mp_worker = p_worker;  // note: we don't own mp_worker, so we don't delete it
  m_Thread = boost::thread(&Threaded<W>::start_thread,this);
}

template<class W>
bool
Threaded<W>::done()
{
  bool result;
  m_lock.lock();
  result = m_done;
  assert((mp_worker != 0) || (m_done == true));
  m_lock.unlock();
  return result;
}

template<class W>
void
Threaded<W>::set_done(bool value)
{
  m_lock.lock();
  assert(m_done == !value);  // Checks for race condition.
  m_done = value;
  m_lock.unlock();
};

template<class W>
void
Threaded<W>::start_thread()
{
  assert(!done());
  (*mp_worker)();
  set_done(true);
}

