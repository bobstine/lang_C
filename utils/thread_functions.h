#include <memory>   // move
#include <atomic>
#include <thread>
#include <string>

const std::string tag = "TRDF: ";

template <class F>
class thread_function
{
  typedef typename F::result_type   result_type;
  typedef typename F::argument_type argument_type;
  
private:
  std::string           mName;
  F                     mF;
  std::atomic<bool>     mTaskIsComplete;
  std::thread           mThread;
  result_type           mResult;

public:
  ~thread_function()
  {
    if(mThread.joinable())
    { std::cout << tag << "Deleting thread function " << mName << " with status=" << mTaskIsComplete << std::endl;
      mThread.join();
    }
  }

 thread_function()
   : mName("EMPTY"), mF(), mTaskIsComplete(false) { }
  
 thread_function(std::string name, F f)
   : mName(name), mF(f), mTaskIsComplete(false) {  }
  
 thread_function(thread_function&& tf) // move copy
   : mName(tf.mName), mF(tf.mF), mTaskIsComplete(tf.mTaskIsComplete.load()), mThread(std::move(tf.mThread)), mResult(tf.mResult) { }
  
  thread_function& operator=(thread_function&& tf) // move assign
    { mName = tf.mName; mF = tf.mF; mTaskIsComplete =tf.mTaskIsComplete.load(); mThread = std::move(tf.mThread); mResult = tf.mResult;
      return *this;
    }
  
  std::string name()         const { return mName; }  
  bool        finished()     const { return mTaskIsComplete.load(); }
  result_type result  ()     const { return mResult; }
  
  void   operator()(argument_type const& arg)
  { mThread = std::thread([this, arg]()
			  { this->mResult = this->mF(arg);
			    this->mTaskIsComplete=true;
			  }); }
  
  thread_function& operator=(thread_function const&) = delete;
  thread_function           (thread_function const&) = delete;
};

template<class F>
inline
std::ostream&
operator<< (std::ostream& os, thread_function<F> const& tf)
{ os << "thread_function " << tf.name() << " [" ;
  if(tf.finished()) os << "finished] ";
  else              os << "running ] ";
  return os;
}

