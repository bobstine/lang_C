//  g++ easy_threads.cc -lboost_thread -L/usr/local/lib;./a.out


#include "threaded.h"

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  Sample worker object.
//
/////////////////////////////////////////////////////////////////////////////////////////////


class Worker
{
public:
    
  Worker(int N)
    : m_N(N),
      m_results()
  {
  }
    
  void operator()()
  {
    float ms = m_N * 1e3;
    boost::posix_time::milliseconds workTime(ms);

    std::cout << "Worker: started, will work for "
	      << ms << "ms"
	      << std::endl;

    // We're busy, honest!
    boost::this_thread::sleep(workTime);

    std::cout << "Worker: completed" << std::endl;
    m_results = "Worker: completed";
  };

  std::string results()
  {
    return m_results;
  };

private:
  int m_N;
  std::string m_results;
};

int main(int, char**)
{
    std::cout << "main: startup" << std::endl;

    Worker worker(3);
    {
      Threaded<Worker> w(&worker);
      //      Thread_free<Worker> w(&worker);
      if(w.done())
	std::cout << "Probably running without threads." << std::endl;
      else
	std::cout << "Probably running with threads." << std::endl;
	
      std::cout << "main: waiting for thread" << std::endl;
    } // this closing "}" is important.  It ensure the Thread has returned.
    std::cout << "RESULTS:" << worker.results() << std::endl;
    std::cout << "main: done" << std::endl;

    std::cout << "DONE." << std::endl;

    return 0;
}
