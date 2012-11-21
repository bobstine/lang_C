//  g++ easy_threads.cc -lboost_thread -L/usr/local/lib;./a.out

#include "light_threads.h"

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  Sample worker object.
//
/////////////////////////////////////////////////////////////////////////////////////////////


class Worker
{
private:
  int m_N;
  std::string m_results;

public:    
  Worker(int N)             : m_N(N), m_results()  {  }
    
  Worker(const Worker& rhs) : m_N(rhs.m_N), m_results(rhs.m_results){ }
    
  void operator()()
  {
    float ms = m_N * 1e3;
    boost::posix_time::milliseconds workTime(ms);
    std::cout << "Worker: started, will work for " << ms << "ms\n";
    boost::this_thread::sleep(workTime);
    std::cout << "Worker: completed" << std::endl;
    m_results = "Worker: completed";
  }

  
  std::string results() const  { return m_results; }

};


int main(int, char**)
{
    std::cout << "main: startup" << std::endl;

    {
      LightThread<Worker> w3("Lt:w3");
      w3(Worker(3));
      LightThread<Worker> w2("Lt:w2",Worker(2));
      LightThread<Worker> w4(w3);
      if(w2.done())
	std::cout << "Probably running without threads." << std::endl;
      else
	std::cout << "Probably running with threads." << std::endl;
	
      std::cout << "main: waiting for thread" << std::endl;

      // get information from threaded worker via ->
      std::cout << "Results from W2:" << w2->results() << std::endl;
      std::cout << "             W3:" << w3->results() << std::endl;
      std::cout << "             W4:" << w4->results() << std::endl;

      // put them into a list
      /*
	std::vector< LightThread<Worker> > workers;
      workers.push_back(LightThread<Worker>( Worker(1) ) );
      
      std::cout << "main: waiting for threads in vector" << std::endl;
      std::cout << "     vector[0]: " << workers[0]->results() << std::endl;
      */
    } 

    std::cout << "DONE." << std::endl;

    return 0;
}
