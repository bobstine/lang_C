#include "light_threads.Template.h"

#include <chrono>
#include <math.h>

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
  Worker(int N)             : m_N(N), m_results("NULL")  {  }
    
  Worker(const Worker& rhs) : m_N(rhs.m_N), m_results(rhs.m_results){ }
    
  void operator()()
  {
    std::cout << "Worker: started, will work for 1 sec" << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(1));
    m_results = "Worker(" + std::to_string(m_N) + ") finished";
    std::cout << m_results << std::endl;
  }
  
  std::string results() const  { return m_results; }

};


int main(int, char**)
{
    std::cout << "MAIN: startup" << std::endl;

    {
      LightThread<Worker> w2("Lt:w2",Worker(2));
      LightThread<Worker> w3("Lt:w3",Worker(3));
      LightThread<Worker> w4("Lt:w4",Worker(4));
      // other startup...
      // w3(Worker(3));
      // LightThread<Worker> w4(w2);
      
      if(w3.done())
	std::cout << "MAIN: Probably running without threads." << std::endl;
      else
	std::cout << "MAIN: Probably running with threads... " << w4->results() << std::endl;   // only joins this one thread???
        
      // put this in to delay so threads can finish
      std::cout << "MAIN: delay with loop\n";
      double total = 0.0;
      for (int i=0; i< 200000; ++i)
	total += sqrt( (double)i );
      

      // get information from threaded worker via ->
      std::cout << "MAIN: Results from W2 = `" << w2->results() << "'" << std::endl;
      std::cout << "MAIN:              W3 = `" << w3->results() << "'" << std::endl;
      std::cout << "MAIN:              W4 = `" << w4->results() << "'" << std::endl;

      // put them into a list
      /*
	std::vector< LightThread<Worker> > workers;
      workers.push_back(LightThread<Worker>( Worker(1) ) );
      
      std::cout << "main: waiting for threads in vector" << std::endl;
      std::cout << "     vector[0]: " << workers[0]->results() << std::endl;
      */

      std::cout << "MAIN: Done." << std::endl;

    } 


    return 0;
}
