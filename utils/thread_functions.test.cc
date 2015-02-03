#include <iostream>
#include <string>
#include <chrono>
#include <vector>

#include "thread_functions.h"

using std::string;

/// simple delay test function that takes int argument and returns doubled version

class Delay : public std::function<double (int)>
{
public:
  double operator()(int ms) const
  { std::cout << " Thread will sleep for " << ms << " ms.\n";
    std::this_thread::sleep_for (std::chrono::milliseconds(ms));
    return 0.001 * (double)ms;
    }
};


int main()
{
  typedef thread_function<Delay> Task;  

  const int nThreads = 5;
  
  std::vector<int>  delays  {nThreads};
  std::vector<bool> finished(nThreads);
  std::vector<Task> tasks   {nThreads};
  
  // define tasks
  for(int i=0; i<nThreads; ++i)
  { finished[i] = false;
    tasks[i] = Task{std::to_string(i), Delay()};
  }
  // run them by calling operator()
  const int base = 3000;
  for(int i=0; i<nThreads; ++i)
  { int delay = base/(1+i);
    std::cout << "TEST: start " << i << " with delay " << delay << std::endl;
    tasks[i](base/(i+1));
  }
  // poll for which are done
  bool done = false;
  while(!done)
  { 
    std::this_thread::sleep_for( std::chrono::milliseconds (200) );
    bool areDone = true;
    for(int i=0; i<nThreads; ++i)
    { finished[i] = tasks[i].finished();
      areDone = areDone & finished[i];
    }
    std::cout << " finished = " ;
    for(auto b : finished)
      if(b) std::cout << 't'; else std::cout << 'f';
    std::cout << std::endl;
    if( areDone ) break;
  }
  for (int i=0; i<nThreads; ++i)
    std::cout << "i=" << i << "   delay="<< tasks[i].result() << std::endl;    

  return 0;
}
