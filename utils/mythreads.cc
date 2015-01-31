#include <iostream>
#include <string>
#include <memory>
#include <thread>

#include <chrono>
#include <vector>

using std::string;

class RecordedDelay
{
  int mMilliseconds;
  std::vector<int> *pTimeVec;
  
  RecordedDelay(int ms, std::vector<int> pTimeVec)
    : mMilliseconds(ms), pTimeVec(pTimeVec) {}

  void operator() const
    { std::this_thread::sleep_for (std::chrono::milliseconds(n));
      pTimeVec->push_back(mMilliseconds);
    }
};
  
const string tag = "TTSK: ";

template <class Task>
class ThreadedTask
{
private:
  string mName;
  std::shared_ptr<bool>        mTaskCompletePtr;
  std::shared_ptr<std::thread> mThreadPtr;

public:
  ~ThreadedTask()
  {
    if(mThreadPtr->joinable())
    { std::cout << tag << "Destructor deleting thread.\n";
      mThread.join();
    }
  }

  ThreadedTask(string name, Task task)
    : mName(name), mTaskCompletePtr(new bool{false})
    {
      mThreadPtr = std::shared_ptr<std::thread>
	(new std::thread([]() task(); mTaskCompletePtr->true;));
    }
  
  bool finished() const { return *mTaskCompletePtr; }
}
    
int main()
{
  std::vector<int> delays;
  std::vector<bool> running = {true,true,true};

  typename ThreadedTask<RecordedDelay> Task;
  
  std::vector<Task> tasks;
  
  tasks.push_back( ThreadedTask("t1", RecordedDelay(500)) );
  tasks.push_back( ThreadedTask("t2", RecordedDelay(300)) );
  tasks.push_back( ThreadedTask("t3", RecordedDelay(100)) );

  int waitingFor = 3;
  
  while(waitingFor)
  { for(int i=0; i<3; ++i)
      if(running[i])
	if(tasks[i].finished)
	{ --waitingFor;
	  running[i]=false;
	  std::cout << "Task " << i << " finished\n";
	}
  }

  return 0;
}
