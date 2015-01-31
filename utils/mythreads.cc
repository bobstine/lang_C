#include <iostream>
#include <string>
#include <memory>
#include <thread>

#include <chrono>
#include <vector>

using std::string;

/// sample worker operator object writes result into supplied item (here a vector)

class RecordedDelay
{
  int mPos;
  int mMilliseconds;
  std::vector<int> *pTimeVec;

public:
  RecordedDelay(int i, int ms, std::vector<int> *pTimeVec)
    : mPos(i), mMilliseconds(ms), pTimeVec(pTimeVec) {}

  void operator()() const
    { std::this_thread::sleep_for (std::chrono::milliseconds(mMilliseconds));
      (*pTimeVec)[mPos] = mMilliseconds;
    }
};

/// alternative to light_threads... basically just a wrapper around a thread that
/// encapsulates the task into a lambda function tha fills a boolean slot when the
/// thread finishes.

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
    { std::cout << tag << "Destructor deleting thread task " << mName << " with status=" << *mTaskCompletePtr << std::endl;
      mThreadPtr->join();
    }
  }
  
  ThreadedTask(string name, Task task)
    : mName(name), mTaskCompletePtr(new bool{false}) { init_thread(task) ; }

  ThreadedTask(ThreadedTask const& task) // copy
    : mName(task.mName), mTaskCompletePtr(task.mTaskCompletePtr), mThreadPtr(std::move(task.mThreadPtr))
    { std::cout << "Copy thread " << mName << ", status=" << *mTaskCompletePtr << std::endl; }
  
  bool finished() const { return *mTaskCompletePtr; }
  
private:
  void init_thread(Task task)
  { mThreadPtr =
      std::shared_ptr<std::thread>
      (new std::thread([task, this]() { task(); *(this->mTaskCompletePtr)=true;}));
    }

};

inline
std::ostream&
operator<< (std::ostream& os, std::vector<int> v)
{ for (int i : v) os << i << " "; return os;
}

int main()
{
  std::vector<int> delays {0,0,0};
  std::vector<bool> finished = {false, false, false};

  typedef ThreadedTask<RecordedDelay> Task;
  
  std::vector<Task*> pTasks(3);
  std::cout << " start 0 - " << std::endl;
  Task task0 = Task("t1", RecordedDelay(0, 3000, &delays)); pTasks[0] = &task0;
  std::cout << " start 1 - " << std::endl;
  Task task1 = Task("t2", RecordedDelay(1, 1000, &delays)); pTasks[1] = &task1;
  std::cout << " start 2 - " << std::endl;
  Task task2 = Task("t3", RecordedDelay(2, 2000, &delays)); pTasks[2] = &task2;

  while(true)
  {
    std::this_thread::sleep_for( std::chrono::milliseconds (2000) );
    finished[0] = task0.finished(); finished[1] = task1.finished(); finished[2] = task2.finished();
    std::cout << " finished = " ;
    for(auto b : finished)
      if(b) std::cout << 't'; else std::cout << 'f';
    std::cout << std::endl;
    if( finished[0] & finished[1] & finished[2] ) break;
  }

  for(size_t i=0; i<delays.size(); ++i)
    std::cout << i << "  delay="<< delays[i] << std::endl;    

  return 0;
}
