
#include <iostream>
#include <boost/thread.hpp>

class Worker
{
private:
    unsigned    m_N;
public:
    
    Worker(unsigned N) 
    {        m_N = N;    }

    void processQueue(unsigned M)
    {
        float ms = m_N * M * 1e3;
        boost::posix_time::milliseconds workTime(ms);

        std::cout << "Worker: started, will work for "
                  << ms << "ms"
                  << std::endl;

        // We're busy, honest!
        boost::this_thread::sleep(workTime);

        std::cout << "Worker: completed" << std::endl;
    }
};

int main(int argc, char* argv[])
{
    std::cout << "main: startup" << std::endl;

    Worker w(3);
    boost::thread workerThread(&Worker::processQueue, &w, 2);

    std::cout << "main: waiting for thread" << std::endl;
    workerThread.join();
    std::cout << "main: done" << std::endl;

    return 0;
}
