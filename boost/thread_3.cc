
#include <iostream>
#include <boost/thread.hpp>

class Worker       // worker object should be copy constructable...
{
private:
  unsigned    m_Number;
  float       m_Guess;
  unsigned    m_Iterations;
  float       m_answer;

public:    
  Worker(unsigned N, float guess, unsigned iter) 
    : m_Number(N), m_Guess(guess), m_Iterations(iter), m_answer(0) {    }
  
  float answer() const { return m_answer; }
  
  void operator()()
    {
        std::cout << "Worker: calculating sqrt(" << m_Number
                  << "), itertations = " << m_Iterations << std::endl;
        // Use Newton's Method
        float   x;
        float   x_last = m_Guess;
        for (unsigned i=0; i < m_Iterations; i++)
        {
            x = x_last - (x_last*x_last-m_Number)/(2*x_last);
            x_last = x;
            std::cout << "Iter " << i << " = " << x << std::endl;
        }
	m_answer = x;
        std::cout << "Worker: Answer = " << m_answer << std::endl;
    }

};

int main(int argc, char* argv[])
{
    std::cout << "main: startup" << std::endl;

    Worker w(612, 10, 5);
    boost::thread workerThread(w);

    std::cout << "main: waiting for thread" << std::endl;
    workerThread.join();
    std::cout << "main: done; answer from worker = " << w.answer() << std::endl;

    return 0;
}
