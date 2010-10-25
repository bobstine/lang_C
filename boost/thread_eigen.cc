#include <iostream>
#include <boost/thread.hpp>
#include <Eigen/Array>

class Worker       // worker object should be copy constructable...
{
private:
  const std::string m_name;
  const unsigned    m_rows, m_cols;
  float             m_answer;

public:    
  Worker(std::string name, unsigned rows, unsigned cols) 
    : m_name(name), m_rows(rows), m_cols(cols),  m_answer(0) {    }

  Worker(Worker const& w)
    : m_name(w.m_name), m_rows(w.m_rows), m_cols(w.m_cols), m_answer(w.m_answer) { }
  
  
  float answer() const { return m_answer; }
  
  void operator()()
    {
      std::cout << "Worker #" << boost::this_thread::get_id() << " " << m_name << " operator called\n";
      Eigen::MatrixXf m (Eigen::MatrixXf::Random(m_rows,m_cols));
      m_answer = (m.transpose() * m).trace();
      std::cout << "Worker " << m_name << ": Answer = " << m_answer << std::endl;
    }

};

int main(int argc, char* argv[])
{
    std::cout << "main: startup" << std::endl;
    const unsigned rows (10000);
    const unsigned cols ( 200 );
    
    Worker w("base", rows, cols);
    boost::thread workerThread1(Worker("one",rows,cols));
    boost::thread workerThread2(Worker("two",rows,cols));
    boost::thread workerThread3(w);
    boost::thread workerThread4(w);

    std::cout << "main: waiting for threads; threads start when created even though output follows..." << std::endl;
    //    workerThread1.join();   both still get things done, though we don't wait for them
    //    workerThread2.join();
    workerThread3.join();
    workerThread4.join();
    // copy constructs w, so w.answer() will always return 0
    std::cout << "main: done; answer from worker = " << w.answer() << std::endl;

    return 0;
}
