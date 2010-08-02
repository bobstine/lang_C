#include <omp.h>
#include <iostream>

main()
{
  std::cout << "Hello" << std::endl;
#pragma omp parallel
  std::cout << "Hello from thread " << omp_get_thread_num()
	    << " out of " << omp_get_num_threads() << " threads.\n";
}
