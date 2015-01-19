#include <omp.h>
#include <iostream>
int main (int argc, char *argv[]) {
  int id, nthreads;
#pragma omp parallel private(id)
  {
    id = omp_get_thread_num();
    std::cout << "Hello World from thread " << id << std::endl;
#pragma omp barrier
#pragma omp master
    if ( id == 0 ) {
      nthreads = omp_get_num_threads();
      std::cout << "There are " << nthreads << " threads\n";
    }
  }
  return 0;
}

