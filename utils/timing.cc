#include "timing.h"

#include <time.h>
#include <iostream>


void
print_with_time_stamp(std::string str, std::ostream &os)
{
  time_t rawtime;
  struct tm * timeinfo;
  time ( &rawtime );
  timeinfo = localtime ( &rawtime );
  os << "TIME: " << str << " @ " << std::string(  asctime (timeinfo) );  // includes new line
}


void
print_with_elapsed_seconds(std::string const& s, clock_t const& start, clock_t const& stop, std::ostream &os)
{
  clock_t diff (stop-start);
  os <<  "TIME: " << s << " [" << ((float)diff)/CLOCKS_PER_SEC << " sec]" << std::endl;
}
