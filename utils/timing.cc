#include "timing.h"

#include <time.h>
#include <iostream>
#include <sstream>


void
print_with_time_stamp(std::string str, std::ostream &os)
{
  time_t rawtime;
  struct tm * timeinfo;
  time ( &rawtime );
  timeinfo = localtime ( &rawtime );
  std::ostringstream ss;
  ss << "TIME: " << str << " @ " << std::string(  asctime (timeinfo) ) << std::endl;
  os << ss.str();
}


void
print_with_elapsed_seconds(std::string const& s, clock_t const& start, clock_t const& stop, std::ostream &os)
{
  clock_t diff (stop-start);
  std::ostringstream oss;
  oss <<  "TIME: " << s << " [" << ((float)diff)/CLOCKS_PER_SEC << " sec]" << std::endl;
  os << oss.str();
}
