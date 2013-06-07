#ifndef _TIMING_H_
#define _TIMING_H_

#include <string>

void
print_with_time_stamp(std::string s, std::ostream &os);

void
print_with_elapsed_seconds(std::string const& s, clock_t const& start, clock_t const& stop, std::ostream &os);

#endif
