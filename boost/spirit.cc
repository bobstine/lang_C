
#include <string>
#include <iostream>
#include <vector>

#include <boost/spirit/home/qi.hpp>


template <typename Iter>
bool parse_numbers(Iter first, Iter last)
{
  using qi::double_;
  using qi::phrase_parse;
  using ascii::space;
  bool r = phrase_parse(
			first,
			last,
			double_ >> *(',' >> double_),     // parser
			space                             // skip parser
			);
  if (first != last) // fail if we did not get a full match
    return false;
  return r;
}


int main()
{
  std::string input = "2, 3, 4, 5, 65, 45";

  if (parse_numbers(input.begin(), input.end()))
    std::cout << "Success\n";
  else
    std::cout << "Failed\n";

  return 0;
}
