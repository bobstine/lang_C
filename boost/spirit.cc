
#include <string>
#include <iostream>

#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_stl.hpp>

// #include <boost/lambda/lambda.hpp>

namespace qi      = boost::spirit::qi;
namespace ascii   = boost::spirit::ascii;
namespace phoenix = boost::phoenix;


template <typename Iter, class V>
bool parse_numbers_and_save(Iter first, Iter last, V *pVec)
{
  using qi::double_;
  using qi::char_;
  using qi::_1;
  using qi::phrase_parse;
  using ascii::space;
  using phoenix::push_back;

  bool r = phrase_parse(first, last,
			(
			 double_ % char_(',')
			 ),
			space,                               // skip parser, also skips tabs
			*pVec                                // matches parser attribute, so fills
			);

  /* without lots of sugar
     bool r = phrase_parse(first, last,
			(
			 double_[ push_back(phoenix::ref(*pVec),_1) ]
			 % char_(',')
			 ),
			space                                 // skip parser, also skips tabs
			);
  */
  /*  without sugar 
  bool r = phrase_parse(first, last,
			(
			 double_[ push_back(phoenix::ref(*pVec),_1) ]
			 >> *(
			      char_(',') >>
			      double_[ push_back(phoenix::ref(*pVec),_1) ]
			      )
			 ),
			space                                 // skip parser, also skips tabs
			);
  */
  if (first != last) // fail if we did not get a full match
    return false;
  return r;
}

int
main()
{
  std::cout << "/////////////////////////////////////////////////////////\n\n";
  std::cout << "\t\tA comma separated list parser for Spirit...\n\n";
  std::cout << "/////////////////////////////////////////////////////////\n\n";

  std::cout << "Give me a comma separated list of numbers.\n";
  std::cout << "Type [q or Q] to quit\n\n";

  std::string str;
  std::vector<double> v;
  while (getline(std::cin, str))
    {
      v.clear();
      
      if (str.empty() || str[0] == 'q' || str[0] == 'Q')
	break;

      if (parse_numbers_and_save(str.begin(), str.end(), &v))
	{
	  std::cout << "-------------------------\n";
	  std::cout << "Parsing succeeded\n";
	  std::cout << str << " Parses OK: " << std::endl;
	  std::cout << "Vector [" << v.size() << "] ";
	  for (auto x : v) std::cout << " " << x ;
	  std::cout << std::endl;
	}
      else
	{
	  std::cout << "-------------------------\n";
	  std::cout << "Parsing failed for input\n";
	  std::cout << "   `" << str << "'\n";
	  std::cout << "-------------------------\n";
	}
    }

  std::cout << "Bye... :-) \n\n";
  return 0;
}

