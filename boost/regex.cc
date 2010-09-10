#include <boost/regex.hpp>
#include <iostream>
#include <string>
#include <vector>

std::string
trim_string(std::string const& str)
{
  std::string result ("");
  size_t start (str.find_first_not_of(" \t"));
  size_t end   (str.find_last_not_of(" \t"));

  // if all spaces or empty return an empty string
  if(start <= end)
    result = str.substr(start,end-start+1);
  return result;
}

 
int main()
{
  std::string line;
  boost::regex pat( "^(.*)Subject: (Re: |Aw: )*(.*)" );

  // Test the fancy e-mail pattern
  if(true)
    while (std::cin)
    {
      std::getline(std::cin, line);
      std::cout << "Pattern match: Input string [" << line << "]  ";
      boost::smatch matches;
      if (boost::regex_match(line, matches, pat))
	std::cout << " matches with matches[2] = " << matches[2] << std::endl;
      else
	std::cout << " does not match pattern.\n";
    }

  std::cout << "\n\nTEST:\n";
  std::vector< std::string > variableNames;
  
  std::string v1 ("Variable1=2"); variableNames.push_back(v1);
  std::string v2 ("Variable2");   variableNames.push_back(v2);
  std::string v3 ("VAR == 3");    variableNames.push_back(v3);
  std::string v4 ("XXX _=_ 3");   variableNames.push_back(v4);

  // Pattern for a categorical variable produced by c4.5 pre-processing; emacs reg expr
  boost::regex vPattern ("^(.*)_=_(.*)$");
  for(std::vector< std::string >::const_iterator it = variableNames.begin(); it != variableNames.end(); ++it) {
    std::cout << "Match results:  input string is   " << *it << "  ";
    boost::smatch name;
    if(boost::regex_match(*it,name, vPattern))
      std::cout << " matches with variable name {"
		<< trim_string(name[1]) << "} and value {"
		<< trim_string(name[2]) << "}\n";
    else
      std::cout << " does not match pattern.\n";
  }
  // Patter to detect a repeated interaction derived from dummies of same variable
  boost::regex skipPattern ("^(.+)\\d+_=_.+\\*\\1\\d+_=_.+$");
  std::cout << "\n\nTest of interaction patterns:\n";
  
  std::vector< std::string > interactionNames;

  std::string i1 ("Variable1=2**Variable3"); interactionNames.push_back(i1);
  std::string i2 ("Variable1*xxx");          interactionNames.push_back(i2);
  std::string i3 ("X?1_=_2*X?1_=_4");        interactionNames.push_back(i3);
  std::string i4 ("month2_=_1*month3_=_4");  interactionNames.push_back(i4);

  for(std::vector< std::string >::const_iterator it = interactionNames.begin(); it != interactionNames.end(); ++it) {
    std::cout << "Match results:  input string is   " << *it << "  ";
    boost::smatch name;
    if(boost::regex_match(*it,name, skipPattern))
      std::cout << " matches with variable name {"
		<< trim_string(name[1]) << "}\n";
    else
      std::cout << " does not match pattern.\n";
  }
}
