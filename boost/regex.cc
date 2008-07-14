#include <boost/regex.hpp>
#include <iostream>
#include <string>
#include <vector>

int main()
{
  std::string line;
  boost::regex pat( "^Subject: (Re: |Aw: )*(.*)" );

  if(false)
    while (std::cin)
    {
      std::getline(std::cin, line);
      boost::smatch matches;
      if (boost::regex_match(line, matches, pat))
	std::cout << matches[2] << std::endl;
    }

  std::cout << "\n\nTEST:\n";
  std::vector< std::string > variableNames;
  
  std::string v1 ("Variable1=2"); variableNames.push_back(v1);
  std::string v2 ("Variable2");   variableNames.push_back(v2);
  std::string v3 ("VAR == 3");    variableNames.push_back(v3);
  std::string v4 ("XXX = 3");     variableNames.push_back(v4);

  boost::regex vPattern ("^(.*)=\\d");
  for(std::vector< std::string >::const_iterator it = variableNames.begin(); it != variableNames.end(); ++it) {
    boost::smatch name;
    if(boost::regex_match(*it,name, vPattern))
      std::cout << "Match: variable name { " << name[1] << " }\n";
    else
      std::cout << "No match for " << *it << std::endl;
  }
  
}
