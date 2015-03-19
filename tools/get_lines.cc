/*
 *  get_lines.cc
 *  utils

  Reads stdin and pulls out the lines defined here or in an external file.

  Writes selected lines to stdout
 
 */

#include <getopt.h>

#include <iostream>
#include <fstream>
#include <string>
#include <vector>


void  parse_arguments(int argc, char** argv,
		      std::string &lineFile,      // file with line numbers
		      bool        &addNumber      // prepend result with line number
		      );

int 
main(int argc, char ** argv)
{
  using std::string;

  string linesFileName = "";
  bool   appendLineNumber = false;
  parse_arguments(argc, argv, linesFileName, appendLineNumber);
  if(linesFileName.empty())
  { std::cerr << "ERROR: need to supply file with line numbers to extract.\n";
    return -1;
  }
  std::vector<int> linesToReturn;
  {
    std::ifstream file (linesFileName);
    int line0=-1, line=0;
    while (file >> line)
    { if (line0 < line)
	linesToReturn.push_back(line);
      else
	std::cerr << "ERROR: Lines numbers must be sorted into increasing order\n";
      line0 = line;
    }
    std::clog << "GetLines: Read " << linesToReturn.size() << " line numbers.\n";
  }
  int k = 0;
  string theLine;
  int currentLineIndex = 0;
  while (k < (int) linesToReturn.size())
  { ++currentLineIndex;
    getline(std::cin, theLine);
    if (currentLineIndex == linesToReturn[k])
    { if (appendLineNumber)
	std::cout << "[" << currentLineIndex << "]";
      std::cout << theLine << std::endl;
      ++k;
    }
  }
  return 0;
} 

void
parse_arguments(int argc, char** argv, std::string &lineFile, bool &appendNumber)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	{"lines",        1, 0, 'l'},  // has arg,
	{"number",       0, 0, 'n'},  // no arg
	{0, 0, 0, 0}                       // terminator 
      };
      key = getopt_long (argc, argv, "l:n", long_options, &option_index);
      if (key == -1)
	break;
	switch (key)
	  {
	  case 'l' : { lineFile = optarg;                                                break; }
	  case 'n' : { appendNumber = true;                                              break; }
	  default:   { std::cerr << "ERROR: did not understand key " << key<< std::endl; return; }
	  }
    }
}
