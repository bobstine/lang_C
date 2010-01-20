#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <set>
#include <map>
#include <assert.h>
#include <getopt.h>

#include "read_utils.h"


/*

  19 Jan 10 ... Revise for new input format that has correct word at end.
  
*/

//////////////////////////////////////////////////////////
//  Format of the frequency file is (it includes the header line)
//
//        Word    PoS     Freq
//        the     Det     61847
//        of      Prep    29391
//        and     Conj    26817
//        ...
//
// The current format of the sentence file puts the sought word last after a /t
// 
///////////////////////////////////////////////////////////////////////////////////

void
parse_arguments(int argc, char** argv,
		std::string &keywordFile,
		std::string &targetWord,         // this last word coded 1, others coded 0
		int         &minFreq,
		int         &numberOfSentences,
		int         &numberOfTokens);    // context for response

int
parse_answer(std::istream& in, std::string target, std::set<std::string>& lastWords)
{
  std::string lastWord;
  in >> lastWord;
  lastWords.insert(lastWord);
  if (lastWord == target)
    return 1;
  else
    return 0;
}

template<class T>
bool
is_all_zeros(const T& v)
{
  bool result = true;
  for(typename T::const_iterator i = v.begin(); i != v.end(); ++i)
    if(*i != 0)
      result = false;
  return result;
}


int
main(int argc, char** argv)
{

  std::string   keywordFileName;
  std::string   targetWord;
  int           freqLimit        (1000);
  int           numberOfSentences  (40);
  int           numberOfTokens     (49);
  parse_arguments(argc, argv, keywordFileName, targetWord, freqLimit, numberOfSentences, numberOfTokens);

  // Read word frequencies: number of words determines the number of features (pair is word, pos)
  typedef std::multimap<int, std::pair<std::string, std::string> > FrequencyMap;
  FrequencyMap freqMap;
  std::set<std::string> keywords;
  { 
    std::ifstream keywordFile (keywordFileName.c_str());
    std::string header;
    getline(keywordFile, header);          // dump header line
    while(!keywordFile.eof())
    { 
      std::string keyword;
      std::string pos;
      int         freq;
      std::string one_line;
      getline(keywordFile, one_line);
      std::stringstream line(one_line);
      getline(line, keyword, char(9));  // the leading \t
      line >> keyword >> pos >> freq;
      if(freq > freqLimit)
      { keywords.insert(keyword);
	freqMap.insert(std::make_pair(freq, std::make_pair(keyword,pos)));
      }
    }
  }
  if (freqMap.empty())
  { std::cerr << "ERRR: No keyword has frequency larger than minimum (" << freqLimit << ")\n";
    return -1;
  }
    
  // Initialize empty set of data: bag size = number of features
  //     count those keywords that appear in each sentence

  std::map<std::string, std::vector<double> > counts;
  for(std::set<std::string>::const_iterator i = keywords.begin(); i != keywords.end(); ++i)
    counts.insert(std::make_pair(*i,std::vector<double>(numberOfSentences,0)));


  // Read sentences from stdin: each sentence is an observation, response is 0/1 binary
  std::vector<int> y(numberOfSentences);
  {
    std::set<std::string> lastWords;
    int sentenceNumber = 0;
    while(sentenceNumber < numberOfSentences && !std::cin.eof())
    {
      for(int i = 0; i < numberOfTokens; ++i)
      { std::string token;
	std::cin >> token;
	if(keywords.find(token) != keywords.end())
	  ++counts[token][sentenceNumber];
      };
      y.at(sentenceNumber) = parse_answer(std::cin, targetWord, lastWords);
      std::cin >> std::ws;
      ++sentenceNumber;
    };
    if (numberOfSentences != sentenceNumber)
    { std::cerr << "ERROR: Not enough sentences(" << sentenceNumber << "<" << numberOfSentences <<") found on input.\n";
      return -2;
    }

    std::clog << "Found " << lastWords.size() << " last words { ";
    for(std::set<std::string>::const_iterator it = lastWords.begin(); it !=lastWords.end(); ++it)
      std::clog << *it << " ";
    
    int count(0);
    for (int i = 0; i<numberOfSentences; ++i)
      count += y[i];
    std::clog << "}  with " << count << " equal to target word (" << targetWord << ").\n";
  }
  
  // Write out in streaming format

  // Write n,p+1
  std::cout << numberOfSentences << " " << freqMap.size() << std::endl;
  
  // Print out Y
  std::cout << "is_" << targetWord << std::endl;
  std::cout << std::endl;  // no attributes
  for(int j = 0; j < numberOfSentences; ++j)
    std::cout << y[j] << " ";
  std::cout << std::endl;

  // Print out the X's
  for(FrequencyMap::const_reverse_iterator i = freqMap.rbegin();i != freqMap.rend(); ++i)
  {
    std::string keyword = i->second.first;
    if(!is_all_zeros(counts[keyword]))
    {
      std::string pos = i->second.second;
      std::cout << keyword << std::endl;  // name of the variable
      std::cout << "stream " << pos << " frequency " << i->first << std::endl;  // properties
      for(int j = 0; j < numberOfSentences; ++j)
	std::cout << counts[keyword][j] << " ";
      std::cout << std::endl;
    };
  }
}



void
parse_arguments(int argc, char** argv,
		std::string &keywordFile,
		std::string &targetWord,
		int         &minFreq,
		int         &numSentences,
		int         &numTokens)
{
  int key;
  while (1)                                  // read until empty key causes break
    {
      int option_index = 0;
      static struct option long_options[] = {
	  {"keyword-file",      1, 0, 'f'},  // has arg,
	  {"target-word",       1, 0, 'w'},  // has arg,
	  {"min-frequency",     1, 0, 'm'},  // has arg,
	  {"num-sentences",     1, 0, 'n'},  // has arg,
	  {"num-tokens",        1, 0, 't'},  // has arg,
	  {"help",              0, 0, 'h'},  // no  arg, 
	  {0, 0, 0, 0}                       // terminator 
	};
	key = getopt_long (argc, argv, "f:w:m:n:t:h", long_options, &option_index);
	if (key == -1)
	  break;
	// std::cout << "MAIN: Parsing key " << char(key) << " with option_index " << option_index << std::endl;
	switch (key)
	  {
	  case 'f' :                                    
	    {
	      std::string name(optarg);
	      // std::cout << "Read name from optional args..." << name << std::endl;
	      keywordFile = name;
	      break;
	    }
	  case 'w' :    
	    {
	      std::string word(optarg);
	      targetWord = word;
	      break;
	    }
	  case 'm' :
	    {
	      minFreq = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'n' :
	    {
	      numSentences = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 't' :
	    {
	      numTokens = read_utils::lexical_cast<int>(optarg);
	      break;
	    }
	  case 'h' :
	    {
	      std::cout << "switches: (sentences read from stdin, data to stdout)" << std::endl << std::endl;
	      std::cout << "      --keyword-file=foo       file defining keywords" << std::endl;
	      std::cout << "      -ffoo" << std::endl << std::endl;
	      std::cout << "      --target-word=red        last word coded as 1" << std::endl;
	      std::cout << "      -wred" << std::endl << std::endl;
	      std::cout << "      --min-frequency=#        minimum observed frequency to be used as keyword" << std::endl;
	      std::cout << "      -m1000" << std::endl << std::endl;
	      std::cout << "      --num-sentences=#        number of sentences to read from std input" << std::endl;
	      std::cout << "      -n1000" << std::endl << std::endl;
	      std::cout << "      --num-tokens=#           number of tokens before y word" << std::endl;
	      std::cout << "      -t49" << std::endl << std::endl;
	      std::cout << "      --help                   generates this message" << std::endl;
	      std::cout << "      -h" << std::endl << std::endl;
	      exit(0);
	      break;
	    }
	  }
    }
  std::clog << "PARSE: keyword-file=" << keywordFile << ", target-word=" << targetWord
	    << ", min-frequency=" << minFreq << ", num-sentences=" << numSentences
	    << ", num-tokens=" << numTokens << std::endl;
}
