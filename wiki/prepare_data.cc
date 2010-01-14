#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <set>
#include <map>
#include <assert.h>
 
//////////////////////////////////////////////////////////
//  From the file:
//
//        Word    PoS     Freq
//        the     Det     61847
//        of      Prep    29391
//        and     Conj    26817
//
// A Description is the pair with last two items
//////////////////////////////////////////////////////////

class Description
{
public:
  std::string m_class;
  int m_count;
};

std::istream&
operator>>(std::istream& in, Description& d)
{
  in >> d.m_class >> d.m_count;
  return in;
}

///////////////////////////////////////////////////////////////////////////////////
// The current format of the sentence file is a bit weird.  The last few tokens look like:
//     ...  her the 0
//     ...  her the 1
//     ...  the her 0
//     ...  the her 1
// where the middle two mean that "the" is the correct answer.  These last tokens
// are what the following function expects to analysis and returns a zero or a one.
///////////////////////////////////////////////////////////////////////////////////

int
parse_answer(std::istream& in)
{
  std::string challenge1;
  std::string challenge2;
  int answer;
  in >> challenge1 >> challenge2 >> answer;
  assert((challenge1 == "her") || (challenge1 == "the"));
  assert((challenge2 == "her") || (challenge2 == "the"));
  assert(challenge1 != challenge2);
  assert( (answer == 0) || (answer == 1) );
  int binaryAnswer = ((challenge1 == "her") == (answer == 0));
  return binaryAnswer;
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
cstring_to_int (char *s)
{
  std::istringstream ss (s);
  int i;
  ss >> i;
  return i;
}


int
main(int argc, char** argv)
{

  //  Parse command line arguments
  if(argc != 3)
    std:: cout << "Oops: argc = " << argc
	       << "\nProper usage: "  << argv[0]
	       << " word_frequency_table frequency_limit < tokenized_lines > stream_format" << std::endl;
  assert(argc == 3);
  std::ifstream wordFrequencyFile (argv[1]);
  int limit;
  limit = cstring_to_int(argv[2]);
  
  // Read word frequencies: number of words determines the number of features
  typedef std::multimap<std::string, Description> FrequencyTable;
  FrequencyTable frequency;
  std::set<std::string> words;
  std::string header;
  getline(wordFrequencyFile, header);
  int numberOfWordsUsed (0);
  while(!wordFrequencyFile.eof())
  { Description value;
    std::string key;
    std::string one_line;
    getline(wordFrequencyFile, one_line);
    std::stringstream line(one_line);
    getline(line, key, char(9));  // the leading \t
    getline(line, key, char(9));  // everything up to the next \t
    line >> value;
    words.insert(key);
    if(value.m_count > limit)
    { frequency.insert(std::make_pair(key,value));
      ++numberOfWordsUsed;
    }
  }
  

  //                          DEBUGGING
  // Let's see what the table looks like:
  //  for(FrequencyTable::const_iterator i = frequency.begin(); i != frequency.end(); ++i)
  //    if(i->second.m_count > 10000)
  //      std::cout << "most frequent: " << i->first << " POS=" << i->second.m_class
  //                << " @ " << i->second.m_count << std::endl;


  
  // Initialize empty set of data: bag size = number of features
  int numberOfSentences = 44;
  std::map<std::string, std::vector<double> > table;
  for(std::set<std::string>::const_iterator i = words.begin(); i != words.end(); ++i)
    table.insert(std::make_pair(*i,std::vector<double>(numberOfSentences,0)));


  // Read sentences: each sentence is an observation, response is 0/1 binary
  std::vector<int> y(numberOfSentences);
  const int numberOfTokens (49);                    // tokens in prior sentence context
  int sentenceNumber = 0;
  while(!std::cin.eof())
  {
    for(int i = 0; i < numberOfTokens; ++i)
    { std::string token;
      std::cin >> token;
      if(words.find(token) != words.end())
	++table[token][sentenceNumber];
    };
    y.at(sentenceNumber) = parse_answer(std::cin);
    std::cin >> std::ws;
    ++sentenceNumber;
  };

  
  // Write out in streaming format
  // First sort bag counts by frequency
  typedef std::multimap<int, std::pair<std::string, std::string> > SortMap;
  SortMap sorted;
  for(FrequencyTable::const_iterator i = frequency.begin(); i != frequency.end(); ++i)
    sorted.insert(std::make_pair(i->second.m_count, std::make_pair(i->first, i->second.m_class)));

  // Write n,p+1
  std::cout << numberOfSentences << " " << 1+numberOfWordsUsed << std::endl;
  
  // Print out Y
  std::cout << "is_her" << std::endl;
  std::cout << std::endl;  // no attributes
  for(int j = 0; j < numberOfSentences; ++j)
    std::cout << y[j] << " ";
  std::cout << std::endl;

  // Print out the X's
  for(SortMap::const_reverse_iterator i = sorted.rbegin();i != sorted.rend(); ++i)
  {
    std::string token = i->second.first;
    if(!is_all_zeros(table[token]))
    {
      std::string pos = i->second.second;
      std::cout << token << std::endl;  // name of the variable
      std::cout << "stream " << pos << " frequency " << i->first << std::endl;  // properties
      for(int j = 0; j < numberOfSentences; ++j)
	std::cout << table[token][j] << " ";
      std::cout << std::endl;
    };
  }
}
