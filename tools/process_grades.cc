/*
  Process the SDF file that comes from scanning a multiple choice exam.

  First line is assumed to have the answer key, and the first question
  is assumed to indicate a cyclic permutation of the answers.

*/


#include "read_utils.h"

#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

#include <getopt.h>
#include <math.h>

void
parse_arguments(int argc, char** argv, std::string &inputFile, std::string &outputFile, int &idCol, int &ansCol, int &nQues)
{
  int key;
  while (1)                              // read until empty key causes break
  { int option_index = 0;
    static struct option long_options[] = {
      {"input-file",        1, 0, 'f'},  // has arg,
      {"output-file",       1, 0, 'o'},  // has arg,
      {"questions",         1, 0, 'q'},  // has arg,
      {"id-column",         1, 0, 'i'},  // has arg,
      {"answer-column",     1, 0, 'a'},  // has arg,
      {0, 0, 0, 0}                       // terminator 
    };
    key = getopt_long (argc, argv, "f:o:i:a:q", long_options, &option_index);
    // std::cout << "Key  " << key << "  optarg " << optarg << std::endl;
    if (key == -1)
      break;
    switch (key)
    {
    case 'f' :                                    
      {
	std::string name(optarg);   inputFile = name;   break;
      }
    case 'o' :  
      {
	std::string name(optarg);   outputFile = optarg; break;
      }
    case 'q' :   // what the fuck is wrong with this???
      {
	std::istringstream is(optarg); is >> nQues;    break;
      }
    case 'i' :
      {
	idCol = read_utils::lexical_cast<int>(optarg);   break;
      }
    case 'a' :
      {
	ansCol = read_utils::lexical_cast<int>(optarg);    break;
      }
    }
  }
}

std::ostream&
operator<<(std::ostream& os, std::vector<int> v)
{
  for(unsigned int i=0; i<v.size(); ++i)
    os << " " << std::setw(3) << v[i];
  return os;
}

int
process (std::istream& input, std::ostream& output, int idColumn, int answerColumn, int nQuestions)
{
  if(!input)
  { std::cerr << "Could not open the indicated input stream.\n";
    return -1;
  }
  if (!output)
    std::cerr << "Count not open the indicated output stream; writing to stdout.\n";
  
  std::string line;
  // get the answer key, store zero-based so a=0, b=1,...
  std::vector<int> answerKey (nQuestions);
  std::getline(input, line);
  std::cout << "Key line: '" <<  line << "'" << std::endl;
  std::string keys (line.substr(answerColumn, nQuestions));
  std::istringstream istrm(keys);
  for (int i=0; i<nQuestions; ++i)
  { char c;
    istrm >> c;
    answerKey[i] = read_utils::ctoi(c)-1;                        // convert to zero base
  }

  // space for results
  std::vector< std::vector<int> > answerFrequencies;
  for(int i=0; i<nQuestions; ++i)
    answerFrequencies.push_back(std::vector<int>(5));
  std::vector< std::vector<int> > correctArray;
  std::vector< int > questionTotal (nQuestions);
  std::vector< int > studentTotal;
  std::vector< std::string > names;
  std::vector< std::string > ids;
  
  
  // process each student record, first 'rotating' answers, then counting number correct
  int student (0);
  while(std::getline(input,line))
  {
    names.push_back(line.substr(0,idColumn));
    ids.push_back(  line.substr(idColumn,8));
    //    std::cout << "Processing grades for " << names[student] << std::endl;
    studentTotal.push_back(0);
    correctArray.push_back(std::vector<int>(nQuestions,0));
    std::istringstream is(line.substr(answerColumn,nQuestions));
    char choice;
    is >> choice;
    int examKey (0);
    examKey = read_utils::ctoi(choice)-1;                        // 0 means no shift
    for(int q=1; q<nQuestions; ++q)
    { is >> choice;
      if(('0' < choice) && choice < '6')         
      { int ans = read_utils::ctoi(choice)-1;
	ans = (5 + ans - examKey)%5;               // zero based simplifies this
	++answerFrequencies[q][ans];
	if(ans == answerKey[q])
	{ ++studentTotal[student];
	  ++questionTotal[q];
	  ++correctArray[student][q];
	}
      }
    }
    ++student;
  }
  //  summary of results
  unsigned int nStudents (names.size());
  double mean (0);
  for(unsigned int i=0; i<nStudents; ++i)
    mean += studentTotal[i];
  mean /= nStudents;
  double ss (0);
  for(unsigned int i=0; i<nStudents; ++i)
  { double dev = (studentTotal[i]-mean);
    ss += dev * dev;
  }
  double sd = sqrt(ss/nStudents);
  double factor = 100.0/nQuestions;
  std::cout << "Average score " << std::setprecision(3) << mean*factor << " with standard deviation " << std::setprecision(2) << sd*factor << std::endl;
  // check
  /*  std::cout << "Total right for Question 25 = " << questionTotal[25] << std::endl;
  int count (0);
  for (unsigned int i=0; i<nStudents; ++i)
    count += correctArray[i][25];
  std::cout << "Total right for Question 25 = " << count  << std::endl;
  */
  // detailed output  for each question
  std::cout << "Ques  Answer  #Correct           Choices                 Corr\n";
  for(int q=1; q<nQuestions; ++q)
  { std::cout << "Q" << std::setw(2) << q+1;
    std::cout << "      " << "abcde"[answerKey[q]] << "  "
	      << "    " << std::setw(4) << questionTotal[q]
	      << "    " << answerFrequencies[q] << "     ";
    // Correlations with total number right
    double pctCorrect (((double)questionTotal[q])/nStudents);
    std::cout << std::setw(2) << floor(100.0*pctCorrect) << "%     ";
    double cov (0);
    for(unsigned int i=0; i<nStudents; ++i)
      cov += (correctArray[i][q]-pctCorrect)*(studentTotal[i]-mean);
    std::cout << std::setprecision(2) << cov/((nStudents-1)*sd*sqrt(pctCorrect*(1-pctCorrect))) << std::endl;
  }
  // write tab delimited line for each student
  for (unsigned int i=0; i<names.size(); ++i)
    output << names[i] << "\t" << ids[i] << "\t" << studentTotal[i] << std::endl;
  return 0;
}



int 
main(int argc, char** argv)
{
  // default is io via stdin and stdout
  std::string       inputFile ("");
  std::string       outputFile("");
  int               idColumn(15);
  int               answerColumn(30);
  int               nQuestions(44);

  parse_arguments(argc, argv, inputFile, outputFile, idColumn, answerColumn, nQuestions);
  std::cout << "process_grades -f " << inputFile << " -o " << outputFile
	    <<" -i " << idColumn << " -a " << answerColumn << " -q " << nQuestions << std::endl;
  std::ifstream input (inputFile.c_str());
  if(!input) return -1;
  std::ofstream output (outputFile.c_str());
  if(!output)
    return process(input, std::cout, idColumn, answerColumn, nQuestions);
  else
    return process(input, output, idColumn, answerColumn, nQuestions);
    
}
  
