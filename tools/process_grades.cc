/*
  Process the SDF file that comes from scanning a multiple choice exam.

  Notes
        -  '<cntl x> ='   in emacs gives cursor position (zero based)
	- convert file to unix format with <cntl x> <return> f
	- easier to clean answer lines so line starts with name
	- kill any residual special character last line in answer file
	
  First line of the file with the answers is assumed to have the
  answer key. If the option multVersions is set to true, then the
  first question is assumed to indicate a cyclic permutation of the
  answers.  Otherwise all of them use the common key defined in the
  first line of the answers file.

  Use * to denote any answer is correct.

  Output file is tab delimited with name, penn id, score (# correct).

*/


// Enable special processing of messed up questions!
//
#define FUBAR_ROTATE
//
//

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
parse_arguments(int argc, char** argv,
		std::string &inputFile, std::string &outputFile,
		int &nameCol, int &idCol, int &ansCol, int &nQues, bool &multVersions, bool &buildIndicators);

int
process (std::istream& input, std::ostream& output,
	 int nameCol, int idColumn, int answerColumn, int nQuestions,   // zero based
	 bool multVersions, bool buildIndicators);


// convenience output
std::ostream&
operator<<(std::ostream& os, std::vector<int> v)
{
  for(unsigned int i=0; i<v.size(); ++i)
    os << " " << std::setw(3) << v[i];
  return os;
}



int 
main(int argc, char** argv)
{
  // default is io via stdin and stdout
  std::string       inputFile       ("");
  std::string       outputFile      ("");
  int               nameColumn      ( 0);
  int               idColumn        (29);     // all are zero based
  int               answerColumn    (38);
  int               nQuestions      (44);
  bool              multVersions    (false);
  bool              buildIndicators (false);

  parse_arguments(argc, argv, inputFile, outputFile, nameColumn, idColumn, answerColumn, nQuestions, multVersions, buildIndicators);
  { std::string vStr ("");
    if (multVersions) vStr = "-v";
    std::string bStr ("");
    if (multVersions) bStr = "-b";
    std::cout << "process_grades -f " << inputFile << " -o " << outputFile 
	      << " -n " << nameColumn <<" -i " << idColumn << " -a " << answerColumn
	      << " -q " << nQuestions << " " << vStr << " " << bStr << std::endl;
  }
  std::ifstream input (inputFile.c_str());
  if(!input)
  { std::cerr << "ERROR: Could not open input file named " << inputFile << std::endl;
    return -1;
  }
  std::ofstream output (outputFile.c_str());
  if(!output)
  { std::cout << "No output file supplied; results going to standard output." << std::endl;
    return process(input, std::cout, nameColumn, idColumn, answerColumn, nQuestions, multVersions, buildIndicators);
  }
  else
  { std::cout << "Results going to file " << outputFile << std::endl;
    return process(input,    output, nameColumn, idColumn, answerColumn, nQuestions, multVersions, buildIndicators);
  }
}

///////////////////////////  process  ////////////////////////////////////////////

// set buildIndicators = true to get the full regression data set

int
process (std::istream& input, std::ostream& output,
	 int nameColumn, int idColumn, int answerColumn, int nQuestions, bool multVersions, bool buildIndicators)
{
  if(!input)
  { std::cerr << "Could not open the indicated input stream.\n";
    return -1;
  }
  if (!output)
    std::cerr << "Count not open the indicated output stream; writing to stdout.\n";

  // string used for all io
  std::string line;
  // get the answer key, store zero-based so a=0, b=1,...
  std::vector<int> answerKey ((size_t)nQuestions);
  std::getline(input, line);
  std::cout << "Answer line: '" <<  line << "'" << std::endl;
  std::string keys (line.substr(answerColumn, (size_t)nQuestions));
  std::cout << "       Keys: '" << keys << "' with length " << keys.size() << std::endl;
  std::istringstream istrm(keys); 
  for (size_t i=0; i<(size_t)nQuestions; ++i)                               // nQuestions *includes* the first question identifying key
  { char c;                                                      // and the answer key includes the leading 0 for exam key
    istrm >> c;
    if(c == '*')
      answerKey[i] = -1;
    else
      answerKey[i] = read_utils::ctoi(c)-1;                        // shift all down to base 0 for easier modular arith
  }
  const int firstQuestion( (multVersions) ? 1:0 );               // question numbers are zero based
  // space for results
  std::vector< std::vector<int> > answerFrequencies;
  for(int i=0; i<nQuestions; ++i)
    answerFrequencies.push_back(std::vector<int>(5));
  std::vector< std::vector<int> > correctArray;                  // 0 if wrong, 1 if correct
  std::vector< std::vector<int> > studentAnswers;                // aligns to common answer key
  std::vector< int >              questionTotal ((size_t)nQuestions);
  std::vector< int >              studentTotal;
  std::vector< std::string >      names;
  std::vector< std::string >      ids;
  // process each student record, first 'rotating' answers, then counting number correct
  { int student (0);
    while(std::getline(input,line))
    {
      names.push_back(line.substr(nameColumn, 20));              // 20 char for name
      ids.push_back(  line.substr(  idColumn,  8));
      std::cout << "Processing grades for " << names[student] /* ": " << line.substr(answerColumn,nQuestions) */ <<  std::endl;
      studentTotal.push_back(0);
      correctArray.push_back(std::vector<int>((size_t)nQuestions,0));
      studentAnswers.push_back(std::vector<int>((size_t)nQuestions,0));
      std::istringstream is(line.substr(answerColumn,(size_t)nQuestions));
      char choice;
      int examKey (0);
      if(multVersions)
      { is >> choice;
	examKey = read_utils::ctoi(choice)-1;          // 0 means no shift
      }
      for(int q=firstQuestion; q<nQuestions; ++q)
      { is >> choice;
	if(('0' < choice) && choice < '6')         
	{ int ans = read_utils::ctoi(choice)-1;            // zero-based answers 0..4 
#ifndef FUBAR_ROTATE
	  ans = (5 + ans - examKey)%5;                     // standard rotate
#else
	  if(q != 20)                                      // standard rotate
	    ans = (5 + ans - examKey)%5;                   
	  else                                             // special rotate
	  { if (examKey == 1)
	      ans = (5 + ans - 0)% 5;                
	    else
	    { if (examKey == 2)
		ans = (5 + ans - 1)% 5;                 //  rotate 1 place
	    }
	  }
#endif
	  studentAnswers[student][q] = ans;
	  ++answerFrequencies[q][ans];
#ifdef FUBAR_TWO
	  if (21 == q)                                     // fubared questions
	  { if((ans == 1) || (ans==4))
	    { ++studentTotal[student];
	      ++questionTotal[q];
	      ++correctArray[student][q];
	    }
	  }
	  else if (33 == q)  
	  { if((ans == 2) || (ans==4))
	    { ++studentTotal[student];
	      ++questionTotal[q];
	      ++correctArray[student][q];
	    }
	  }
	  else
#endif
	    if ( (answerKey[q]<0) || (ans == answerKey[q]) )  // omit negative (skip question, give credit)
	      { ++studentTotal[student];
		++questionTotal[q];
		++correctArray[student][q];
	      }
	}
      }
      ++student;
    }
  }
  //  summary of results
  size_t nStudents (names.size());
  double mean (0);
  for(size_t i=0; i<nStudents; ++i)
    mean += studentTotal[i];
  mean /= (double)nStudents;
  double ss (0);
  for(unsigned int i=0; i<nStudents; ++i)
  { double dev = (studentTotal[i]-mean);
    ss += dev * dev;
  }
  double sd = sqrt(ss/(double)nStudents);
  double factor = 100.0/(nQuestions-firstQuestion);
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
  for(int q=firstQuestion; q<nQuestions; ++q)
  { std::cout << "Q" << std::setw(2) << q+1;
    std::cout << "      " << "*abcde"[1+answerKey[q]] << "  "
	      << "    " << std::setw(4) << questionTotal[q]
	      << "    " << answerFrequencies[q] << "     ";
    // Correlations with total number right
    double pctCorrect (((double)questionTotal[q])/(double)nStudents);
    std::cout << std::setw(2) << floor(100.0*pctCorrect) << "%     ";
    double cov (0);
    for(unsigned int i=0; i<nStudents; ++i)
      cov += (correctArray[i][q]-pctCorrect)*(studentTotal[i]-mean);
    std::cout << std::setprecision(2) << cov/(((double)nStudents-1)*sd*sqrt(pctCorrect*(1-pctCorrect))) << std::endl;
  }
  // write tab delimited line for each student with header line for column names
  std::ostream_iterator<int> out_it (output,"\t ");
  output << "Name \t ID \t Total \t";
  if(buildIndicators) 
    for(int q=firstQuestion; q<nQuestions; ++q)
      for (int j=0; j<5; ++j)
	output <<  "Q" << q+1 << "_" << "abcde"[j] << "\t ";
  output << std::endl;
  for (unsigned int i=0; i<names.size(); ++i)
  { output << names[i] << "\t" << ids[i] << "\t" << studentTotal[i] << "\t ";
    if (buildIndicators)
      for (int q=firstQuestion; q<nQuestions; ++q)
      { std::vector<int> b(5); //  = {0,0,0,0,0};
	b[studentAnswers[i][q]] = 1;
	std::copy (b.begin(), b.end(), out_it);
      }
    output << std::endl;
  }
  return 0;
}


  
///////////////////////////  parse_arguments  ////////////////////////////////////////////

void
parse_arguments(int argc, char** argv,
		std::string &inputFile, std::string &outputFile,
		int &nameCol, int &idCol, int &ansCol, int &nQues, bool &multVersions, bool &buildIndicators)
{
  int key;
  while (1)                              // read until empty key causes break
  { int option_index = 0;
    static struct option long_options[] = {
      {"input-file",        required_argument, 0, 'f'},  // has arg,
      {"output-file",       required_argument, 0, 'o'},  // has arg,
      {"questions",         required_argument, 0, 'q'},  // has arg,
      {"name-column",       required_argument, 0, 'n'},
      {"id-column",         required_argument, 0, 'i'},  // has arg,
      {"answer-column",     required_argument, 0, 'a'},  // has arg,
      {"multiple-versions", no_argument,       0, 'v'},  //  no arg
      {"build-indicators",  no_argument,       0, 'b'},  //  no arg
      {0, 0, 0, 0}                       // terminator 
    };
    key = getopt_long (argc, argv, "f:o:q:n:i:a:vb", long_options, &option_index);
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
    case 'q' :
      {
	nQues = read_utils::lexical_cast<int>(optarg);   break;
      }
    case 'n' :
      {
	nameCol = read_utils::lexical_cast<int>(optarg);   break;
      }
    case 'i' :
      {
	idCol = read_utils::lexical_cast<int>(optarg);   break;
      }
    case 'a' :
      {
	ansCol = read_utils::lexical_cast<int>(optarg);    break;
      }
    case 'v' :
      {
	multVersions = true;    break;
      }
    case 'b' :
      {
	buildIndicators = true;    break;
      }
    }
  }
}
