/*
  Process the text file that comes from scanning a multiple choice exam.

  Notes
        -  '<cntl x> ='   in emacs gives cursor position (zero based)
	- convert file to unix format with <cntl x> <return> f
	- easier to clean answer lines so line starts with name
	- kill any residual special character last line in answer file

	
  First TWO lines of the input file with the answers is assumed to
  have two answer keys.  The second key is used to allow more than one
  answer to a question.  Leave this line blank if there are no alternate
  answers.  The first line is assumed to be the intended correct answers,
  with the second line to allow some others.

  Use * to denote any answer is correct.  If there's a permutation error,
  then you need to kludge the precessing.  See the code for "FUBAR_ROTATE"
  to see one way to handle this mistake.

  If the option multVersions is set to true, then the first question
  indicates a cyclic permutation of the answer key.  Otherwise all of
  them use the common key defined in the first line of the answers
  file.  

  Output directed file is tab delimited with name, penn id, score (#
  correct).  The tool also writes a tab-delimited file
  'std_answers.txt' to the calling directory; this file has the
  student answers as if all used key 'A' and writes the choices as A,
  B, C, D, and E.
*/


// Enable special processing if forgot to rotate answers for a question
// #define FUBAR_ROTATE
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
  // read in the two lines with answer keys
  std::getline(input, line);
  std::cout << "Answer key #1: '" <<  line << "'" << std::endl;
  std::string key0 (line.substr(answerColumn, (size_t)nQuestions));
  std::cout << "       keys  : '" << key0 << "' with length " << key0.size() << std::endl;
  // check for and use second keys if present
  std::string key1 (key0);
  std::getline(input, line);
  if(line.size()>5)
  { std::cout << "Answer key #2: '" <<  line << "'" << std::endl;
    key1 = line.substr(answerColumn, (size_t)nQuestions);
    std::cout << "       keys  : '" << key1 << "' with length " << key1.size() << std::endl;
  }
  else std::cout << "Answer key #2 is not used.\n";

  // build the answer key, store zero-based so a=0, b=1,...
  //   Each key is a vector of length 2 ( second element is -2 which matches none by default )
  std::vector<std::vector<int>> answerKeys(nQuestions);          // nQuestions x 2 
  for (size_t q=0; q<(size_t)nQuestions; ++q)                    // nQuestions *includes* the first question identifying key
  { char c = key0[q];                                            // and the answer key includes the leading 0 for exam key
    std::vector<int> v = std::vector<int>(2);
    if(c == '*')
      v[0] = -1;
    else
      v[0] = read_utils::ctoi(c)-1;                              // shift all down to base 0 for easier modular arith
    if (key1[q]==key0[q])
      v[1] = -2;
    else
    { std::cout << "NOTE: Multiple answers allowed for Question #" << q+1 << std::endl;
      v[1] = read_utils::ctoi(key1[q])-1;
    }
    answerKeys[q] = v;
  }
  const int firstQuestion( (multVersions) ? 1:0 );               // question numbers are zero based
  // space for results
  std::vector< std::vector<int> > answerFrequencies;
  for(int i=0; i<nQuestions; ++i)
    answerFrequencies.push_back(std::vector<int>(5));
  std::vector< std::vector<int> > correctArray;                  // 0 if wrong, 1 if correct
  std::vector< std::vector<int> > studentAnswers;                // aligns to common answer key
  std::vector< std::string>       studentCorrectAnswers;         // star wrong answers for output
  std::vector< int >              questionTotal ((size_t)nQuestions);
  std::vector< int >              studentTotal;
  std::vector< int >              examKeys;
  std::vector< std::string >      names;
  std::vector< std::string >      ids;
  // process each student record, first 'rotating' answers, then counting number correct
  { int student (0);
    const int nameLength=18;
    while(std::getline(input,line))
    {
      if (line.size() < 2) break;
      names.push_back(line.substr(nameColumn, nameLength));      // 18 char for name
      ids.push_back(  line.substr(  idColumn,  8));              //  8 char for Penn id
      std::cout << "Processing grades for " << names[student] <<  std::endl;
      studentTotal.push_back(0);
      correctArray.push_back(std::vector<int>((size_t)nQuestions,0));
      studentAnswers.push_back(std::vector<int>((size_t)nQuestions,0));
      studentCorrectAnswers.push_back(std::string((size_t)nQuestions,'-'));
      std::string is = line.substr(answerColumn,(size_t)nQuestions);
      if(is.size() < (size_t)nQuestions)
      { std::cout << "Note: padding answers for student " << names[student] << std::endl;
	is.insert(is.end(),nQuestions-is.size(),' ');
      }
      char choice;
      int examKey (0);
      size_t q = 0;                                        // position in answer key
      if(multVersions)
      { choice = is[q++];
	examKey = read_utils::ctoi(choice)-1;              // 0 means no shift
      }
      examKeys.push_back(examKey);
      while (q < (size_t)nQuestions)
      { choice = is[q];
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
		ans = (5 + ans - 1)% 5;                    //  rotate 1 place
	    }
	  }
#endif
	  studentAnswers[student][q] = ans;
	  ++answerFrequencies[q][ans];
	  if ( (answerKeys[q][0]<0) || (ans == answerKeys[q][0])  || (ans == answerKeys[q][1]) )
	  { ++studentTotal[student];
	    ++questionTotal[q];
	    ++correctArray[student][q];
	  } else
	    studentCorrectAnswers[student][q] = '*';
	}
	++q;
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
    std::cout << "      " << "*abcde"[1+answerKeys[q][0]] << "  "
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
  // write student choices in standardized choice order (as if all had first key)
  std::ofstream stdOutput ("std_answers.txt");
  std::string choices = "ABCDE";
  for (unsigned int i=0; i<names.size(); ++i)
  {  stdOutput << names[i] << '\t' << ids[i] << '\t';
     for (auto j : studentAnswers[i])
       stdOutput << choices.at( (unsigned int) j);
     stdOutput << "  " << studentTotal[i] << std::endl;
     stdOutput << "                  " << '\t' << "        " << '\t'
	       <<  studentCorrectAnswers[i] << std::endl;
  }
  // write tab delimited line for each student with header line for column names
  std::ostream_iterator<int> out_it (output,"\t ");
  output << "Name \t ID \t ExamKey \t Total \t";
  if(buildIndicators) 
    for(int q=firstQuestion; q<nQuestions; ++q)
      for (int j=0; j<5; ++j)
	output <<  "Q" << q+1 << "_" << "abcde"[j] << "\t ";
  output << std::endl;
  for (unsigned int i=0; i<names.size(); ++i)
  { output << names[i] << "\t" << ids[i] << "\t" << examKeys[i] << "\t" << studentTotal[i] << "\t ";
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
