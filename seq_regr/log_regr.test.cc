//  $Id: log_regr.test.cc,v 1.28 2007/11/26 23:09:36 bob Exp $
  
/*
  Run as
  
          log_regr.test.exec -f filename -p pvalue

  with input from std::cin if no filename is given.  A test example is

          log_regr.test.exec -f test/lregr.dat -p .1

  with the fairly large p-value forces it to add some things.  The second X is
  a copy of the first, and so this also tests the check for linear redundant
  predictors (raise p-value to about .3 to force this one is as well)
*/

#include "log_regr.h"
#include "print_utils.h"
#include "function_utils.h"
#include "range.h"

#include <sstream>
#include <iostream>
#include <fstream>
#include <vector>
#include <list>


// IO type switch
// #define _use_scanf_io_


#ifdef _use_scanf_io_

#include <cstdio>
typedef FILE*         INPUT;
void run_logistic_regression(double pValue, INPUT input); 

#else

#include <fstream>
typedef std::istream  INPUT;
void    run_logistic_regression(double pValue, INPUT& input); 

#endif 
  

void
parse_arguments(int argc, char** argv, double& pValue, std::string& fileName);


  
int
main(int argc, char** argv)
{

  // default arguments
  std::string fileName ("/Users/bob/C/seq_regr/test/lregr.dat");
  double      pValue   (0.5);
  parse_arguments(argc, argv, pValue, fileName);
  if (false)  // force to use default from files for gdb
  //  if (argc <= 1 || fileName.size()==0)
  {
    #ifdef _use_scanf_io_
    run_logistic_regression(pValue, stdin);
    #else
    run_logistic_regression(pValue, std::cin);
    #endif
  }
  else
  {
    #ifdef _use_scanf_io_
    FILE* inputFile (fopen(fileName.c_str(),"r"));
    #else
    std::ifstream inputFile (fileName.c_str());
    #endif
    if (inputFile)
			run_logistic_regression(pValue, inputFile);
    else
      std::cout << "Could not open input file " << fileName << "." << std::endl;
  }
  return 0;
}

#ifdef _use_scanf_io_
const int maxNameLength = 100;
bool
read_string(char *s, int n, register FILE *iop)
{
  register int c;
  register char *cs;
  cs = s;
  while (--n > 0 && (c = getc(iop)) != EOF)
  {
    if (c != ' ')
      if ((*cs++ = c) == '\n')
	break;
  }
  --cs;
  *cs='\0';
  return (cs == s);
}
#define NAME_LEN(s)      strlen(s)
#define READ_INT(f,i)    fscanf(f,"%d",&i)
#define READ_REAL(f,x,i) fscanf(f,"%lf",&x[i])
#define READ_NAME(f)     read_string(name, maxNameLength-1, f);read_string(name, maxNameLength-1, f);
#else
#define NAME_LEN(s)      (s).size()
#define READ_INT(f,i)    f >> i
#define READ_REAL(f,x,i) f >> x[i]
#define READ_NAME(f)     getline(input, name);  getline(input, name) // flush eol
#endif


#ifdef _use_scanf_io_
void
run_logistic_regression(double pValue, INPUT input)
#else
void
run_logistic_regression(double pValue, INPUT& input)
#endif 
{
  // read, then check sample size
  int n (0);
  READ_INT(input,n);
  if (n > 0)
    std::cout << "TEST: Reading data stream with n = " << n << " cases." << std::endl;
  else
    std::cout << "TEST: Input stream says n = " << n << "; terminating." << std::endl;
  // space holding data that is used
  std::vector<double> y (n);
  std::vector<double> x (n);
  #ifdef _use_scanf_io_
  char      name[maxNameLength];
  #else
  std::string name;
  #endif
  // get y first; takes two reads to flush a leading end of line
  READ_NAME(input);
  for (int i=0; i<n; ++i)
    READ_REAL(input,y,i);
  std::cout << "TEST: "
	    << name << "[0] = " << y[0] << ";  " 
	    << name <<"[n-1] = " << y[n-1] << std::endl;   
  // build and print base regression model
  LogisticRegression regr(name, make_anonymous_range(make_range(y)), n);
  std::cout << " *************  Initialized Model  *************** \n"
	    << regr << std::endl;

  // read sequence of predictors from input file until no more
  int j (0);
  bool exit (false);
  while (input)
  { ++j;
    READ_NAME(input);
    if (NAME_LEN(name)==0)
    { std::cout << "TEST: EOF on input file; terminating." << std::endl;
      break;
    }
    std::cout << "TEST: Considering [" << name << "] .................................. " << std::endl;
    for (int i=0; i<n; ++i)
      if (input)
				READ_REAL(input,x,i);
      else
      { exit = true;
				std::clog << "TEST: RUN OUT OF DATA on input." << std::endl;
				break;
      }
    if (exit) break;
    std::pair<double,double> result;
    double center (range_stats::average(make_range(x),n));
    double sd     (range_stats::standard_deviation(make_range(x), center, n));
    result = regr.score_predictor(name, make_anonymous_range(x), center, sd);
    std::cout << "TEST: For predictor " << name << " score test result = " << result << std::endl;
		double df (4.0);
    result = regr.add_predictor(name, make_anonymous_range(x), center, sd, pValue, df);
    std::cout << "TEST: For predictor " << name << " result = " << result << std::endl;
    if (result.second < pValue) // it was added, so test the code for dropping variable
    { std::cout << "TEST: Added predictor " << name << ", model is now: " << regr << std::endl;
      regr.remove_last_predictor();
      regr.add_predictor(name, make_anonymous_range(x), center, sd, pValue, df);
      std::cout << "TEST: Model after Drop/Add: " << regr << std::endl;
    }
  }
  std::ofstream output ("test/log_regr.model");
  regr.write_to(output);
  output.close();
  std::ofstream outputData("test/log_regr.model.txt");
  regr.write_data_to(outputData);
  outputData.close();
  std::cout << "TEST: Total of " << j << " predictors considered." << std::endl;

  if (regr.number_of_predictors()>0)
	{ const int df(5);
		regr.add_calibrator(df, 0.05);
		std::cout << "TEST: Model after adding calibrator... " << regr << std::endl;
		// save x/y data to file
		std::ofstream calOutputData("test/cal_log_regr_data.txt");
		regr.write_data_to(calOutputData);
		calOutputData.close();
	}
  // done
  std::cout.flush();
}

//  Parsing command line arguments

void
parse_arguments(int argc, char** argv,
		double& pValue,
		std::string& fileName)
{
  for (int i=1; i<argc; i=i+2)
  {
    char key(argv[i][0]);
    if ('-' == key)
    { 
      switch (argv[i][1])
      {
      case 'p' :                                    
	{ std::stringstream input(argv[i+1]);
	  input >> pValue;
	  break;
	}
      case 'f' :                                    
	{ std::string name(argv[i+1]);
	  fileName = name;
	  break;
	}
      default  :
	{
	  std::clog << "Option " << argv[i][0] << " not recognized." << std::endl;
	  break;
	}
      }
    }
  }
}  
  
