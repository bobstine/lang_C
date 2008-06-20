// $Id: c45_syntax.test.cc,v 1.4 2004/08/30 01:54:37 foster Exp $-*- c++ -*-

#include "c45_syntax.h"
#include <iostream>
#include <assert.h>
#include <sstream>
#include <getopt.h>

void internal_testing(const std::string&);


void read_from_file(int argc, char** argv);

int
main(int argc,char** argv)
{
  if(argc <= 1)
    internal_testing(argv[0]);
  else
    read_from_file(argc,argv);
  return 0;
};



//////////////////////////////////////////////////////////////////////////////////////////////////////
void read_from_file(int argc, char** argv)
{
  int c;
  bool print_raw_data   = false;  // true if "bankruptcy" model for output
  bool print_validation = false;
  bool print_names      = false;
  bool print_sanitize   = false;


  /////////////////////////////////////////////////////////////////////////////////////////////////////
  //
  // Reading in the command line parameters (this was modified from "man 3 getopt".
  // So it looks like C instead of C++.  Oh well, it runs.)
  //
  std::string file_name;
  while (1)
    {
      int option_index = 0;
      static struct option long_options[] =
      {
	{"input",        2, 0, 'i'},     // optional arg,    don't return strings, return 'i'

	{"help",         0, 0, 'h'},     // no arg,          don't return strings, return 'h'
	{"raw-data",     0, 0, 'r'}, 
	{"validation",   0, 0, 'v'}, 
	{"names",        0, 0, 'n'}, 
	{"sanitize",     0, 0, 's'}, 

	{0, 0, 0, 0}                 // terminator I think
      };

      c = getopt_long (argc, argv, "rihrvns", long_options, &option_index);
      if (c == -1)
	break;
			 
      switch (c)
	{
	case 'i':
	  if(optarg)
	    {
	      file_name = optarg;
	    }
	  break;
	  
	case 'r':
	  print_raw_data = true;

	  break;

	case 'v':
	  print_validation = true;

	  break;

	case 'n':
	  print_names = true;

	case 's':
	  print_sanitize = true;

	  break;

	case 'h':
	  std::cout << "primary switches:" << std::endl << std::endl;
	  std::cout << "      --input=sample_c45      read from sample_c45.names and sample_c45.data" << std::endl;
	  std::cout << "      --raw-data or -r        print out raw data sutiable for bob's code to read in (row=variable)" << std::endl;
	  std::cout << "      --validation or -v      print out in validation format (row=observation)" << std::endl;
	  std::cout << "      --names or -n           print out names for validation" << std::endl;
	  std::cout << "      --sanitize or -s        print out sanitary version of data" << std::endl;

	  exit(0);
	  break;

	default:
	  std::cout << "error: " << c << " with opt of " << optarg;
	  break;
	}
    }
  C45_syntax g(file_name);  // testing construction
  if(print_raw_data)
    g.raw_data_print_on(std::cout);
  else if(print_validation)
    g.validation(std::cout);
  else if(print_names)
    g.validation_names(std::cout);
  else if(print_sanitize)
    {
      g.print_sanitary_names(std::cout);
      g.print_sanitary_data(std::cout);
    }
  else
    g.print_on(std::cout);
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void
internal_testing(const std::string& program_name)
{
  {
    std::stringstream s_in;
    s_in << "1,5\n 2,10\n 3 , 0 \n 4, 8" << std::endl;  // three rows of data
    s_in << " ?,10\n 50,50\n 3 , 0 \n 3, " << std::endl;  // three more rows of data (2 full)
    s_in << "10,  \n 50,\n 3 , ? \n 3, " << std::endl;  // three more rows of data (none full)
    s_in << ",10\n 2,2\n 3 , 0 \n 40, " << std::endl;    // three more rows of data (2 full)
    std::stringstream t_in;

    t_in << "\
| Comments start with | in this language. \n\
Y.\n\
| \n\
X: continuous.\n\
Y: continuous.\n\
" << std::endl;

    std::stringstream s_out(s_in.str());
    std::stringstream t_out(t_in.str());

    std::cout << "\nIf you were to put in foo.names the following:\n\n" << std::endl;
    std::cout << " = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = " << std::endl;
    std::cout << t_in.str() << std::endl;
    std::cout << " = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = " << std::endl;
    std::cout << "\n\nAnd you were to put in foo.data the following:\n\n" << std::endl;
    std::cout << " = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = " << std::endl;
    std::cout << s_in.str() << std::endl;
    std::cout << " = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = " << std::endl;
    std::cout << "\n\nThen running with a variety of command lines generates:" << std::endl;
    std::cout << " = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = " << std::endl;
    std::cout << "      > " << program_name << " --input=foo" << std::endl;
    C45_syntax g(s_out,t_out); 
    g.print_on(std::cout);
    std::cout << " = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = " << std::endl;
    std::cout << "      > " << program_name << " -r --input=foo" << std::endl;
    g.raw_data_print_on(std::cout);
    std::cout << " = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = " << std::endl;
    std::cout << "      > " << program_name << " -n --input=foo" << std::endl;
    g.validation_names(std::cout);
    std::cout << " = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = " << std::endl;
    std::cout << "      > " << program_name << " -v --input=foo" << std::endl;
    g.validation(std::cout);
    std::cout << " = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = " << std::endl;
    std::cout << "      > " << program_name << " -s --input=foo" << std::endl;
    g.print_sanitary_names(std::cout);
    g.print_sanitary_data(std::cout);
    std::cout << " = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = " << std::endl;
  };
}
