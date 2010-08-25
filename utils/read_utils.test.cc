/*
 *  read_utils.test.cc
 *  utils
 *
 *  Created by Robert Stine on 11/30/05.
 *  Copyright 2005. All rights reserved.
 *
 
 The fscanf io defined by the input iterator defined here reads homogeneous input data (such as
 all doubles) about 10 times faster than from an istream, presumably because the input is homogeneous. 
 
 Next: Filling a vector?  Or is this done via ranges?  Need templates, since a back inserter is not the
       the same as a vector::iterator.  Confused about whether I want to have read routine insert and
       expand, or just fill an existing range.
 
       Maybe easier just to have 2 functions, one called fill and the other called expand or create.
 */


#include "read_utils.h"
#include "print_utils.h"

#include <vector>
#include <algorithm>
#include <ctime>
#include <fstream>

int
main()
{

  // test string trim
  std::string both("  both  ");
  std::string left("   left");
  std::string right("right  ");
  std::cout << "Trim:  [" << both  << "]  ->  [" << read_utils::trim(both)  << "]\n";
  std::cout << "Trim:  [" << left  << "]  ->  [" << read_utils::trim(left)  << "]\n";
  std::cout << "Trim:  [" << right << "]  ->  [" << read_utils::trim(right) << "]\n";

  std::cout << "\nGet first word from string 'first word'" << get_word_from_string("first word") << std::endl;
  std::cout << "Get first word from string 'firstword'" << get_word_from_string("firstword") << std::endl;

  { // special chars
    std::string test ("abcd^^gh*jk^*[]]");
    std::cout << "\nRemove special charaters from " << test << " gives " << read_utils::remove_special_chars(test,"^*[]") << std::endl;
    test = "^^cd*fg^*[]mmm";
    std::cout << "\nRemove special charaters from " << test << " gives " << read_utils::remove_special_chars(test,"^*[]") << std::endl;
  }
  
  std::string       fileName("/Users/bob/C/utils/test/big_test.dat");
  std::string prefixFileName("/Users/bob/C/utils/test/prefix_test.dat");
  const int maxLength (1023);
  char str[maxLength];
    
  FILE *file;
  
  const int nToRead (25000);  // last one craps out if this is set to less than the file size (reads until end of file)
  const int nRepeat (10);
  std::cout << "TEST: Reading floating vector of " << nToRead << " elements.\n";
  
  file = fopen (fileName.c_str(), "r");
  if(!file)
  {
    fclose(file);
    std::cout << "TEST: Could not open input file.\n" ;
    return -1;
  }
  else    
  {
    { // check read line
      int n;
      n = read_file_line(str, maxLength, file);
      std::string s (str);
      std::cout << "TEST: Read " << n << " chars from file; string is " << s << std::endl;
    }
    
    { // now read numbers using standard istream 
      std::vector<double> x (nToRead);
      clock_t time = clock();
      for(int j=0; j<nRepeat; ++j)
      { std::ifstream input(fileName.c_str());
        for (int i=0; i<nToRead; ++i)
        {
          input >> x[i] ;
        }
      }
      std::cout << "TEST: standard istream read took " << clock()-time << " ticks; x[n]= " << x[nToRead-1] << std::endl;
    }
    
    { // now use the file iterator within loop
      std::vector<double> x (nToRead);
      clock_t time = clock();
      for(int j=0; j<nRepeat; ++j)
      { file = fopen (fileName.c_str(), "r");
        read_file_iterator<double> it(file);
        for (int i=0; i<nToRead; ++i)
        {
          x[i] = *it;
          ++it;
        }
        fclose(file);
      }
      std::cout << "TEST: loop with iterator took " << clock()-time << " ticks; x[n]= " << x[nToRead-1] << std::endl;
    }
    
    { //  with iterator and a copy algorithm
      std::vector<double> x (nToRead);
      clock_t time = clock();
      for(int j=0; j<nRepeat; ++j)
      { file = fopen (fileName.c_str(), "r");
        read_file_iterator<double> it(file);
        read_file_iterator<double> stop;
        std::copy(it,stop,x.begin());
        fclose(file);
      }
      std::cout << "TEST: std::copy with iterator took " << clock()-time << " ticks; x[n]= " << x[nToRead-1] << std::endl;
    }
     
    { //  and last with an iterator and a copy algorithm using a back_inserter
      clock_t time = clock();
      for(int j=0; j<nRepeat; ++j)
      { std::vector<double> x;
        std::back_insert_iterator< std::vector<double> > dest(x);
        file = fopen (fileName.c_str(), "r");
        read_file_iterator<double> it(file);
        read_file_iterator<double> stop;
        std::copy(it,stop,dest);
        fclose(file);
      }
      std::cout << "TEST: std::copy with into back_iterator took " << clock()-time << " ticks." << std::endl;
    }
    
    { // and now a test of the routines that fill a vector
      std::vector<double> x;
      std::back_insert_iterator< std::vector<double> > xIter (x);
      file = fopen (prefixFileName.c_str(), "r");
      if(!file)
      { std::cout << "TEST: Could not open prefix input file.\n" ;
        return -1;
      }
      int n;
      n = fill_iterator_from_file(file, xIter);
      std::cout << "TEST: Read " << n << " from prefixed file... " << x << std::endl;
    }
    
    return 0;
  }
  
}


