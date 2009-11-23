/* 
 *  read_utils.h
 *  utils
 *
 *  Created by Robert Stine on 11/29/05.
 *  Copyright 2005. All rights reserved.
 *
 */

#ifndef _READ_UTILS_H_
#define _READ_UTILS_H_

#include <vector>
#include <iterator>
#include <iostream>
#include <cstdio>


// Read rest of current file line into c char string of indicated length

int
read_file_line (char *s, int maxLength, FILE *fp);


// Read vector of arbitrary length, emptying input stream

std::istream&
operator>>(std::istream& input, std::vector<double>& vec);


// Read into range of a vector defined by begin/end pairs; returns number read

int 
fill_vector_from_stream(std::istream& input, 
			std::vector<double>::iterator b, std::vector<double>::iterator e);

//  c io input from file iterator

template<class T>
class type_traits;

template <> 
class type_traits<double> 
{
public:
  static std::string scanStr;
};

template <> 
class type_traits<int> 
{
public:
  static std::string scanStr;
};


template <class T>
class read_file_iterator: public std::iterator<std::output_iterator_tag,T>
{
  FILE *mInputFile;
  T mValue;
  int mState;

public:
  read_file_iterator()
    : mInputFile(0), mValue(0), mState(EOF)                            { }
  read_file_iterator(FILE *file)
    : mInputFile(file), mValue(0), mState(0)                           { read_next(); }
  read_file_iterator(read_file_iterator const& it)
    : mInputFile(it.mInputFile), mValue(it.mValue), mState(it.mState)  { }
	
  double              operator*()  const { return mValue; } 
  read_file_iterator& operator++()       { read_next(); return *this; }
  read_file_iterator  operator++(int)    { read_file_iterator it(*this); read_next(); return it; }

  bool   operator!=(read_file_iterator const& it) const { return (mState != it.mState); }
  bool   operator==(read_file_iterator const& it) const { return (mState == it.mState); }
  
  void print_to(std::ostream& os)  const       { os << mInputFile << ":" << mState << " -> " << mValue;  }

private:
    void read_next(void) { mState = fscanf(mInputFile,type_traits<T>::scanStr.c_str(), &mValue); }
};

template<class T>
inline
std::ostream&
operator<<(std::ostream& os, read_file_iterator<T> const& it)
{
  it.print_to(os);
  return os;
}

// These functions read an integer, fill a vector from a file

int
read_int_from_file(FILE* inputFile);

template <class Iter>
int
fill_iterator_from_file(FILE* inputFile, int n, Iter xIter)
{
  // alas, the type of a back_insert_iterator is void so the next line fails...
  // read_file_iterator<typename std::iterator_traits<Iter>::value_type> fileIter(inputFile);
  read_file_iterator<double> fileIter(inputFile);
  int i;
  for(i=0; inputFile && (i<n); ++i)
  { *xIter = *fileIter;
    ++fileIter;
    ++xIter;
  }
  return i;
}

template <class Iter>
int
fill_iterator_from_file(FILE* inputFile, Iter xIter)
{
  const int n(read_int_from_file(inputFile));
  return fill_iterator_from_file(inputFile, n, xIter);
}


#endif
