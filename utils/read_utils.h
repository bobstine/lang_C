/* $Id: read_utils.h,v 1.4 2008/01/03 04:06:15 bob Exp $
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
class input_file_iterator: public std::iterator<std::input_iterator_tag,T>
{
  FILE *mInputFile;
  T mValue;
  int mState;

public:
  input_file_iterator()
    : mInputFile(0), mValue(0), mState(EOF)                            { }
  input_file_iterator(FILE *file)
    : mInputFile(file), mValue(0), mState(0)                           { read_next(); }
  input_file_iterator(input_file_iterator const& it)
    : mInputFile(it.mInputFile), mValue(it.mValue), mState(it.mState)  { }
	
  double               operator*()  const { return mValue; } 
  input_file_iterator& operator++()       { read_next(); return *this; }
  input_file_iterator  operator++(int)    { input_file_iterator it(*this); read_next(); return it; }

  bool   operator!=(input_file_iterator const& it) const { return (mState != it.mState); }
  bool   operator==(input_file_iterator const& it) const { return (mState == it.mState); }
  
  void print_to(std::ostream& os)  const       { os << mInputFile << ":" << mState << " -> " << mValue;  }

private:
    void read_next(void) { mState = fscanf(mInputFile,type_traits<T>::scanStr.c_str(), &mValue); }
};

template<class T>
inline
std::ostream&
operator<<(std::ostream& os, input_file_iterator<T> const& it)
{
  it.print_to(os);
  return os;
}

// These functions read an integer, fill a vector from a file

int
read_int_from_file(FILE* inputFile);

int
fill_vector_from_file(FILE* inputFile, std::vector<double>::iterator xit);
// reads n from the file first; return number read

int
fill_vector_from_file(FILE* inputFile, int n, std::vector<double>::iterator xit);
// you know how many to read; returns the number read



#endif