// -*- c++ -*-
#ifndef _COLUMN_STREAM_H_
#define _COLUMN_STREAM_H_

#include "column.h"

#include <string>
#include <vector>
#include <map>
#include <iterator>

#include <stdio.h>   // FILE

// Read columns as a stream from a stream input object:
//        First line of stream gives number of observations expected for each column.
//        Seems to work okay so long as this number is >= to the number present.
//        Then come triples of lines for each column: name, description, data
//        These classes only handle pure numerical columns.

template <class F>
class ColumnStream : public std::iterator<std::forward_iterator_tag, Column<F> >
{
  std::istream&    mStream;
  std::string      mStreamName;
  int              mN;
  int              mCount;
  std::string      mCurrentName;
  std::string      mCurrentDesc;
  Column<F>        mCurrentColumn;
  
 public:
  ~ColumnStream() {  }
  
  ColumnStream (std::istream& is, std::string name)
    :  mStream(is), mStreamName(name), mN(0), mCount(0), mCurrentName(), mCurrentDesc(), mCurrentColumn()
    { if (is) { initialize(); read_next_column(); } }

  std::string      currentName()        const { return mCurrentName; }
  std::string      currentDescription() const { return mCurrentDesc; }
  
  Column<F>        operator*()          const { return mCurrentColumn; }
  ColumnStream<F>& operator++()               { ++mCount; read_next_column(); return *this; }

  int              position()           const { return mCount; }
  int              n()                  const { return mN;     }
  
 private:
  void initialize();
  bool read_next_column();
};


// This version makes columns from an input file using C io rather than C++ (faster this way)

template <class F>
class FileColumnStream : public std::iterator<std::forward_iterator_tag, Column<F> >
{
  std::string      mFileName;
  int              mN;
  int              mCount;
  char             mCurrentName[maxColumnNameLength];
  char             mCurrentDesc[maxColumnDescLength];
  Column<F>        mCurrentColumn;
  FILE*            mFile;
  
 public:
  ~FileColumnStream() { if (mFile) fclose(mFile); }
  
  FileColumnStream (std::string const& fileName)
    :
    mFileName(fileName), mN(0), mCount(0), mCurrentName(), mCurrentDesc(), mCurrentColumn(), mFile(0)
    { open_file(); read_next_column_from_file(); }
  
  Column<F>            operator*()  const { return mCurrentColumn; }
  FileColumnStream<F>& operator++()       { ++mCount; read_next_column_from_file(); return *this; }

  int                  position()   const { return mCount; }
  int                  n()          const { return mN;     }
  void                 close_file()       { fclose(mFile); }
  
 private:
  bool open_file();
  bool read_next_column_from_file();
};

//  insert_columns_from...
//
//  Each returns n obs and number columns appended. See code for each as to whether
//  function expects stream or file to be prefixed with number of cases for each column.
//  Only the next allows mapped variables (eg, eigenwords) and missing values (F = float).
//  The dictionary *must* define a response for NA (missing) and OOV (not in dictionary).
//  Returns pair: number columns read from stream, number columns written
			
std::pair<size_t,size_t>
insert_columns_from_stream (std::istream& is,
			    std::map<std::string, std::vector<float>> const& dictionary,      // handles domain=word
			    std::back_insert_iterator< std::vector<Column<float>> > it);

//  only numerical columns for these; returns dimension of implied matrix

template<class F>
std::pair<int,int>
insert_numerical_columns_from_stream (std::istream &is, 
				      std::back_insert_iterator< std::vector<Column<F>> > it);


template<class F>
std::pair<int,int>
insert_numerical_columns_from_file (std::string const& fileName, int ny,
				    std::back_insert_iterator< std::vector<Column<F>> > yIt,
				    std::back_insert_iterator< std::vector<Column<F>> > xIt);

template<class F>
std::pair<int,int>
insert_numerical_columns_from_file (FILE *is, std::string const& nameFileName, int nRows,
				    std::back_insert_iterator< std::vector<Column<F>> > it);

#endif
