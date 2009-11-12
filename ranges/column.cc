//  $Id: column.cc,v 1.16 2008/01/22 21:15:07 bob Exp $

#include "column.h"

#include <set>
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <fstream>


//   Column   Column   Column   Column   Column   Column   Column   Column   Column   Column   Column   Column


Column& 
Column::operator= (Column const& c)
{
  if(--mData->mRefCount <= 0 && mData != c.mData) delete mData;  // decrement mine and delete if not the same
  mData = c.mData;
  ++mData->mRefCount;
  mData->mName = mData->mName;                                   // add  + "_C"  to mark for a copy
  return *this;
}


//  ColumnData    ColumnData    ColumnData    ColumnData    ColumnData    ColumnData    ColumnData    ColumnData

void
ColumnData::print_to (std::ostream &os) const
{ 
  double *x (mBegin);
  int     n (mN);
  os << mName << " [" << mNumUnique << "/" << n << ", "
     << mMin << "<" << mAvg << "<" << mMax << "; " << mDescription << "] \n     {"
     << x[0] << ", " << x[1] << ", " << x[2] << ", ... " << x[n-1] << "} " ;
}


void
ColumnData::init_properties ()
{
  double *x = mBegin;
  if ((!x) || (mN==0)) return;         // nothing to do 
  
  std::set<double> uniq;
  mAvg = 0.0;
  mMin = mMax = *x;
  while (x != end())
  { mAvg += *x;
    if (*x > mMax)
      mMax = *x;
    else if (*x < mMin)
      mMin = *x;
    uniq.insert(*x);
    ++x;
  }
  mAvg /= mN;
  mNumUnique = uniq.size();
}



////  file column stream  --  file column stream  --   file column stream  --   file column stream  --

bool
FileColumnStream::open_file()
{
  mFile = fopen(mFileName.c_str(),"r");
  if (mFile)
  {
    // Read count from first file line
    mN = 0;
    int count = fscanf (mFile, "%d", &mN);
    if ( (count==1) && (mN > 0) )
    { std::cout << "CLMN: File " << mFileName << " opened; n = " << mN << std::endl;
      return true;
    }
    else
    { std::cout << "CLMN: Read invalid n = " << mN << std::endl;
      return false;
    }
  }
  else
  { std::cout << "CLMN: Could not open file " << mFileName << std::endl;
    return false;
  }
}



namespace {
  bool
  read_name_and_desc_after_skip(char *s, int max, char *desc, int dMax, register FILE *iop)
  {
    register int c;
    // skip to next line
    while ((c = getc(iop)) != EOF)
    {
      if (c == '\n') break;
    }
    if (c == EOF) return false;
    // read a string name, and position at start of next line (removes blanks)
    register char *cs;
    cs = s;
    while (--max > 0 && (c = getc(iop)) != EOF)
    {
      if (c == ' ')         // put _ in place of blank
	*cs++ = '_';
      else if ((*cs++ = c) == '\n')
	break;
    }
    --cs; *cs='\0';         // mark the end of the string
    // read description line
    while (--dMax > 0 && (c = getc(iop)) != EOF)
    {
      if ((*desc++ = c) == '\n')
	break;
    }
    --desc; *desc = '\0';
    return (cs != s);
  }

  bool
  read_name(char *s, int max, register FILE *iop)
  {
    register int c;
    register char *cs;
    bool addedEOL (false);
    cs = s;
    while (--max > 0 && (c = getc(iop)) != EOF)
    {
      if (c == ' ')   // put _ in place of blank
        *cs++ = '_';
      else if ((*cs++ = c) == '\n')
      { addedEOL = true;
        break;
      }
    }
    if (addedEOL)  // dump the end of line char
    { --cs;
      *cs='\0';
    }
    return (cs != s);
  }
};

bool
FileColumnStream::read_next_column_from_file()
{
  if (!mFile)
  {
    std::cout << "COLM: Error. File is not open for reading.\n";
    return false;
  }
  else
  { mCurrentName[0] = '\0';
    if (read_name_and_desc_after_skip(mCurrentName, maxNameLength, mCurrentDesc, maxDescLength, mFile)) // don't gobble trailing /n; leave for next read
    { mCurrentColumn = Column(mCurrentName, mCurrentDesc, mN, mFile);
      return true;
    }
    else
    { mCurrentColumn = Column();
      return false;
    }
  }
}


std::pair<int,int>
insert_columns_from_file (std::string const& fileName, 
                          std::back_insert_iterator< std::vector<Column> > it)
{
  FileColumnStream colStream(fileName);
  int k (0);
  int n (colStream.n());
  for (Column col = *colStream; col->size()>0; ++k, ++it)
  { *it = col;
    ++colStream;
    col = *colStream;
  }
  std::cout << "CLMN: Inserted " << k << " columns from " << fileName << ", each of length " << n << std::endl;
  return std::make_pair(n,k);
}

std::pair<int,int>
insert_columns_from_file (std::string const& fileName, int ny,
                          std::back_insert_iterator< std::vector<Column> > yIt,
                          std::back_insert_iterator< std::vector<Column> > xIt)
{
  FileColumnStream colStream(fileName);
  int k (0);
  int n (colStream.n());
  for (int i=0; i<ny; ++i)
  { *yIt = *colStream;
    ++yIt;
    ++colStream;
  }
  for (Column col = *colStream; col->size()>0; ++k, ++xIt)
  { *xIt = col;
    ++colStream;
    col = *colStream;
  }
  std::cout << "CLMN: Inserted " << k << " columns from " << fileName << ", each of length " << n << std::endl;
  return std::make_pair(n,k);
}

int
insert_columns_from_stream (FILE *is, std::string const& nameFileName, int nRows,
                            std::back_insert_iterator< std::vector<Column> > it)
{
  FILE *nameFile;
  nameFile = fopen(nameFileName.c_str(), "r");
  if (not nameFile)
  { std::cout << "COLM: Could not open name file to create columns; open of " << nameFileName << " failed.\n";
    return 0;
  }
  std::cout << "COLM: Making columns from stream data with " << nRows << " rows.\n";
  std::vector<std::string> names;
  std::vector<double*> xPtrs;
  char name[maxNameLength];
  while (read_name(name, maxNameLength, nameFile))
  { std::string nameStr(name);
    if (strlen(name) > 0)
    { names.push_back(nameStr);
      Column col(name, " ", nRows, is);  // fill from file
      *it = col;
    }
  }
  return names.size();
}

