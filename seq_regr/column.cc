//  $Id: column.cc,v 1.16 2008/01/22 21:15:07 bob Exp $

#include "column.h"

#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <fstream>


Column& 
Column::operator= (Column const& c)
{
  mName = c.mName;
  mAvg = c.mAvg;
  mMin = c.mMin;
  mMax = c.mMax;
  mUnique = c.mUnique;
  mRange = make_range(c.mRange);
  init_fields();
  return *this;
}

void
Column::print_to (std::ostream &os) const
{ 
  double *x(Ranges::begin(mRange));
  int n(size());
  os << "Column " << mName << " [" << mUnique << "/" << n << ", "
     << mMin << " < " << mAvg << " < " << mMax << "] "
     << x[0] << ", " << x[1] << ", " << x[2] << ", ... " << x[n-1] ;
}

void
Column::init_fields ()
{
  double *x = Ranges::begin(mRange);
  if (!x) return;  // nothing to do 
  
  std::set<double> uniq;
  int n (0);
  mMin = mMax = *x;
  while (x != end(mRange))
  { ++n;
    mAvg += *x;
    if (*x > mMax)
      mMax = *x;
    else if (*x < mMin)
      mMin = *x;
    uniq.insert(*x);
    ++x;
  }
  mAvg /= n;
  mUnique = uniq.size();
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
  read_name_with_skip(char *s, int max, register FILE *iop)
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
    bool addedEOL (false);
    while (--max > 0 && (c = getc(iop)) != EOF)
    {
      if (c == ' ')   // put _ in place of blank
	*cs++ = '_';
      else if ((*cs++ = c) == '\n')
	{ addedEOL = true;
	  break;
	}
    }
    if (addedEOL)
    { --cs;
      *cs='\0';
    }
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
    mCurrentAvg = 0.0;
    mMin = mMax = 0.0;
    mUniqueSet = std::set<double>();
    if (read_name_with_skip(mCurrentName, maxNameLength, mFile))
    {
      mCurrentColumn = new double[mN];         // somebody else has to free this
      double *x(mCurrentColumn);
      fscanf(mFile, "%lf", x);                 // init using first value
      mCurrentAvg = mMin = mMax = *x;
      mUniqueSet.insert(*x);
      ++x;
      for (int i=1; i<mN; ++i, ++x)
      { fscanf(mFile, "%lf", x);
        mCurrentAvg += *x;
        mUniqueSet.insert(*x);
        if (*x < mMin)
          mMin = *x;
        else if (*x > mMax)
          mMax = *x;
      }
      mCurrentAvg /= mN;
      return true;
      // do not gobble a trailing /n; that's handled by next read_name
    }
    else
    {
      mN = 0;
      mCurrentName[0] = '\0';  // make sure name is empty
      mCurrentAvg     = 0.0;
      mCurrentColumn    = 0;
      return false;
    }
  }
}


std::pair<double,double>
insert_columns_from_file (std::string const& fileName, 
                          std::back_insert_iterator< std::vector<Column> > it)
{
  FileColumnStream colStream(fileName);
  int k (0);
  int n (colStream.n());
  for (Column col = *colStream; col.size()>0; ++k, ++it)
  { *it = col;
    ++colStream;
    col = *colStream;
  }
  std::cout << "CLMN: Inserted " << k << " columns from " << fileName << ", each of length " << n << std::endl;
  return std::make_pair(n,k);
}

std::pair<double,double>
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
  for (Column col = *colStream; col.size()>0; ++k, ++xIt)
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
    if (name != "")
    { names.push_back(nameStr);
      double *x = new double [nRows];
      if (x)
        xPtrs.push_back(x);
      else
      { std::cout << "COLM: Allocation of columns fails at column " << names.size() << "; returning.\n";
        return names.size();
      }
    }
  }
  std::cout << "COLM: Allocated " << names.size() << " pointers of size " << nRows << " for each column.\n";
  for (int row=0; row<nRows; ++row)
    for (unsigned int j=0; j < names.size(); ++j)
      fscanf(is, "%lf", &xPtrs[j][row]);
  std::cout << "COLM: Data read into the ranges; now inserting the columns.\n";
  for (unsigned int j=0; j<names.size(); ++j, ++it)
    *it = Column(names[j].c_str(), xPtrs[j], xPtrs[j]+nRows);
  return names.size();
}
