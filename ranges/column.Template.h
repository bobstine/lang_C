#ifndef _COLUMN_TEMPLATE_H_
#define _COLUMN_TEMPLATE_H_

#include "column.h"
#include "debug.h"

#include "read_utils.h"
#include "string_trim.h"

#include <algorithm>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <fstream>
#include <sstream>


using debugging::debug;

//   Column   Column   Column   Column   Column   Column   Column   Column   Column   Column   Column   Column

template<class F>
Column<F>::Column()
: mData( new ColumnData<F>(0) )
{  }

template<class F>
Column<F>::Column(Column<F> const& c)
: mData(c.mData)
{
  ++c.mData->mRefCount;
}

template<class F>
Column<F>::Column(char const* name, int n)
: mData( new ColumnData<F>(n) )
{
  mData->mName = name;
  mData->mRole="";
  mData->mDescription="";
}

template<class F>  inline  std::string scan_string(F const);
template<>         inline  std::string scan_string(double const) { return "%lf"; }
template<>         inline  std::string scan_string(float const) { return "%f"; }


template<class F>
Column<F>::Column(char const* name, char const* description, size_t n, FILE *fp)
: mData( new ColumnData<F>(n) )
{
  mData->mName = name;
  mData->mRole = extract_role_from_string(description);
  mData->mDescription = description;
  F *x (mData->mBegin);
  int result = 0;
  std::string str = scan_string(*x);
  const char* format = str.c_str();
  while(n--)
    result = fscanf(fp, format, x++);   // watch for this one!
  if (result != 1)
  { std::cerr << "CLMN: *** ERROR *** Could not read sought column element with n=" << n << std::endl;
    std::cerr << "                    Last values read are " << *(x-2) << ", " << *(x-1) << ", " << (*x) << std::endl;
  }
  mData->init_properties();
}


template<class F>
template <class Iter>
Column<F>::Column(char const* name, char const* description, size_t n, Iter source) : mData( new ColumnData<F>(n) )
{
  mData->mName = name;
  mData->mRole = extract_role_from_string(description);
  mData->mDescription = description;
  F *x (mData->mBegin);
  while(n--)
  { *x = *source;
    ++x; ++source;
  }
  mData->init_properties();
}


template<class F>
template <class Iter>
Column<F>::Column(std::string name, std::string description, size_t n, Iter source) : mData( new ColumnData<F>(n) )
{
  mData->mName = name;
  mData->mRole = extract_role_from_string(description);
  mData->mDescription = description;
  F *x (mData->mBegin);
  while(n--)
  { *x = *source;
    ++x; ++source;
  }
  mData->init_properties();
}


template<class F>
template <class Iter, class Function>
Column<F>::Column(std::string name, std::string description, size_t n, Iter iter, Function const& func) : mData( new ColumnData<F>(n) )
{
  mData->mName = name;
  mData->mRole = extract_role_from_string(description);
  mData->mDescription = description;
  F *x (mData->mBegin);
  while(n--) {
    *x++ = func(iter);
    ++iter;
  }
  mData->init_properties();
}





template <class F>
Column<F>& 
Column<F>::operator= (Column<F> const& c)
{
  --mData->mRefCount;
  if(mData->mRefCount <= 0 && mData != c.mData) delete  mData;    // delete if not the same
  mData = c.mData;
  ++mData->mRefCount;
  return *this;
}

template <class F>
std::string
Column<F>::extract_role_from_string(std::string const& str) const
{
  size_t pos0 = 0, pos1 = 0;
  pos1 = str.find("=",pos0);
  if(pos1 == std::string::npos) return "";
  string name = str.substr(pos0,pos1);
  pos0 = pos1+1;
  pos1 = str.find(",",pos0);
  if(pos1 == std::string::npos) pos1 = str.size();
  string value = str.substr(pos0,pos1);
  name = trim(name);
  if("role" == name)
  { value = trim(value);
    return value;
  }
  else return "";
}


//  ColumnData    ColumnData    ColumnData    ColumnData    ColumnData    ColumnData    ColumnData    ColumnData

template <class F>
void
ColumnData<F>::print_to (std::ostream &os) const
{ 
  F *x (mBegin);
  int     n (mN);
  os << mName << "  [" << mDescription <<";  "
     << mNumUnique << "/" << n << ", " << mMin << "<" << mAvg << "<" << mMax <<  "]   {";
  if (mUniqueElements.size() > 0)
    std::for_each(mUniqueElements.begin(), mUniqueElements.end(), [&os](F x) { os << " " << x ;});
  else
    os << x[0] << ", " << x[1] << ", " << x[2] << ", ..., " << x[n-1];
  os << "}";
}


template <class F>
void
ColumnData<F>::init_properties ()
{
  if ((!mBegin) || (mN==0)) return;         // nothing to do 
  
  F *x = mBegin;
  std::set<F> uniq;
  double sum = 0.0;
  mMin = mMax = *x;
  while (x != end())
  { sum += *x;
    if (*x > mMax)
      mMax = *x;
    else if (*x < mMin)
      mMin = *x;
    uniq.insert(*x);
    ++x;
  }
  mAvg = (F)(sum/(double)mN);
  mNumUnique = (int) uniq.size();
  if (mNumUnique < 10)               // save the set if count is small
    mUniqueElements = uniq;
}



////    ColumnStream     ColumnStream     ColumnStream     ColumnStream     ColumnStream     ColumnStream     ColumnStream    

template <class F>
void
ColumnStream<F>::initialize()
{
  // read number of cases (ignore number of columns)
  std::string line;
  getline(mStream, line);
  std::istringstream ss(line);
  ss >> mN;
  debug("CLMN",4) << "ColumnStream '" << mStreamName << "' open; expecting n = " << mN << " cases per variable.\n";
}

template <class F>
bool
ColumnStream<F>::read_next_column()
{
  mCurrentName = "";
  if(!mStream.eof())
  { getline(mStream, mCurrentName);
    mCurrentName = read_utils::fill_blanks(read_utils::trim(mCurrentName));    // no embedded blanks
    mCurrentName = read_utils::remove_special_chars(mCurrentName, "*^");     // no special chars
  }
  if (mCurrentName.empty())
  { debug("CLMN",4) << "Stream '" << mStreamName << "' now empty.\n";
    mCurrentColumn = Column<F>();
    return false;
  }
  getline(mStream, mCurrentDesc);
  mCurrentColumn = Column<F>(mCurrentName, mCurrentDesc, mN, mStream);
  return true;
}

template <class F>
std::pair<int,int>
insert_columns_from_stream (std::istream& is, 
			    std::back_insert_iterator< std::vector<Column<F>> > it)
{
  ColumnStream<F> colStream(is, "column stream");
  int k (0);
  int n (colStream.n());

  for (Column<F> col = *colStream; col->size()>0; ++k, ++it)
  { *it = col;
    ++colStream;
    col = *colStream;
  }
  debug("CLMN",3) << "Inserted " << k << " columns from input stream, each of length " << n << std::endl;
  return std::make_pair(n,k);
}

template <class F>
void
insert_columns_from_stream (std::istream& is,
			    std::map<std::string, std::back_insert_iterator< std::vector<Column<F>> > > insertMap)
{
  typedef std::map<std::string, std::back_insert_iterator< std::vector<Column<F>> > > NamedColumnInsertMap;
  ColumnStream<F> colStream(is, "column stream");
  int k (0);
  int n (colStream.n());
  while(true) 
  { Column<F> col = *colStream;
    if(col->size() == 0) break;
    debug("CLMN",3) << "Reading column " << col->name() << " with description " << col->description()
			       << "  [0]=" << *(col->begin()) << "  [n]=" << *(col->end()-1) << std::endl;
    ++colStream;
    std::string role (col->role());
    if (role.empty())
    { debug("CLMN",4) << "Column '" << col->name() << "' lacks a role for the analysis; role x assigned.\n";
      role = "x";
    }
    typename NamedColumnInsertMap::iterator it (insertMap.find(role));
    if (it != insertMap.end()) // col has a role and its among those with inserters
    { ++k;                     // cannot use [] since no default constructor for back inserter
      *(it->second)=col;
      ++(it->second);
    }
    else
      debug("CLMN",-1) << "Inserter for column '" << col->name() << "' with role '" << role << "' not found; not inserted.\n";
  }
  debug("CLMN",2) << "Inserted " << k << " columns, each of length " << n << std::endl;
}


////    FileColumnStream     FileColumnStream     FileColumnStream     FileColumnStream     FileColumnStream     FileColumnStream     

template <class F>
bool
FileColumnStream<F>::open_file()
{
  mFile = fopen(mFileName.c_str(),"r");
  if (mFile)
  {
    // Read count from first file line
    mN = 0;
    int count = fscanf (mFile, "%d", &mN);
    if ( (count==1) && (mN > 0) )
    { debug("CLMN",3) << "File " << mFileName << " opened; n = " << mN << std::endl;
      return true;
    }
    else
    { std::cerr << "CLMN: Read invalid n = " << mN << std::endl;
      return false;
    }
  }
  else
  { std::cerr << "CLMN: Could not open file " << mFileName << std::endl;
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
    while ((--max > 0) && ((c = getc(iop)) != EOF))
    {
      if (c == ' ')         // put _ in place of blank
      { *cs++ = '_';
      }
      else
      { *cs = (char) c;
	if (*cs == '\n') break;
	++cs;
      }
    }
    *cs='\0';         // mark the end of the string
    // read description line
    while (--dMax > 0 && (c = getc(iop)) != EOF)
    {
      if ((*desc++ = (char) c) == '\n')
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
      else if ((*cs++ = (char) c) == '\n')
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

template<class F>
bool
FileColumnStream<F>::read_next_column_from_file()
{
  if (!mFile)
  {
    std::cerr << "CLMN: Error. File is not open for reading.\n";
    return false;
  }
  else
  { mCurrentName[0] = '\0';
    if (read_name_and_desc_after_skip(mCurrentName, maxColumnNameLength,
				      mCurrentDesc, maxColumnDescLength, mFile)) // don't gobble trailing /n; leave for next read
    { mCurrentColumn = Column<F>(mCurrentName, mCurrentDesc, mN, mFile);
      debug("CLMN",4) << "Current column from file has name `" << mCurrentName << "' with size " << mCurrentColumn->size() << std::endl;
      return true;
    }
    else
    { mCurrentColumn = Column<F>();
      debug("CLMN",3) << "Current column from file is empty with size " << mCurrentColumn->size() << std::endl;
      return false;
    }
  }
}

template<class F>
std::pair<int,int>
insert_columns_from_file (std::string const& fileName, 
                          std::back_insert_iterator< std::vector<Column<F>> > it)
{
  FileColumnStream<F> colStream(fileName);
  int k (0);
  int n (colStream.n());
  if (n>0)
  { std::clog << "CLMN: Reading column number ";
    for (Column<F> col = *colStream; col->size()>0; ++k, ++it)
    { std::clog << k << " ";
      *it = col;
      ++colStream;
      col = *colStream;
    }
    std::clog << std::endl;
  }
  if ((0 == n) || (0 == k))
    std::cerr << "CLMN: *** ERROR *** Read " << n << " cases and " << k << " columns from file " << fileName << std::endl;
  else
    debug("CLMN",1) << "Inserted " << k << " columns from " << fileName << ", each of length " << n << std::endl;
  return std::make_pair(n,k);
}

template <class F>
std::pair<int,int>
insert_columns_from_file (std::string const& fileName, int ny,
                          std::back_insert_iterator< std::vector<Column<F>> > yIt,
                          std::back_insert_iterator< std::vector<Column<F>> > xIt)
{
  FileColumnStream<F> colStream(fileName);
  int k (0);
  int n (colStream.n());
  for (int i=0; i<ny; ++i)
  { *yIt = *colStream;
    ++yIt;
    ++colStream;
  }
  for (Column<F> col = *colStream; col->size()>0; ++k, ++xIt)
  { *xIt = col;
    ++colStream;
    col = *colStream;
  }
  debug("CLMN",4) << "Inserted " << k << " columns from " << fileName << ", each of length " << n << std::endl;
  return std::make_pair(n,k);
}

template <class F>
int
insert_columns_from_file (FILE *is, std::string const& nameFileName, int nRows,
                            std::back_insert_iterator< std::vector<Column<F>> > it)
{
  FILE *nameFile;
  nameFile = fopen(nameFileName.c_str(), "r");
  if (not nameFile)
  { std::cerr << "COLM: Could not open name file to create columns; open of " << nameFileName << " failed.\n";
    return 0;
  }
  debug("CLMN", 3) << "Making columns from stream data with " << nRows << " rows.\n";
  std::vector<std::string> names;
  std::vector<F*> xPtrs;
  char name[maxColumnNameLength];
  char desc[maxColumnNameLength];
  while (read_name(name, maxColumnNameLength, nameFile))
  { std::string nameStr(name);
    read_file_line(desc, maxColumnNameLength, nameFile); // from utils/read_utils
    std::string descStr(desc);
    if (strlen(name) > 0)
    { names.push_back(nameStr);
      Column<F> col(name, desc, nRows, is);  // fill from file
      *it = col;
    }
  }
  return (int) names.size();
}

#endif
