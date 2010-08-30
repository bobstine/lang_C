#include "column.h"
#include "debug.h"

#include "read_utils.h"

#include <set>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <fstream>
#include <sstream>


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


std::string
Column::extract_role_from_string(std::string const& str) const
{
  std::istringstream iss(str);
  std::string term ("");
  iss >> std::ws >> term;
  if("role" == term)
  { iss >> std::ws >> term;
    return term;
  }
  else
    return "";
}


//  ColumnData    ColumnData    ColumnData    ColumnData    ColumnData    ColumnData    ColumnData    ColumnData

void
ColumnData::print_to (std::ostream &os) const
{ 
  double *x (mBegin);
  int     n (mN);
  os << mName << "  [" << mDescription <<";  "
     << mNumUnique << "/" << n << ", " << mMin << "<" << mAvg << "<" << mMax <<  "]     {"
     << x[0] << ", " << x[1] << ", " << x[2] << ", ... " << x[n-1] << "} " ;
}


void
ColumnData::init_properties ()
{
  if ((!mBegin) || (mN==0)) return;         // nothing to do 
  
  double *x = mBegin;
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
  mNumUnique = (int) uniq.size();
}



////    ColumnStream     ColumnStream     ColumnStream     ColumnStream     ColumnStream     ColumnStream     ColumnStream    

void
ColumnStream::initialize()
{
  // read number of cases (ignore number of columns)
  std::string line;
  getline(mStream, line);
  std::istringstream ss(line);
  ss >> mN;
  debugging::debug("CLMN",4) << "ColumnStream '" << mStreamName << "' open; expecting n = " << mN << " cases per variable.\n";
}

bool
ColumnStream::read_next_column()
{
  mCurrentName = "";
  if(!mStream.eof())
  { getline(mStream, mCurrentName);
    mCurrentName = read_utils::fill_blanks(read_utils::trim(mCurrentName));    // no embedded blanks
    mCurrentName = read_utils::remove_special_chars(mCurrentName, "*^");     // no special chars
  }
  if (mCurrentName.empty())
  { debugging::debug("CLMN",4) << "Stream '" << mStreamName << "' now empty.\n";
    mCurrentColumn = Column();
    return false;
  }
  getline(mStream, mCurrentDesc);
  mCurrentColumn = Column(mCurrentName, mCurrentDesc, mN, mStream);
  return true;
}


std::pair<int,int>
insert_columns_from_stream (std::istream& is, 
			    std::back_insert_iterator< std::vector<Column> > it)
{
  ColumnStream colStream(is, "column stream");
  int k (0);
  int n (colStream.n());

  for (Column col = *colStream; col->size()>0; ++k, ++it)
  { *it = col;
    ++colStream;
    col = *colStream;
  }
  debugging::debug("CLMN",3) << "Inserted " << k << " columns from input stream, each of length " << n << std::endl;
  return std::make_pair(n,k);
}

void
insert_columns_from_stream (std::istream& is, NamedColumnInsertMap insertMap)
{
  ColumnStream colStream(is, "column stream");
  int k (0);
  int n (colStream.n());
  while(true) 
  { Column col = *colStream;
    if(col->size() == 0) break;
    debugging::debug("CLMN",4) << "Reading column " << col->name() << " with description " << col->description()
			       << "  [0]=" << *(col->begin()) << "  [n]=" << *(col->end()-1) << std::endl;
    ++colStream;
    std::string role (col->role());
    if (role.empty())
    { debugging::debug("CLMN",4) << "Column '" << col->name() << "' lacks a role for the analysis; role x assigned.\n";
      role = "x";
    }
    NamedColumnInsertMap::iterator it (insertMap.find(role));
    if (it != insertMap.end()) // col has a role and its among those with inserters
    { ++k;                     // cannot use [] since no default constructor for back inserter
      *(it->second)=col;
      ++(it->second);
    }
    else
      debugging::debug("CLMN",-1) << "Inserter for column '" << col->name() << "' with role '" << role << "' not found; not inserted.\n";
  }
  debugging::debug("CLMN",2) << "Inserted " << k << " columns, each of length " << n << std::endl;
}


////    FileColumnStream     FileColumnStream     FileColumnStream     FileColumnStream     FileColumnStream     FileColumnStream     

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
    { debugging::debug("CLMN",3) << "File " << mFileName << " opened; n = " << mN << std::endl;
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
    while (--max > 0 && (c = getc(iop)) != EOF)
    {
      if (c == ' ')         // put _ in place of blank
	*cs++ = '_';
      else if ((*cs++ = (char) c) == '\n')
	break;
    }
    --cs; *cs='\0';         // mark the end of the string
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

bool
FileColumnStream::read_next_column_from_file()
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
  debugging::debug("CLMN",4) << "Inserted " << k << " columns from " << fileName << ", each of length " << n << std::endl;
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
  debugging::debug("CLMN",4) << "Inserted " << k << " columns from " << fileName << ", each of length " << n << std::endl;
  return std::make_pair(n,k);
}

int
insert_columns_from_file (FILE *is, std::string const& nameFileName, int nRows,
                            std::back_insert_iterator< std::vector<Column> > it)
{
  FILE *nameFile;
  nameFile = fopen(nameFileName.c_str(), "r");
  if (not nameFile)
  { std::cerr << "COLM: Could not open name file to create columns; open of " << nameFileName << " failed.\n";
    return 0;
  }
  debugging::debug("CLMN", 3) << "Making columns from stream data with " << nRows << " rows.\n";
  std::vector<std::string> names;
  std::vector<double*> xPtrs;
  char name[maxColumnNameLength];
  char desc[maxColumnNameLength];
  while (read_name(name, maxColumnNameLength, nameFile))
  { std::string nameStr(name);
    read_file_line(desc, maxColumnNameLength, nameFile); // from utils/read_utils
    std::string descStr(desc);
    if (strlen(name) > 0)
    { names.push_back(nameStr);
      Column col(name, desc, nRows, is);  // fill from file
      *it = col;
    }
  }
  return (int) names.size();
}



//   IntegerColumn      IntegerColumn      IntegerColumn      IntegerColumn      IntegerColumn      IntegerColumn      IntegerColumn      

void
IntegerColumn::print_to(std::ostream &os) const
{
  int counter (mData->size());
  std::string more ("");
  // print up to 10 elements
  if (counter > 10)
  { counter = 10;
    more = "...";
  }
  os << mData->name() << "[" << mData->size() << "]";
  int * pInt (mData->begin());
  while(counter--) os << " " << *pInt++;
  os << more;
}

void
IntegerColumn::transfer_from_double (double *pDouble)
{
  int counter (mData->size());
  int *pDest  (mData->begin()); 
  while(counter--) *pDest++ = floor(*pDouble++);
}
