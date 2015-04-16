#ifndef _COLUMN_STREAM_TEMPLATE_H_
#define _COLUMN_STREAM_TEMPLATE_H_

#include "column_stream.h"
#include "debug.h"
#include <cstring>   // strlen

////    ColumnStream     ColumnStream     ColumnStream     ColumnStream     ColumnStream     ColumnStream     ColumnStream    

template <class F>
void
ColumnStream<F>::initialize()
{
  std::string line;         // read number of cases (ignore number of columns)
  getline(mStream, line);
  std::istringstream ss(line);
  ss >> mN;
  debugging::debug("CLMN",3) << "ColumnStream '" << mStreamName << "' open; expecting n = " << mN << " cases per variable.\n";
}

template <class F>
bool
ColumnStream<F>::read_next_column()
{
  mCurrentName = "";
  if(!mStream.eof())
  { getline(mStream, mCurrentName);
    read_utils::cleanup_name(mCurrentName);
  }
  if (mCurrentName.empty())
  { debugging::debug("CLMN",4) << "Stream '" << mStreamName << "' now empty.\n";
    mCurrentColumn = Column<F>();
    return false;
  }
  getline(mStream, mCurrentDesc);
  mCurrentColumn = Column<F>(mCurrentName, mCurrentDesc, mN, mStream);
  return true;
}

template <class F>
std::pair<int,int>
insert_numerical_columns_from_stream (std::istream& is, 
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
  debugging::debug("CLMN",2) << "Inserted " << k << " columns from input stream, each of length " << n << std::endl;
  return std::make_pair(n,k);
}

	       

template <class F>
void
insert_numerical_columns_from_stream (std::istream& is,
			    std::map<std::string, std::back_insert_iterator< std::vector<Column<F>> > > insertMap)
{
  typedef std::map<std::string, std::back_insert_iterator< std::vector<Column<F>> > > NamedColumnInsertMap;
  ColumnStream<F> colStream(is, "column stream");
  int k (0);
  int n (colStream.n());
  while(true) 
  { Column<F> col = *colStream;
    if(col->size() == 0) break;
    debugging::debug("CLMN",3) << "Reading column " << col->name() << " with attributes " << col->attributes()
			       << "  [0]=" << *(col->begin()) << "  [n]=" << *(col->end()-1) << std::endl;
    ++colStream;
    std::string role (col->role());
    if (role.empty())
    { debugging::debug("CLMN",4) << "Column '" << col->name() << "' lacks a role for the analysis; role x assigned.\n";
      role = "x";
    }
    typename NamedColumnInsertMap::iterator it (insertMap.find(role));
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
      debugging::debug("CLMN",4) << "Current column from file has name `" << mCurrentName << "' with size " << mCurrentColumn->size() << std::endl;
      return true;
    }
    else
    { mCurrentColumn = Column<F>();
      debugging::debug("CLMN",3) << "Current column from file is empty with size " << mCurrentColumn->size() << std::endl;
      return false;
    }
  }
}

template<class F>
std::pair<int,int>
insert_numerical_columns_from_file (std::string const& fileName, 
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
    debugging::debug("CLMN",1) << "Inserted " << k << " columns from " << fileName << ", each of length " << n << std::endl;
  return std::make_pair(n,k);
}

template <class F>
std::pair<int,int>
insert_numerical_columns_from_file (std::string const& fileName, int ny,
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
 debugging::debug("CLMN",4) << "Inserted " << k << " columns from " << fileName << ", each of length " << n << std::endl;
  return std::make_pair(n,k);
}

template <class F>
int
insert_numerical_columns_from_file (FILE *is, std::string const& nameFileName, int nRows,
				    std::back_insert_iterator< std::vector<Column<F>> > it)
{
  FILE *nameFile;
  nameFile = fopen(nameFileName.c_str(), "r");
  if (not nameFile)
  { std::cerr << "COLM: Could not open name file to create columns; open of " << nameFileName << " failed.\n";
    return 0;
  }
  debugging::debug("CLMN", 3) << "Making columns from stream data with " << nRows << " rows.\n";
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
