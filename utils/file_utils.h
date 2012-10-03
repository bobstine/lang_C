// $Id: file_utils.h,v 1.1 2003/06/04 19:09:25 bob Exp $

/*
   4 Jun 03 ... Handy file utilities; from old fileOps.
*/

#ifndef _FILE_UTILS_H_
#define _FILE_UTILS_H_

#include <string>
  
namespace File_Utils {

  int
    count_fields (std::string const& fileName, int lineNumber = 0);

  // Counts number of space delimited column fields on chosen line

  
  int
    count_lines (std::string const& fileName);

  // Counts the number of lines found in the text file.


}

#endif
