// $Id: file_utils.cc,v 1.3 2008/01/03 04:06:15 bob Exp $

#include "file_utils.h"

#include <stdio.h>
#include <iostream>

namespace File_Utils {
  
int count_fields (std::string const& fileName, int lineNumber)
{
    int counter, ch;
    int readingField;
    FILE *fp;
    
    // Open file as read only
    fp = fopen(fileName.c_str(), "r");
    if (fp == NULL)
    {	std::clog << "FILE: file " << fileName << " not found" << std::endl;
      return 0;
    };	
    // Skip the desired number of lines
    while (--lineNumber > 0)
      while ((ch = getc(fp)) != '\n')
        if (ch == EOF) return (-2);			
    // Read line, counting the number of space-delimited fields.
    counter = 0;
    readingField = false;
    while ((ch = getc(fp)) != '\n')
    {	// printf("Read %c reading field %d -> ", ch, readingField);
      if (isspace(ch))
      {
        if (readingField == true)
        { ++counter; 
          readingField = false;
        }
      }
      else if (readingField == false)
        readingField = true;		
      // printf("%d\n", readingField);
    }
    if (readingField) ++ counter;
    // Close file and return total number of columns
    fclose(fp);
    return (counter);
}
  
  
  int count_lines (std::string const& fileName)
  {
    int ch;
    long line; 
    FILE *fp;
	
    // Open file as read only
    fp = fopen(fileName.c_str(), "r");
    if (fp == NULL)
      return -1;
    else
    {
      // count the number of lines as the number of \n found
      line = 0;
      while (EOF != (ch = getc(fp)))
	if (ch == '\n') ++ line;
      // Close file and return number of lines
      fclose(fp);
      return (line);
    }
  }

}
