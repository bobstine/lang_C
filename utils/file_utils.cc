// $Id: file_utils.cc,v 1.3 2008/01/03 04:06:15 bob Exp $

#include "file_utils.h"

#include <dirent.h>

#include <stdio.h>
#include <iostream>
#include <fstream>
#include <ios>

namespace FileUtils {
  
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
      return ((int)line);
    }
  }

  //     files_in_directory     files_in_directory     files_in_directory     files_in_directory     files_in_directory
  std::vector<std::string>
  files_in_directory (std::string dir, bool verbose)
  {
    DIR *dp;
    struct dirent *dirp;
    
    std::vector<std::string> files;
    if((dp  = opendir(dir.c_str())) == NULL)
      { std::cerr << "Error(" << errno << ") opening " << dir << std::endl;
	return files;
      }
    while ((dirp = readdir(dp)) != NULL)
      files.push_back(std::string(dirp->d_name));
    closedir(dp);
    if (verbose)
      { std::clog << "     Found the following files: " ;
	for(auto f : files) std::clog << f << ",";
	std::clog << std::endl;
      }
    return files;
  }


  
}
