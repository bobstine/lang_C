/*
  This tool converts a "raveled" line-by-line array from a csv file back into
  a table.  It was written to convert the MBA grades from admissions, which come as
  one long file with one student/section per line into a table organized by one
  student with many courses.

  Input data is
         class_year, student_id, term, course_id, course_name, instructor, grade

  with each value appearing in a csv layout.

  For each chosen variable the program writes a tab-delimined output table, organized
  as students with possible courses as columns.

  11 Mar 10 ... Created from trenddata processing files.

*/

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <set>
#include <map>
#include <assert.h>

#include "read_utils.h"


void
parse_line(std::string line, std::vector<std::string> & strs)
{
  int nFields (strs.size());
  std::string::const_iterator i (line.begin());
  bool insideQuote (false);
  int s (0);
  while(s<nFields)
  { if (i == line.end())
    { if (s < nFields-1)
	std::cout << "\n\n*** Error ***   Line did not have needed fields: \n     " << line << std::endl;
      else
	strs[s] = trim(strs[s]);
      return;
    }
    if (*i == '"')
      insideQuote = !insideQuote;
    else if (*i == ',' && !insideQuote)
    { strs[s] = trim(strs[s]);
      ++s;
    }
    else
      strs[s].push_back(*i);
    ++i;
  }
}

double
convert_grade_to_number(std::string str)
{
  if (str == "A+" || str == "A") return 4.0;
  if (str == "A-")               return 3.67;
  if (str == "B+")               return 3.33;
  if (str == "B")                return 3.0;
  if (str == "B-")               return 2.67;
  if (str == "C+")               return 2.33;
  if (str == "C")                return 2.0;
  if (str == "C-")               return 1.67;
  if (str == "D+")               return 1.33;
  if (str == "D")                return 1.0;
  if (str == "D-")               return 0.67;
  return 0;
}

void
write_strings (std::vector<std::string> strs, std::ostream& os)
{
  for(int j=0; j <(int)strs.size()-1; ++j)
    os << strs[j] << "|";
  os << strs[strs.size()-1] << std::endl;
}
    


int
main()
{
  const int number_of_fields (7);
  const int    iStudent      (1);
  const int    iCourse       (3);
  const int    iGrade        (6);

  std::string inputDataFileName ("/Users/bob/data/admit/courses_2010.csv");
  std::string outputDataFileName("/Users/bob/data/admit/grades.txt");

  std::set< std::string > courses;
  std::set< std::string > students;

  std::map< std::pair<std::string, std::string>, std::string> grades;   // key on <student,course>
  
  int lineCount (0);
  std::cout << "Opening file " << inputDataFileName << " for reading..." << std::endl;
  std::ifstream dataFile (inputDataFileName.c_str());
  std::string header;
  getline(dataFile, header);                                           // dump header line
  
  while(!dataFile.eof())
  { ++lineCount;
    // if (lineCount == 1000) break;                                   // useful for testing
    if (0 == lineCount % 25000) std::cout << "Line Count @ " << lineCount << std::endl;
    std::string line;                
    getline(dataFile, line);
    if (line.empty()) break;
    if (line[0] != '2')                                                // validity check
    { std::cerr << "Leading character in line is != 2; ending.\n";
      break;
    }
    // process quoted tokens
    //    std::cout << "Parsing line: \n" << line << std::endl << std::endl;
    std::vector<std::string> strs (number_of_fields);
    parse_line(line, strs);
    //    write_strings(strs, std::cout);   // echo parsed strings

    // store value if find sought variable
    courses.insert(strs[iCourse]);
    students.insert(strs[iStudent]);
    
    grades[ std::make_pair(strs[iStudent], strs[iCourse]) ] = strs[iGrade];
  }
  
  // summarize read and field counts
  std::cout << "Completed reading file: " << inputDataFileName << ".  Read " << lineCount << " input lines.\n";
  // write output table
   std::cout << "Preparing to write data to " << outputDataFileName << std::endl;
   std::ofstream output (outputDataFileName.c_str());
   output << "Student.ID";
   for (std::set<std::string>::const_iterator j=courses.begin(); j != courses.end(); ++j)
     output << "\t" << *j;
   output << std::endl;
   for(std::set<std::string>::const_iterator i = students.begin(); i != students.end(); ++i)
   { output << *i;
     for (std::set<std::string>::const_iterator j=courses.begin(); j != courses.end(); ++j)
     { std::string grade (grades[ make_pair(*i,*j) ]);
       if (grade.empty()) output << "\tNA";
       else               output << "\t" << convert_grade_to_number(grade);
     }
     output << std::endl;
   }
}

