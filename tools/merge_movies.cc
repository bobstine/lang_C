/*
  Merges the movie ratings, adding a prefix rating to the text of the
  review and writes merged lines into a separate file suitable for
  input to the random projection code.

  Steps...  for each rater

     (a) Read the id file for this rater
     
     (b) Read the rating file for this rater
     
     (c) Read all text from scale_whole_review/reviewer/number.txt
           Do some minor processing: downcase, remove internal eol
	   
     (d) Write the rating followed by parsed text
     
*/

#include <iostream>
#include <fstream>

#include <vector>
#include <algorithm>

#include <string>

using std::string;
using std::vector;

string messageTag = "MMOV: ";



int
write_ratings_with_reviews(string path, string reviewer);

vector<int>
read_integer_vector_from_file(string filename);

string
process_line(string line);



int
main()
{
  string path = "/data/movies/";
  
  vector<string> reviewerVec;
  reviewerVec.push_back("Dennis+Schwartz");
  reviewerVec.push_back("James+Berardinelli");
  reviewerVec.push_back("Scott+Renshaw");
  reviewerVec.push_back("Steve+Rhodes");

  int numberReviews = 0;
  for (auto reviewer = reviewerVec.cbegin(); reviewer != reviewerVec.end(); ++ reviewer)
    numberReviews += write_ratings_with_reviews(path, *reviewer);
  std::clog << messageTag << "Wrote total of " << numberReviews << " to standard output with prefix ratings.\n";
  
  return 0;
}

int
write_ratings_with_reviews(string path, string reviewer)
{
  string filename;

  filename = path + "scale_data/" + reviewer + "/id." + reviewer;
  vector<int> ids     = read_integer_vector_from_file(filename);
  if (0 == ids.size()) return 0;
  filename = path + "scale_data/" + reviewer + "/rating." + reviewer;
  vector<int> ratings = read_integer_vector_from_file(filename);
  if (0 == ratings.size() || ids.size() != ratings.size()) return 0;

  for(size_t i=0; i<ids.size(); ++i)
  { std::cout << ratings[i] << "\t";
    string tag = std::to_string(ids[i]);
    string filename = path + "scale_whole_review/" + reviewer + "/" + tag + ".txt";
    std::ifstream textFile (filename);
    if(textFile)
    { string line;
      getline(textFile, line);
      std::cout << process_line(line);
    }
    else
    { std::clog << messageTag << "Could not open text file " << filename << " for reviewer " << reviewer << std::endl;
      return 0;
    }
    std::cout << std::endl;
  }
  return (int) ids.size();
}


vector<int>
read_integer_vector_from_file(string filename)
{
  vector<int> result;
  std::ifstream file (filename);
  if (file)
  { while(!file.eof())
    { int tag;
      file >> tag;
      result.push_back(tag);
    }
    std::clog << messageTag << "Read " << result.size() << " integers from file " << filename << std::endl;
    return result;
  }
  else
  { std::clog << messageTag << "Could not open file " << filename << " for reading integers.\n";
    return result;
  }
}


string
process_line(string line)
{
  std::transform(line.begin(), line.end(), line.begin(), ::tolower);
  return line;
}
