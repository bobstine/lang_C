/*
  Merges the movie ratings, adding a prefix rating to the text of the
  review and writes merged lines into a separate file suitable for
  input to the random projection code.  The merged text goes to the
  std output.  Two other files are also created.

  Steps...  for each rater

     (a) Read the id file for this rater
     
     (b) Read the rating file for this rater
     
     (c) Read all text from scale_whole_review/reviewer/number.txt
           Do some minor processing: downcase, remove internal eol
	   
     (d) Write the rater, the rating and then the processed text
     
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

template <class C>
int
fill_vector_from_file(string filename, vector<C> *pVec);

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
  vector<int> ids;
  fill_vector_from_file(filename, &ids);
  if (0 == ids.size()) return 0;
  filename = path + "scale_data/" + reviewer + "/rating." + reviewer;
  vector<float> ratings;
  fill_vector_from_file(filename, &ratings);
  if (0 == ratings.size() || ids.size() != ratings.size()) return 0;

  for(size_t i=0; i<ids.size(); ++i)
  { if(0 == ratings[i])
      std::clog << messageTag << "Drop zero-rated movie #" << ids[i] << " for reviewer " << reviewer << std::endl;
    else
    { std::cout << reviewer << "\t" << ratings[i] << "\t";              // put reviewer then rating at head of output line
      string tag = std::to_string(ids[i]);
      string filename = path + "scale_whole_review/" + reviewer + "/" + tag + ".txt";
      std::ifstream textFile (filename);
      if(textFile)
      { while(!textFile.eof())                                          // suck down whole file, write to one line
	{ string line;
	  getline(textFile, line);
	  std::cout << process_line(line);                              // add text after rating
	}
      }
      else
      { std::clog << messageTag << "Could not open text file " << filename << " for reviewer " << reviewer << std::endl;
	return 0;
      }
      std::cout << std::endl;
    }
  }
  return (int) ids.size();
}

template <class C>
int
fill_vector_from_file(string filename, vector<C> *pVec)
{
  std::ifstream file (filename);
  if (file)
  { while(!file.eof())
    { C tag;
      file >> tag;
      pVec->push_back(tag);
    }
    std::clog << messageTag << "Read " << pVec->size() << " items from file " << filename << std::endl;
    return (int) pVec->size();
  }
  else
  { std::clog << messageTag << "Could not open file " << filename << " for reading integers.\n";
    return 0;
  }
}


string
process_line(string line)
{
  std::transform(line.begin(), line.end(), line.begin(), ::tolower);
  return line;
}
