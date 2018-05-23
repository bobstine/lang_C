/*
  This filter reads records from standard input and cuts out the
  indicated columns. The output columns are separated by a tab
  character.

  The only difference from the built-in cut is the separation of
  output fields.
*/

#include <iostream>
#include <vector>

int
main()
{
  // define input ranges using   1   based indexing
  std::vector< std::pair<size_t,size_t> > ranges;
  ranges.push_back(std::make_pair( 28, 31));   
  ranges.push_back(std::make_pair(214,215));
  ranges.push_back(std::make_pair(216,217));
  ranges.push_back(std::make_pair(218,219));
  ranges.push_back(std::make_pair(220,221));
  ranges.push_back(std::make_pair(222,223));
  ranges.push_back(std::make_pair(251,258));
  ranges.push_back(std::make_pair(259,266));
  size_t numberOfRanges (ranges.size());

  // convert range positions to 0 based, find lengths
  std::vector<size_t> len (numberOfRanges);
  for (size_t i=0; i<numberOfRanges; ++i)
  { --ranges[i].first;
    --ranges[i].second;
    len[i] = ranges[i].second - ranges[i].first + 1;
  }
  
  // read and write to standard output
  size_t lineCt (0);
  size_t k      (numberOfRanges-1);
  while (std::cin)
  { ++lineCt;
    std::string inputLine;
    getline(std::cin, inputLine);
    for (size_t i=0; i<numberOfRanges-1; ++i)
    {
      if (inputLine.length() > ranges[i].second)
        std::cout << inputLine.substr((size_t)ranges[i].first, (size_t)len[i]) << '\t';
    }
    if (inputLine.length() >= ranges[k].second)
      std::cout << inputLine.substr(ranges[k].first, len[k]) << std::endl;
  }
  std::clog << "CUTT: Wrote " << lineCt << " lines to std out.\n";
  return 0;
}
      
  
