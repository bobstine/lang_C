 
typedef std::map<std::string, std::vector<double> > TimeSeriesMap;   // state x time series

const int    number_of_quarters  (76);                               //   1992:1 .. 2010:4
const double missing_value (-7.777777);



int
quarter_number(std::string date)               // format is 1992.1, 1992.2, 1992.3, 1992.4, 1993.1, ...
{
  date[4] = ' ';
  int iyear   ;
  int quarter ;
  std::istringstream input(date);
  input >> iyear >> quarter;
  return 4*(iyear-1992)+quarter-1;
}

void
parse_line(std::string line, std::vector<std::string> & strs)
{
  int nFields (strs.size());
  int s (0);
  std::string::const_iterator i (line.begin());
  assert (*i == '"');
  ++i;                                                      // skip initial "
  while(s<nFields)
  { // std::cout << "top with *i = " << *i << std::endl;
    if (*i == '"')
    { ++s;
      if (s < nFields)
      { ++i;
	assert (*i==',');
	i = i+2;                                            // skip ,"
      }
    }
    else
    { //std::cout << "Pushing back " << *i << std::endl;
      strs[s].push_back(*i);
      ++i;
    }
  }
}

void
parse_line_without_quotes(std::string line, std::vector<std::string> & strs)
{
  int nFields (strs.size());
  int s (0);
  std::string::const_iterator i (line.begin());
  // std::cout << "Parsing file with " << nFields << " fields and no quotes.\n";
  while(i != line.end())
  {
    // std::cout << "Processing char [" << *i << "]\n";
    if (*i == ',')
    { ++s;
      if (s < nFields)
	++i;
    }
    else
    { strs[s].push_back(*i);
      ++i;
    }
  }
}


void
insert_value(TimeSeriesMap &m, std::string region, int quarter, std::string value)
{
  if (m.find(region) == m.end()) // insert empty vector
  { 
    m[region] = std::vector<double>(number_of_quarters, missing_value);
  }
  m[region][quarter] = read_utils::lexical_cast<double>(value.c_str());
}
