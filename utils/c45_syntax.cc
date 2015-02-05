// $Id: c45_syntax.cc,v 1.13 2004/09/16 15:31:34 foster Exp $-*- c++ -*-

#include "c45_syntax.h"

#include <math.h>   // for sqrt

#include <sstream>
#include <assert.h>
#include <iostream>
#include <iterator>
#include <fstream>
#include <algorithm>

////////////////////////////////////////////////////////////////////////////////////////////
//                              C O N S T R U C T O R S                         constructors

C45_syntax::~C45_syntax()
{
};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
C45_syntax::C45_syntax(const std::string& file_name)
  :
  m_num_Xs(0),
  m_Xs(),
  m_Ys(),
  m_missing(),
  m_data()

{
  std::ifstream names((file_name+".names").c_str());  
  parse_names(names);
  std::ifstream data((file_name+".data").c_str());  
  add_data(data);
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
C45_syntax::C45_syntax(std::istream& data, std::istream& ys)
    :
  m_num_Xs(0),
  m_Xs(),
  m_Ys(),
  m_missing(),
  m_data()
{
  parse_names(ys);
  add_data(data);
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

////////////////////////////////////////////////////////////////////////////////////////////
//                             M A N I P U L A T O R S                          manipulators
void
C45_syntax::add_data(std::istream & data)
{
  data >> std::ws;
  int row_index = 1;
  while(!data.eof())
    {
      Row r = parse_row(data,row_index);
      for(int i = 0; i < m_num_Xs; ++i)
	{
	  if(r[i].second)
	    m_missing[i].second = true;
	  m_data[i].push_back(r[i]);
	}
      ++row_index;
      data >> std::ws;
    };
};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
////////////////////////////////////////////////////////////////////////////////////////////
//                               A C C E S S O R S                                 accessors
void
C45_syntax::print_on(std::ostream & ostrm) const
{
  ostrm << "All " << m_num_Xs << " variables:\n" << std::endl;
  for(int i = 0; i < m_num_Xs; ++i)
    {
      std::string variable_name = m_Xs[i];
      if(std::find(m_Ys.begin(),m_Ys.end(),variable_name) != m_Ys.end())
	ostrm << "      *";
      ostrm << "\t" << variable_name;
      if(m_missing[i].second)
	ostrm << " (which contains some missing values)";
      if(!m_continuous[i].second)
	{
	  std::cout << "\n\n";
	  Translator t = m_translators[i];
	  Translator used = m_used[i];
	  for(Translator::const_iterator i = t.begin(); i != t.end(); ++i)
	    {
	      const std::string& name = i->first;
	      if(used[name] == 0)
		std::cout << "\t\t(" << i->first << ")--> " << '"' << i->second << '"'
			  << "\t\t # = 0"  << std::endl;
	      else
		std::cout << "\t\t " << i->first << " --> " << '"' << i->second << '"'
			  << "\t\t # = " << used[name] << std::endl;
	    }
	}
      ostrm << std::endl;
    };
  ostrm << "\nThe Y variables (which are star'ed above) are: ";
  for(Names::const_iterator i = m_Ys.begin(); i != m_Ys.end(); ++i)
    {
      std::string name = (*i);
      ostrm << " " << name ;
    };
  ostrm << "\n\n";
  for(Data_matrix::const_iterator col = m_data.begin();col != m_data.end(); ++col)
    {
      int col_index = (int) (col - m_data.begin());
      std::string col_name = m_Xs[col_index];
      ostrm << col_name << " = ";
      int num_printed = 0;
      int n = (int) col->size();
      int num_missing = 0;
      for(int i = 0; i < n; ++i)
	{
	  num_missing += (*col)[i].second;
	  if((i < 10) || (i >= n - 3))
	    if((*col)[i].second)
	      {
		ostrm << " ?";
	      }
	    else
	      {
		if(m_continuous[col_index].second)
		  ostrm << " " << (*col)[i].first;
		else
		  ostrm << " " << '"' << (*col)[i].first << '"';
	      }
	  else if((num_printed == 10) && n > 14)
	    ostrm << " ... ";
	  ++num_printed;
	}
      if(num_missing > 0)
	ostrm << "\t\t (" << col_name << " is missing " << num_missing << " / " << n << ")";

      ostrm << std::endl;
    }
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
namespace
{
  std::string
  sanitary(int i, int max)
  {
    if(max < 26)
      {
	std::string result;
	result += (char)('A' + i - 1);
	return result;
      }
    std::stringstream s;
    s << "V";
    s << i;
    return s.str();
  }
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
C45_syntax::print_sanitary_names(std::ostream & ostrm) const
{
  ostrm << "| Names of " << m_num_Xs << " variables:\n" << std::endl;
  copy(m_Ys.begin(),m_Ys.end(),std::ostream_iterator<std::string>(ostrm,".\n"));

  for(int i = 0; i < m_num_Xs; ++i)
    {
      std::string variable_name = m_Xs[i];
      if(std::find(m_Ys.begin(),m_Ys.end(),variable_name) == m_Ys.end())
	{
	  std::stringstream s;
	  s << i;
	  variable_name = "X" + s.str();
	}
      ostrm << variable_name;
      if(m_continuous[i].second)
	{
	  ostrm << ": continuous." << std::endl;
	}
      else
	{
	  ostrm << ": ";
	  Translator t = m_translators[i];
	  Translator used = m_used[i];
	  bool continuation = false;
	  for(Translator::const_iterator i = t.begin(); i != t.end(); ++i)
	    {
	      if(continuation)
		ostrm << ", ";
	      continuation = true;
	      //	      const std::string& name = i->first;
	      ostrm << sanitary(i->second,(int)t.size());
	    }
	  ostrm << "." << std::endl;
	}
    };
  ostrm << std::endl;
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
C45_syntax::print_variable_names(std::ostream & ostrm) const
{
  ostrm << "| Names of " << m_num_Xs << " variables:\n" << std::endl;
  copy(m_Ys.begin(),m_Ys.end(),std::ostream_iterator<std::string>(ostrm,".\n"));

  for(int i = 0; i < m_num_Xs; ++i)
    {
      std::string variable_name = m_Xs[i];
      ostrm << variable_name;
      if(m_continuous[i].second)
	{
	  ostrm << ": continuous." << std::endl;
	}
      else
	{
	  ostrm << ": ";
	  Translator t = m_translators[i];
	  Translator used = m_used[i];
	  bool continuation = false;
	  for(Translator::const_iterator i = t.begin(); i != t.end(); ++i)
	    {
	      if(continuation)
		ostrm << ", ";
	      continuation = true;
	      //	      const std::string& name = i->first;
	      ostrm << sanitary(i->second,(int)t.size());
	    }
	  ostrm << "." << std::endl;
	}
    };
  ostrm << std::endl;
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
C45_syntax::print_sanitary_data(std::ostream & ostrm) const
{
  int n = (int)m_data.begin()->size();  // we assume all have same size and at least one exists
  std::pair<std::vector<double>,std::vector<double> > ms = mean_sd();
  const std::vector<double>& mean(ms.first);
  const std::vector<double>& sd(ms.second);
  for(int i = 0; i < n; ++i)
    {
      bool continuation = false;
      for(Data_matrix::const_iterator col = m_data.begin();col != m_data.end(); ++col)
	{
	  if(continuation)
	    ostrm << ", ";
	  continuation = true;

	  int col_index = (int)(col - m_data.begin());
	  if(!(*col)[i].second)  // OK, it isn't missing
	    {
	      if(m_continuous[col_index].second)
		ostrm <<  ((*col)[i].first - mean[col_index])/sd[col_index];
	      else
		ostrm << sanitary(int((*col)[i].first),(int) m_translators[col_index].size());
	    }
	}
      ostrm << std::endl;
    }
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
C45_syntax::print_raw_variable(int col_index, std::ostream & ostrm) const
{
  int n = (int) m_data[col_index].size();
  ostrm << m_Xs[col_index] << std::endl;
  for(int i = 0; i < n; ++i)
    {
      ostrm << m_data[col_index][i].first << " ";
    }
  ostrm << std::endl;
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
C45_syntax::print_missing_indicator(int col_index, std::ostream & ostrm) const
{
  int n = (int) m_data[col_index].size();
  ostrm << m_Xs[col_index] << " is missing" << std::endl;
  for(int i = 0; i < n; ++i)
    {
      ostrm << m_data[col_index][i].second << " ";
    }
  ostrm << std::endl;
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
C45_syntax::print_categories(int col_index, std::ostream & ostrm) const
{
  int n = (int)m_data[col_index].size();
  assert(!m_continuous[col_index].second);
  const Translator& t = m_translators[col_index];
  int num_categories = (int)t.size();
  Translator::const_iterator iter = t.begin();

  if(num_categories == 2)
    ++iter;
  // Logic is as follows:
  //   If there are more than 2 categories, we need each as an indicator
  //   If there is only one (or zero?) category--it is a stupid variable
  //   If there are two categories--we don't need both since 1- will convert

  for(; iter != t.end(); ++iter)
    {
      if(m_used[col_index][iter->first] > 0)
	{
	  int category = iter->second;
	  ostrm << m_Xs[col_index] << " = " << iter->first << std::endl;
	  for(int i = 0; i < n; ++i)
	    {
	      if(m_data[col_index][i].first == category)
		ostrm <<  "1 ";
	      else
		ostrm <<  "0 ";
	    }
	  ostrm << std::endl;
	}
    }
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
C45_syntax::print_whole_variable(int col_index, std::ostream & ostrm) const
{
  if(m_missing[col_index].second)
    {
      print_missing_indicator(col_index,ostrm);
    }
  if(m_continuous[col_index].second)
    {
      print_raw_variable(col_index,ostrm);
    }
  else
    {
      print_categories(col_index,ostrm);
    }
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
bool
C45_syntax::is_y(const std::string& target) const
{
  bool result = false;
  for(Names::const_iterator i = m_Ys.begin(); i != m_Ys.end(); ++i)
    {
      std::string name = (*i);
      if(target == name)
	{
	  assert(!result);
	  result = true;
	}
    };
  return result;
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void
C45_syntax::raw_data_print_on(std::ostream & ostrm) const
{
  ostrm << m_data.begin()->size() << std::endl;  // first print number of columns

  bool y_printed = false;
  for(Data_matrix::const_iterator col = m_data.begin();col != m_data.end(); ++col)
    {
      int col_index = (int)(col - m_data.begin());
      std::string col_name = m_Xs[col_index];
      if(is_y(col_name))
	{
	  assert(!y_printed);  // make sure we only have one y to print
	  y_printed = true;
	  print_raw_variable(col_index,ostrm);
	}
    }
  assert(y_printed);  // make sure we printed at least one y

  for(Data_matrix::const_iterator col = m_data.begin();col != m_data.end(); ++col)
    {
      int col_index = (int)(col - m_data.begin());
      std::string col_name = m_Xs[col_index];
      if(!is_y(col_name))
	print_whole_variable(col_index,ostrm);
    }
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

std::pair<std::vector<double>,std::vector<double> >
C45_syntax::mean_sd() const
{
  std::vector<double> mean(m_data.size(),0.0);
  std::vector<double> SD(m_data.size(),0.0);

  for(Data_matrix::const_iterator col = m_data.begin();col != m_data.end(); ++col)
    {
      int col_index = (int)(col - m_data.begin());
      if(m_continuous[col_index].second)
	{
	  double t = 0;
	  double ss = 0;
	  int c = 0;
	  for(Row::const_iterator obs = col->begin(); obs != col->end(); ++obs)
	    {
	      if(!obs->second)  // OK, it isn't missing
		{
		  double value = obs->first;
		  t += value;
		  ss += value * value;
		  c += 1;
		}
	    }
	  if(c > 0)
	    {
	      double m = t / c;
	      mean[col_index] = m; 
	      SD[col_index] = sqrt(ss / c - m * m);
	    }
	}
    }
  return make_pair(mean,SD);
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
C45_syntax::validation(std::ostream & ostrm) const
{
  std::vector<std::vector<Observation>::const_iterator> transpose;
  for(Data_matrix::const_iterator col = m_data.begin();col != m_data.end(); ++col)
    {
      transpose.push_back(col->begin());
    }

  for(unsigned int row_index = 0; row_index < m_data.begin()->size();++row_index)
    {
      for(unsigned int col_index = 0; col_index < m_data.size(); ++col_index)
	{
	  if(m_continuous[col_index].second)
	    {
	      ostrm << transpose[col_index]->first << " ";
	    }
	  else
	    {// print out a categorical variable
	      const Translator& t = m_translators[col_index];
	      for(Translator::const_iterator iter = t.begin(); iter != t.end(); ++iter)
		{
		  int category = iter->second;
		  if(transpose[col_index]->first == category)
		    ostrm <<  "1 ";
		  else
		    ostrm <<  "0 ";
		}
	    }
	  ostrm << transpose[col_index]->second << " ";  // always print missing
	  ++transpose[col_index];
	}
      ostrm << "\n";
    }
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
C45_syntax::validation_names(std::ostream & ostrm) const
{
  for(unsigned int col_index = 0; col_index < m_data.size(); ++col_index)
    {
      if(m_continuous[col_index].second)
	{
	  ostrm << m_Xs[col_index] << std::endl;
	}
      else
	{// print out a categorical variable
	  const Translator& t = m_translators[col_index];
	  for(Translator::const_iterator iter = t.begin(); iter != t.end(); ++iter)
	    {
	      ostrm << m_Xs[col_index] << " = " << iter->first << std::endl;
	    }
	}
      ostrm << m_Xs[col_index] << " is missing" << std::endl;
    }
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

////////////////////////////////////////////////////////////////////////////////////////////
//                           P R O T E C T E D                                     protected

////////////////////////////////////////////////////////////////////////////////////////////
//                           P R I V A T E                                           private

std::string
C45_syntax::get_next_uncommented_line(std::istream& istrm) const
{
  std::string try_line;
  do
    getline(istrm,try_line);
  while(((try_line.length() <= 1) || (try_line[0] == '|')) && !istrm.eof());  // lines starting with "|" are comments
  if(try_line[0] == '|')
    {
      try_line = ""; // empty return
    }
  assert((try_line.length() > 1) || istrm.eof());
  return try_line;
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */



void
C45_syntax::parse_names(std::istream& istrm) 
{
  // this is line oriented
  using std::string;
  
  istrm >> std::ws;
  while(!istrm.eof())
    {
      string one_line = get_next_uncommented_line(istrm);
      istrm >> std::ws;
      string::size_type where_found = one_line.find(":");
      if(where_found != string::npos)
	{
	  // looks like we an X, something like:  "variableName: continuous."
	  string n(one_line.substr(0,where_found));
	  m_Xs.push_back(n);
	  m_missing.push_back(make_pair(n,false));
	  m_data.push_back(Col());
	  if(one_line.find("continuous.") == string::npos)
	    { // Yikes!  Its discrete so we have to deal with names.
	      m_continuous.push_back(make_pair(n,false));
	      Translator t = parse_discrete(one_line);
	      m_translators.push_back(t);
	      m_used.push_back(Translator());
	    }
	  else
	    {
	      m_continuous.push_back(make_pair(n,true));
	      m_translators.push_back(Translator());  // create a slot for the translator to live
	      m_used.push_back(Translator());
	    }
			  
	}
      else
	{
	  // looks like we have a Y:  "variableName."
	  where_found = one_line.find(".");
	  assert(where_found != string::npos);  // lets make sure it actually makes sense
	  string n(one_line.substr(0,where_found));
	  m_Ys.push_back(n);
	}
    }
  m_num_Xs = (int) m_Xs.size();
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
C45_syntax::Row
C45_syntax::parse_row(std::istream& istrm,int row_number) const
{
  Row result;
  std::string line;
  do
    getline(istrm,line);
  while((line[0] == '#') || (line[0] == '|'));
  std::string comma_first = line + "," ; // make it a simplier BNF: 34,14,10 --->   34,14,10, 
  std::stringstream s(comma_first);
  for(int col_index = 0; col_index < m_num_Xs;++col_index)
    {
      if(m_continuous[col_index].second)
	result.push_back(one_number(s,row_number,col_index));
      else
	result.push_back(one_discrete_value(m_translators[col_index],
					    &(m_used[col_index]),s,row_number,col_index));
      s >> std::ws;
    };
  return result;
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
std::pair<std::string,std::string>
C45_syntax::parse_one_discrete_symbol(const std::string& s) const
{
  std::string up_to_comma = s.substr(0,s.find(","));
  std::stringstream remove_white_space(up_to_comma);
  std::string result;
  remove_white_space >> result;
  return make_pair(result,s.substr(s.find(",")+1,std::string::npos));
}


C45_syntax::Translator
C45_syntax::parse_discrete(const std::string& s) const
{
  Translator result;

  std::string::size_type period = s.find(".");
  assert(period != std::string::npos);
  std::string::size_type colon = s.find(":");
  assert(colon != std::string::npos);
  std::string colon_to_period(s.substr(colon+1,period-colon-1));
  colon_to_period += ",";
  int current_value = 1;
  while(colon_to_period.size() > 0)
    {
      std::pair<std::string, std::string> symbol_rest = parse_one_discrete_symbol(colon_to_period);
      std::string symbol = symbol_rest.first;
      colon_to_period = symbol_rest.second;
      result[symbol] = current_value;
      ++current_value;
    }
  return result;
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

C45_syntax::Observation
C45_syntax::one_number(std::istream& istrm,int row_number, int col_number) const
{
  Observation result(0,true);  // set up for a missing value

  char empty;
  istrm >> std::ws;
  assert(!istrm.eof());
  istrm >> empty;
  istrm.putback(empty);

  if((empty != ',') && (empty != '?'))
    {
      // Ah--it isn't missing, but is it a number?
      if(!(((empty >= '0') && (empty <= '9')) || (empty == '-')))
	{
	  std::string value;
	  char next;
	  istrm >> next;
	  while(next != ',')
	    {
	      value += next;
	      istrm >> next;
	    }
	  std::cerr << "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * " << std::endl;
	  std::cerr << "Data error: row " << row_number
		    << " col " << col_number+1 << ": Variable " << m_Xs[col_number] 
		    << " contains " << '"' << value << '"'
		    << " which doesn't appear to be a number." << std::endl;
	  std::cerr << "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n " << std::endl;
	}
      else
	{
	
	  istrm >> result.first;
	  result.second = false;
	  char comma;
	  istrm >> std::ws >> comma;
	  assert(comma == ',');
	}
    }
  else
    {
      // Oops, missing value.  We need to read to the next , and kill everyting up to that point
      while(istrm.get() != ',');
    }
  return result;
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

C45_syntax::Observation
C45_syntax::one_discrete_value(const Translator& t,
			       Translator* p_used,
			       std::istream& istrm,
			       int row_number,int col_number) const
{
  Observation result(0,true);  // set up for a missing value

  char empty;
  istrm >> std::ws;
  assert(!istrm.eof());
  istrm >> empty;
  istrm.putback(empty);

  if((empty != ',') && (empty != '?'))
    {
      std::string value;
      char next;
      istrm >> next;
      while(next != ',')
	{
	  value += next;
	  istrm >> next;
	}
      std::stringstream remove_white_space(value);
      remove_white_space >> value;
      if(t.find(value) == t.end())
	{
	  std::cerr << "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * " << std::endl;
	  std::cerr << "Data error: row " << row_number
		    << " col " << col_number+1 << ": Variable " << m_Xs[col_number] 
		    << " contains " << '"' << value << '"'
		    << " which isn't an allowed value.\nTreated as missing." << std::endl;
	  for(Translator::const_iterator i = t.begin(); i != t.end(); ++i)
	    {
	      std::cerr << "\t" << value << " != " << i->first  << std::endl;
	    }
	  std::cerr << "If it looks like a coding error--fix it.  Otherwise add it to the names file." << std::endl;
	  std::cerr << "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\n " << std::endl;
	}
      else
	{
	  result.first = t.find(value)->second;
	  (*p_used)[value] += 1;
	  result.second = false;
	}
      istrm >> std::ws;
    }
  else
    {
      // Oops, missing value.  We need to read to the next , and kill everyting up to that point
      while(istrm.get() != ',');
    }
  return result;
};
////////////////////////////////////////////////////////////////////////////////////////////
//                     F R E E   F U N C T I O N S                            free functions

std::ostream & operator<<(std::ostream & ostrm, const C45_syntax & object)
{
  object.print_on(ostrm);
  return ostrm;
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

