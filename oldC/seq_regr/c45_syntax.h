// $Id: c45_syntax.h,v 1.2 2004/04/26 19:52:38 foster Exp $  -*- c++ -*-

#ifndef INCLUDED_PATTERN_COMMAS
#define INCLUDED_PATTERN_COMMAS

#ifndef INCLUDED_STD_STRING
#define INCLUDED_STD_STRING
#include <string>
#endif

#ifndef INCLUDED_STL_LIST
#define INCLUDED_STL_LIST
#include <list>
#endif

#ifndef INCLUDED_STL_VECTOR
#define INCLUDED_STL_VECTOR
#include <vector>
#endif

#ifndef INCLUDED_STL_MAP
#define INCLUDED_STL_MAP
#include <map>
#endif

#include <iosfwd>


class C45_syntax
{
public:
  // CONSTRUCTORS
  virtual ~C45_syntax();
  C45_syntax(const std::string& file_name);  // i.e. "foo" if using foo.names, foo.data, foo.costs, etc
  C45_syntax(std::istream & dot_names, std::istream & dot_data);

  // MANIPULATORS
  // ACCESSORS
  void print_on(std::ostream &) const;
  void raw_data_print_on(std::ostream &) const;
  void validation(std::ostream &) const;
  void validation_names(std::ostream &) const;
  
private:
  void print_whole_variable(int col_index, std::ostream & ostrm) const;
  bool is_y(const std::string& target) const;
  void print_raw_variable(int col_index, std::ostream & ostrm) const;
  void print_missing_indicator(int col_index, std::ostream & ostrm) const;
  void print_categories(int col_index, std::ostream & ostrm) const;



  typedef std::vector<std::string> Names;
  typedef std::pair<Names,Names> xy_names;
  typedef std::pair<std::string,bool> Name_bool;
  typedef std::vector<Name_bool> Names_bool;

  typedef std::pair<double,bool> Observation;      // The bool=true if the data is missing.
  typedef std::vector<Observation> Row;
  typedef std::vector<Observation> Col;
  typedef std::vector<Row> Data_matrix;

  typedef std::map<std::string,int> Translator;
  typedef std::vector<Translator>   Translators;

  void                 add_data(std::istream &); 
  std::string          get_next_uncommented_line(std::istream& istrm) const;
  void                 parse_names(std::istream&);
  std::pair<std::string,std::string> parse_one_discrete_symbol(const std::string&) const;
  Translator           parse_discrete(const std::string&) const;
  Row                  parse_row(std::istream&,int row_number)   const;
  Observation          one_number(std::istream&,int row_number,int col_number)  const;
  Observation          one_discrete_value(const Translator&,Translator*,std::istream&,int,int) const;
private:
  // DATA

  int m_num_Xs; // number of items in each row
  Names m_Xs;  // the whole row.
  Names m_Ys;
  Names_bool m_missing;  // true if at least one missing value
  Names_bool m_continuous;  // true if at least one missing value
  Translators m_translators;// if necessary, will translate between symbols and numbers
  mutable Translators m_used;       // counts number of times symbol is used
  Data_matrix m_data;
  
  C45_syntax(const C45_syntax &);            // Don't delete this.
  C45_syntax& operator=(const C45_syntax &); // Don't delete this.
};

std::ostream& operator<<(std::ostream &,const C45_syntax &);

#endif
