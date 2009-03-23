// $Id$-*- c++ -*-


#include "debug.h"

// put other includes here
#include "assert.h"
#include <iostream>
#include <sstream>
#include <fstream>

debugging::Debug* debugging::Debug::sp_singleton = 0;


////////////////////////////////////////////////////////////////////////////////////////////
//                          U S I N G   D I R E C T I V E S                            using
////////////////////////////////////////////////////////////////////////////////////////////
//                              C O N S T R U C T O R S                         constructors

debugging::Debug::~Debug()
{
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
debugging::Debug::Debug()
:
  m_dev_null("/dev/null"),
  m_ostrm(m_dev_null),
  m_prefix("none set"),
  m_level(0),
  m_last_level(0),
  m_panic(0)
{
  if(sp_singleton)
    delete sp_singleton;
  sp_singleton = this;
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
debugging::Debug::Debug(std::ostream& strm,int level):
  m_dev_null("/dev/null"),
  m_ostrm(strm),
  m_prefix("none set"),
  m_level(level),
  m_last_level(0),
  m_panic(0)
{
  assert(level <= 4); // no output
  assert(level >= -1); // mucho output
  if(sp_singleton)
    delete sp_singleton;
  sp_singleton = this;
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
////////////////////////////////////////////////////////////////////////////////////////////
//                             M A N I P U L A T O R S                          manipulators
void
debugging::Debug::set_prefix(const std::string & prefix)
{
  m_prefix = prefix;
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
debugging::Debug::set_minimum_level(int min)
{
  m_level = min;
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
std::string
debugging::Debug::prefix(int level) 
{
  m_last_level = level;
  if(level < 0)
    return "--------------   "+m_prefix+"  ------------------\n";
  else if(level == 0)
    return ")"+m_prefix+":\t";
  else if(level == 1)
    return "!"+m_prefix+":\t";
  else if(level == 2)  // most common level
    return "@"+m_prefix+":\t";
  else if(level == 3)  // most common level
    return "#"+m_prefix+":\t";
  else if(level == 4)
    return "$"+m_prefix+":\t";
  else
    return "%"+m_prefix+":\t";
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
debugging::Debug::panic()
{
  if(m_panic == 0)
    debugging::debug("PANIC",5) << " Panic started." << std::endl;
  else
    debugging::debug("PANIC",3) << " Panic continuing. (" << m_panic << ")" << std::endl;

  m_panic = 5000;
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
////////////////////////////////////////////////////////////////////////////////////////////
//                               A C C E S S O R S                                 accessors
std::ostream&
debugging::Debug::stream(int level)
{
  m_last_level = level;
  return stream();
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
std::ostream&
debugging::Debug::stream()
{
  if(m_last_level >= current_minimum_level())
    return m_ostrm;
  else
    return m_dev_null;
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
debugging::Debug::current_minimum_level() const
{
  if(m_panic > 0)
    {
      --m_panic;
      if(m_panic == 0)
	  debugging::debug("PANIC",2) << " Panic over." << std::endl;
      return(m_level - 2);
    }
  return m_level;
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
////////////////////////////////////////////////////////////////////////////////////////////
//                           P R O T E C T E D                                     protected
////////////////////////////////////////////////////////////////////////////////////////////
//                           P R I V A T E                                           private
////////////////////////////////////////////////////////////////////////////////////////////
//                           S T A T I C E                                            static
debugging::Debug*
debugging::Debug::get_singleton()
{
  if(sp_singleton == 0)
    sp_singleton = new Debug();
  return sp_singleton;
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

////////////////////////////////////////////////////////////////////////////////////////////
//                     F R E E   F U N C T I O N S                            free functions
void
debugging::debug_prefix(const std::string& prefix)
{
  Debug::get_singleton()->set_prefix(prefix);
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
std::string
debugging::debug_prefix(int level)
{
  return Debug::get_singleton()->prefix(level);
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
std::ostream&
debugging::debug(int level)
{
  std::ostream& result = Debug::get_singleton()->stream(level);
  result <<  debug_prefix(level);
  return result;
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
std::ostream&
debugging::debug()
{
  return Debug::get_singleton()->stream(); // uses last level
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
debugging::debug_init(std::ostream & ostrm, int level)
{
  if(level < DEBUG_LOWER_BOUND)
    {
      new debugging::Debug(ostrm,DEBUG_LOWER_BOUND);
      debug("ALERT",4) << "Can't set debug level to " << level
		       << " since " << DEBUG_LOWER_BOUND << " was compiled in as a lower bound." << std::endl;
    }
  else
    new debugging::Debug(ostrm,level);
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
debugging::debug_level()
{
  if(Debug::get_singleton())
    return Debug::get_singleton()->current_minimum_level();
  else
    return 10;  // no debugging is being printed
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
debugging::panic()
{
  if(Debug::get_singleton())
    return Debug::get_singleton()->panic();
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
void
debugging::panic(const std::string& s)
{
  panic();
  debugging::debug("PANIC",5) << s << std::endl;
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
std::ostream&
debugging::debug(const std::string& prefix, int level)
{
  if(level >= debug_level())
    debug_prefix(prefix);
  return debug(level);
};
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
debugging::Debug_lock::~Debug_lock()
{
  debugging::Debug::get_singleton()->set_minimum_level(m_previous_level);
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
debugging::Debug_lock::Debug_lock(const std::string& prefix, int level):
  m_previous_level(Debug::get_singleton()->current_minimum_level())
{
  if(level > m_previous_level)
    Debug::get_singleton()->set_minimum_level(level);
  debug(prefix,level);
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
std::string
debugging::no_crlf(const std::string& input)
{
  std::string result;
  std::stringstream s(input);
  while(!s.eof())
    {
      std::string line;
      getline(s,line);
      if(result == "")
	result = line;
      else
	result += " <CR-LF> " + line;
	
      s >> std::ws;
    }
  return result;
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
