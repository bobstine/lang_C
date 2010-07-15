// $Id$ -*- c++ -*-

#ifndef INCLUDED_DEBUG
#define INCLUDED_DEBUG

#ifndef INCLUDED_STD_STRING
#define INCLUDED_STD_STRING
#include <string>
#endif

#ifndef INCLUDED_STD_FSTREAM
#define INCLUDED_STD_FSTREAM
#include <fstream>
#endif

#include <iosfwd>

// Debugging prefixes:
//
//  ---- name ---- \n    level -1
//
//  object_name0:\t      level 0
//
//  object_name1:\t      level 1 
//
//  object_name2:\t      level 2
//
//  object_name3:\t      level 3
//
//  object_name4:\t      level 4
//
// Use level -1 for error conditions.
// Use level  0 for outputting one line of major decisions
// Use level  1 for outputting one line per major event 
// use level  2 for most interesting diagonstics and most warnings
// use level  3 for huge amounts of output.  This output will be turned off in product runs.
// use level  4 for trace like output

// In production code use 0 as bound.
//        If you use the above rule, then DEBUG0, DEBUG1 are the same
//        These are the only two that I use.


// I have this at -1 either way for the time being... RAS
#ifndef NDEBUG
#define DEBUG_LOWER_BOUND -1
#else
#define DEBUG_LOWER_BOUND -1  
#endif


#if(0 < DEBUG_LOWER_BOUND)
#define DEBUG0(s)
#else
#define DEBUG0(s) if(debugging::debug_level() <= 0){s;};
#endif

#if(1 < DEBUG_LOWER_BOUND)
#define DEBUG1(s)
#else
#define DEBUG1(s) if(debugging::debug_level() <= 1){s;};
#endif

#if(2 < DEBUG_LOWER_BOUND)
#define DEBUG2(s)
#else
#define DEBUG2(s) if(debugging::debug_level() <= 2){s;};
#endif

namespace debugging
{
  class Debug
  {
  public:
    // CONSTRUCTORS
    ~Debug();
    Debug();
    Debug(std::ostream& file_to_write_on,int max_debug_level);

    // MANIPULATORS
    void set_prefix(const std::string & prefix);
    void set_max_level(int);
    std::string prefix(int level);
    void panic();
    // ACCESSORS
    std::ostream& stream(int level);
    std::ostream& stream(); // uses last level used
    int current_max_level() const;
    static Debug* get_singleton();
    
  protected:

  private:
    std::ofstream   m_dev_null;
    std::ostream&   m_ostrm;
    std::string     m_prefix;
    int             m_level;
    int             m_last_level;
    mutable int     m_panic;
    
    static debugging::Debug* sp_singleton;
    Debug(const Debug &);            // Don't delete this.
    Debug& operator=(const Debug &); // Don't delete this.
  };

  void     debug_prefix(const std::string &);

  std::string   debug_prefix(int level);              // prefix used for level of debugging
  std::ostream& debug();                              // continues with last level of debugging
  std::ostream& debug(int);                           // prefix used for appropiate level
  std::ostream& debug(const std::string&, int level); // debug("prefix",level) << "message." << std::endl;
  void     debug_init(std::ostream&,int level);       // sets stream and minimum level to print at
  int     debug_level();                              // returns the current level (i.e. minimal print level)
  void panic();                                       // called when something bad has happened.  Get low level debugging for the next 1000 rounds
  void panic(const std::string&);

  /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
  
  template<class T>
  void
  start_debugging(const T* other)  // typical usage: debug_prefix(this);
  {
    debug_prefix(other->object_name());
  };
  

  template<class T>
  std::ostream&
  debug(const T* other, int level)  // typical usage: debug(this,2) << "Message" << std::endl;
  {
    if(level <= debug_level())
      start_debugging(other);
    return debug(level);
  };
  
  template< >
  inline
  std::ostream&
  debug<char>(const char* s, int level)
  {
    return debug(std::string(s),level);
  }
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

  class Debug_lock
  {
  public:
    ~Debug_lock();
    Debug_lock(const std::string&, int level);
  private:
    int m_previous_level;
  };

  std::string no_crlf(const std::string& s);
}
#endif
