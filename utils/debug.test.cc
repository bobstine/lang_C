// -*- c++ -*-


#include <iostream>
#include <assert.h>

#include "debug.h"


class foo
{
public:
  std::string object_name() const
  {
    return "bar";
  };
  void print_debug()
  {
    using namespace debugging;
    debug(2) << "starting in object." << std::endl;
    start_debugging(this);
    debug(2) << "continuing in object." << std::endl;
    debug(this,3) << "Important message from object." << std::endl;
  }
};

int
main()
{
  std::cout << "\n\n\n\t\t\t DEBUG  DEBUG  DEBUG\n\n\n"<<std::endl;
  {
    using namespace debugging;
    // until it is set, all output goes to the bit bucket
    debug() << "testing one two three" << std::endl;
    debug_prefix("prefix");
    debug( ) << "empty level (aka 0)" << std::endl;
    debug(0) << "level 0." << std::endl;
    debug(1) << "level 1." << std::endl;
    debug(2) << "level 2." << std::endl;
    debug(3) << "level 3." << std::endl;
  };

  {
    using namespace debugging;
    std::cout << "---------------------" << std::endl;
    debug_init(std::cout,5);  // max level of debugging
    debug() << "testing five" << std::endl;
    debug_prefix("foo");
    debug( ) << "empty level" << std::endl;
    debug(-1)<< "level -1 error condition." << std::endl;   // set off by line rather than nameLevel: 
    debug(-1)<< "level -1 error.";
    debug()  << "continued" << std::endl;
    debug(0) << "level 0." << std::endl;
    debug(0) << "level 0.";
    debug()  << "continued" << std::endl;
    debug(1) << "level 1." << std::endl;
    debug(1) << "level 1.";
    debug()  << "continued" << std::endl;
    debug(2) << "level 2." << std::endl;
    debug(2) << "level 2.";
    debug()  << "continued" << std::endl;
    debug(3) << "level 3." << std::endl;
    debug(3) << "level 3.";
    debug()  << "continued" << std::endl;
    debug(4) << "level 4." << std::endl;
    debug(4) << "level 4.";
    debug()  << "continued" << std::endl;
    debug(5) << "level 5." << std::endl;
    debug(5) << "level 5.";
    debug()  << "continued" << std::endl;
  };

  {
    using namespace debugging;
    std::cout << "---------------------" << std::endl;
    debug_init(std::cout,4);  // max level of debugging
    debug() << "testing four" << std::endl;
    debug_prefix("foo");
    debug( ) << "empty level" << std::endl;
    debug(-1)<< "level -1 error condition." << std::endl;   // set off by line rather than nameLevel: 
    debug(-1)<< "level -1 error.";
    debug()  << "continued" << std::endl;
    debug(0) << "level 0." << std::endl;
    debug(0) << "level 0.";
    debug()  << "continued" << std::endl;
    debug(1) << "level 1." << std::endl;
    debug(1) << "level 1.";
    debug()  << "continued" << std::endl;
    debug(2) << "level 2." << std::endl;
    debug(2) << "level 2.";
    debug()  << "continued" << std::endl;
    debug(3) << "level 3." << std::endl;
    debug(3) << "level 3.";
    debug()  << "continued" << std::endl;
    debug(4) << "level 4." << std::endl;
    debug(4) << "level 4.";
    debug()  << "continued" << std::endl;
    debug(6) << "level 6." << std::endl;
    debug(6) << "level 6.";
    debug()  << "continued" << std::endl;
  };
  {
    using namespace debugging;
    std::cout << "---------------------" << std::endl;
    debug_init(std::cout,3);  // max typical level of debugging
    debug() << "testing three" << std::endl;
    debug_prefix("foo");
    debug( ) << "empty level (aka 0)" << std::endl;
    debug(0) << "level 0." << std::endl;
    debug(0) << "level 0.";
    debug()  << "continued" << std::endl;
    debug(1) << "level 1." << std::endl;
    debug(1) << "level 1.";
    debug()  << "continued" << std::endl;
    debug(2) << "level 2." << std::endl;
    debug(2) << "level 2.";
    debug()  << "continued" << std::endl;
    debug(3) << "level 3." << std::endl;
    debug(3) << "level 3.";
    debug()  << "continued" << std::endl;
    debug(4) << "level 4." << std::endl;
    debug(4) << "level 4.";
    debug()  << "continued" << std::endl;
  };
  {
    using namespace debugging;
    std::cout << "---------------------" << std::endl;
    debug_init(std::cout,2);  // less output
    debug() << "testing two" << std::endl;
    debug_prefix("foo");
    debug( ) << "empty level (aka 0)" << std::endl;
    debug(0) << "level 0." << std::endl;
    debug(0) << "level 0.";
    debug()  << "continued" << std::endl;
    debug(1) << "level 1." << std::endl;
    debug(1) << "level 1.";
    debug()  << "continued" << std::endl;
    debug(2) << "level 2." << std::endl;
    debug(2) << "level 2.";
    debug()  << "continued" << std::endl;
    debug(3) << "level 3." << std::endl;
    debug(3) << "level 3.";
    debug()  << "continued" << std::endl;
    debug(4) << "level 4." << std::endl;
    debug(4) << "level 4.";
    debug()  << "continued" << std::endl;
  };
  {
    using namespace debugging;
    std::cout << "---------------------" << std::endl;
    debug_init(std::cout,1);   // less still
    debug() << "testing one" << std::endl;
    debug_prefix("foo");
    debug( ) << "empty level (aka 0)" << std::endl;
    debug(0) << "level 0." << std::endl;
    debug(0) << "level 0.";
    debug()  << "continued" << std::endl;
    debug(1) << "level 1." << std::endl;
    debug(1) << "level 1.";
    debug()  << "continued" << std::endl;
    debug(2) << "level 2." << std::endl;
    debug(2) << "level 2.";
    debug()  << "continued" << std::endl;
    debug(3) << "level 3." << std::endl;
    debug(3) << "level 3.";
    debug()  << "continued" << std::endl;
    debug(4) << "level 4." << std::endl;
    debug(4) << "level 4.";
    debug()  << "continued" << std::endl;
  };
    using namespace debugging;
    std::cout << "---------------------" << std::endl;
    debug_init(std::cout,0);  // only error codes, basic messages
    {
      Debug_lock start("locked",0);
      debug_prefix("foo");
      debug("bar",0) << "level 0." << std::endl;
      debug(0) << "level 0.";
      debug()  << "continued" << std::endl;
      debug(new foo(),1) << "level 1." << std::endl;
      debug(1) << "level 1.";
      debug()  << "continued" << std::endl;
      debug(2) << "level 2." << std::endl;
      debug(2) << "level 2.";
      debug()  << "continued" << std::endl;
      debug(3) << "level 3." << std::endl;
      debug(3) << "level 3.";
      debug()  << "continued" << std::endl;
      debug(4) << "level 4." << std::endl;
      debug(4) << "level 4.";
      debug()  << "continued" << std::endl;
    }
    {
    std::cout << "++++++++++++++++++" << std::endl;
    debug_prefix("foo");
    debug(0) << "level 0." << std::endl;
    debug(0) << "level 0.";
    debug()  << "continued" << std::endl;
    debug(1) << "level 1." << std::endl;
    debug(1) << "level 1.";
    debug()  << "continued" << std::endl;
    debug(2) << "level 2." << std::endl;
    debug(2) << "level 2.";
    debug()  << "continued" << std::endl;
    debug(3) << "level 3." << std::endl;
    debug(3) << "level 3.";
    debug()  << "continued" << std::endl;
  };

  {
    using namespace debugging;
    std::cout << "$$$$$$$$$$$$$$$$$$" << std::endl;
    debug_init(std::cout,-1);  // min output
    Debug_lock start("locked",2);
    {
      debug("shouldn't show up",0) << "level 0." << std::endl;
    }
    debug(2)  << "continued" << std::endl; // note: the 2 is necessary! since evil side effects are in use
  };
  {
    using namespace debugging;
    debug_init(std::cout,1);
    foo* f = new foo();
    f->print_debug();
    debug("main",-1) << "Important message from main." << std::endl;
  }

  {
    std::cout << "testing macros" << std::endl;
    debugging::debug_init(std::cout,0);
    DEBUG0(std::cout << "DEBUG 0" << std::endl);
    DEBUG1(std::cout << "DEBUG 1" << std::endl);
    DEBUG2(std::cout << "DEBUG 2" << std::endl);

  }

  std::string s;
  s = std::string("one") + '\n' + "two";
  std::cout << debugging::no_crlf(s) << std::endl;


  std::cout << "\n\nDONE." << std::endl;  
};
