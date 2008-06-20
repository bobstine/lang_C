

// $Id: go_modeling.cc,v 4.14 2002/10/18 13:54:45 foster Exp $-*- c++ -*-

#include "go_basics/game.h"
#include "go_basics/board.h"
#include "go_basics/game_manager.h"

#include "persistence/backup_and_restore.h"
#include "persistence/library.h"

#include "go_context/context_forecast_imp.h"
#include "pattern/forecast.h"
#include "thing/print_map.h"

#include "time_bank/time_bank.h"
#include "time_bank/exchange.h"
#include "time_bank/master_account.h"
#include "time_bank/constants.h"
#include "time_bank/tic_toc.h"
#include "go_thing/go_model.h"
#include "go_thing/globals.h"
#include "go_thing/links.h"
#include "go_thing/board_template.h"
#include "debugging/debug.h"
#include "statistics/statistics.h"
#include <iostream>
#include <fstream>
#ifdef USE_STRSTREAM
#include <strstream.h>
#else
#include <sstream>
#endif
#include <getopt.h>

void check_point_with_diagnostics(int observation);


void parse_argc(int argc, char** argv,
		int        & limit,    // all will be modified
		int        & save_frequency,
		std::string& name_of_file_to_save_as,
		std::string& name_of_file_read,
		bool       & default_seed,
		int        & rent_collection_interval,
	        bool       & real_time,
		ofstream   & debugger);

using namespace time_bank;

int
main(int argc,char** argv)
{
  int limit = 10 * 1000 * 1000;
  int save_frequency = 0;  // default series of saves
  std::string name_of_file_to_save_as;  
  std::string name_of_file_read;  
  bool real_time = true;
  bool default_seed = false;  // turn on to make runs reproducable
  int rent_collection_interval = 100;
  time_bank::Exchange::get_singleton()->target_memory_usage(300 * 1000 * 1000 * time_bank::Bytes());  
  ofstream debugger("/tmp/go_modeling.debugging",ios::app);
  debugging::debug_init(debugger,1);  // default debug level
  debugging::debug("start",3) << std::string(100,'S') << std::endl;

  parse_argc(argc, argv,
	     limit,           // passed reference--this and all the following are changed
	     save_frequency, 
	     name_of_file_to_save_as,
	     name_of_file_read,
	     default_seed,
	     rent_collection_interval,
	     real_time,
	     debugger);


  double sampling_fraction = .01;
  double printing_rate = 10;  // about every 10% increase in output do a print
  Cpu_seconds time_per_look = 60 * Cpu_seconds();
  ifstream games("/home/foster/games/finished_95_01-06");

  Time_account* p_account = get_account("go_modeling",true);
  
  Second wall_time_per_look =  Second() *(time_per_look/ Cpu_seconds());  // single processor :-)
  {
    using namespace time_bank;
    Tic_toc::use_real_time();
    Time_account::print_reaped_items(true);
  };
  go_thing::Go_model forecaster(p_account->find_or_open_account("forecaster"));
  Time_account* p_checks = p_account->find_or_open_account("checks");
  
  int game_counter = 0;
  int observations = 0;
  // training
  statistics::Uniform U;
  while(!games.eof())
    {
      time_bank::Timer t(p_account->find_or_open_account("main"));  // forces desctruction and hence updates account
      go_basics::Game g;
      games >> g >> ws;
      std::cout << "game " << game_counter << std::endl;
      debugging::debug("main",1) << "game " << game_counter << std::endl;
      for(go_basics::Game_index i = g.begin(); i != g.end(); ++i)
	{
	  //	  if(U() <= sampling_fraction)
	  if((U() <= 2*sampling_fraction) && (i.second == go_basics::Stone_color::BLACK())) // only black-to-play situations
	    {
	      {
		Timer s(p_checks);
 		forecaster.learn_from_whole_game(g,i,p_account->write_a_check(price(time_per_look)));
	      }
	      ++observations;		
	      std::cout << "\t" << observations << " @ " << time_bank::Tic_toc::cpu_time_only() << std::endl;
	      debugging::debug("main",1) << "\t" << observations << " @ " << time_bank::Tic_toc::cpu_time_only() << std::endl;
	      t.update_time_account();
	      if((U() < printing_rate/game_counter) && game_counter > 50)
		{
		  //		  std::cout << "\t\t\tCollecting memory." << std::endl;
		  check_point_with_diagnostics(observations);
		}
	      if(U() < .01) // only collect rent every once in a while
		{
		  std::cout << "\t\t\tCollecting memory." << std::endl;
		  time_bank::collect_rent(wall_time_per_look);
		}
	    }
	};
      game_counter ++;
    };
  time_bank::collect_rent(wall_time_per_look); 

  std::cout << "Done collecting data." << std::endl;
  std::cout << "saving information to disk." << std::endl;
  check_point_with_diagnostics(observations);
  std::cout << "\n\nDONE" << std::endl;
};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void parse_argc(int argc, char** argv,
		int        & limit,    // all will be modified
		int        & save_frequency,
		std::string& name_of_file_to_save_as,
		std::string& name_of_file_read,
		bool       & default_seed,
		int        & rent_collection_interval,
	        bool       & real_time,
		ofstream   & debugger)
{
  int c;
  /////////////////////////////////////////////////////////////////////////////////////////////////////
  //
  // Reading in the command line parameters (this was modified from "man 3 getopt".
  // So it looks like C instead of C++.  Oh well, it runs.)
  //
  while (1)
    {
      int option_index = 0;
      static struct option long_options[] =
      {
	{"seed",         2, 0, 's'},  // optional arg,    don't return strings, return 's'

	{"rent",         1, 0, 'r'},  // has argument,    don't return strings, return 'r'

	{"limit",        1, 0, 'L'},  // has argument,    don't return strings, return 'l'

	{"debug-level",  1, 0, 'd'},  // has argument,    don't return strings, return 'd'

	{"input",        2, 0, 'i'},  // optional arg,    don't return strings, return 'i'

	{"output",       2, 0, 'o'},  // optional arg,    don't return strings, return 's'

	{"save-freq",    1, 0, 'S'},  // has argument,    don't return strings, return 'S'

	{"real-time-off",0, 0, 't'},  // no arg,          don't return strings, return 't'

	{"help",         0, 0, 'h'},  // no arg,          don't return strings, return 'h'

	{0, 0, 0, 0}                 // terminator I think
      };

      c = getopt_long (argc, argv, "sr:d:l:S:ioht", long_options, &option_index);
      if (c == -1)
	break;
			 
      switch (c)
	{
	case 's':
	  if(optarg)
	    {
#ifdef USE_STRSTREAM
	      strstream s;
	      s << optarg;
#else
	      std::stringstream s(optarg);
#endif
	      int seed;
	      s >> seed;
	      std::cout << "setting seed to " << seed << std::endl;
	      statistics::Probability(int(seed));  // silly c++ syntax
	    }
	  else
	    {
	      std::cout << "setting seed to 213501322." << std::endl;
	      statistics::Probability(213501322);
	      default_seed = true;
	    }
	  break;
	case 'r':
	  if(optarg)
	    {
#ifdef USE_STRSTREAM
	      strstream s;
	      s << optarg;
#else
	      std::stringstream s(optarg);
#endif
	      s >> rent_collection_interval;
	      std::cout << "setting rent collection interval to " << rent_collection_interval << std::endl;
	    }
	  break;
	case 'L':
	  if(optarg)
	    {
#ifdef USE_STRSTREAM
	      strstream s;
	      s << optarg;
#else
	      std::stringstream s(optarg);
#endif
	      s >> limit;
	      std::cout << "Running for " << limit << " rounds." << std::endl;
	    }
	  break;

	case 'S':
	  if(optarg)
	    {
#ifdef USE_STRSTREAM
	      strstream s;
	      s << optarg;
#else
	      std::stringstream s(optarg);
#endif
	      s >> save_frequency;
	      std::cout << "Saving every " << save_frequency << " rounds." << std::endl;
	    }
	  break;
	  
	case 'd':
	  assert(optarg);
	    {
#ifdef USE_STRSTREAM
	      strstream s;
	      s << optarg;
#else
	      std::stringstream s(optarg);
#endif
	      int debug_level;
	      s >> debug_level;
	      std::cout << "Setting debugging level to " << debug_level << std::endl;
	      debugging::debug_init(debugger,debug_level);  // this controls level of debugger
	    }
	  break;

	case 'i':
	  {
	    if(optarg)
	      name_of_file_read = optarg;
	    else
	      name_of_file_read = "saved_model";
	    std::cout << "restoring from file " << name_of_file_read << std::endl;
	    ifstream reader(name_of_file_read.c_str());
	    //	    persistence::Debugging_ibar in(reader);
	    //	    persistence::Backup_and_restore::restore(in);
	    persistence::Backup_and_restore::restore(reader);
	    std::cout << "Done reading from file " << name_of_file_read << std::endl;
	  }
	  break;

	case 'o':
	  if(optarg)
	    name_of_file_to_save_as = optarg;
	  else
	    name_of_file_to_save_as = "saved_model";
	  std::cout << "Will save to file " << name_of_file_to_save_as << std::endl;
	  break;

	case 'h':
	  std::cout << "primary switches:" << std::endl << std::endl;
	  std::cout << "      --input=foo       restore-from file foo" << std::endl;
	  std::cout << "      -i                restore-from saved_model" << std::endl << std::endl;

	  std::cout << "      --output=foo      save-to file foo" << std::endl;
	  std::cout << "      -o                save-to saved_model" << std::endl << std::endl;

	  std::cout << "      --save-freq=1000  how often to save" << std::endl;
	  std::cout << "      -S=1000"                             << std::endl << std::endl;

	  std::cout << "switches useful for debugging:" << std::endl << std::endl;
	  
	  std::cout << "      -d,--debug-level  3=results only, 2=important mesage, ...,0=everything" << std::endl << std::endl;

	  std::cout << "      --seed=1234       set the seed to 1234 (otherwise the seed is different every time)." << std::endl;
	  std::cout << "      -s,--seed         set the seed to 213501322 (resets seed to this value after saving to disk)." << std::endl << std::endl;
	  std::cout << "      --rent=1000       set rent collection interval to every 1000 rounds. (default = 100)" << std::endl;
	  std::cout << "      -r1000            " << std::endl << std::endl;

	  std::cout << "      --limit=10000     number of rounds before successfully stopping (default = 10M rounds)" << std::endl;
	  std::cout << "      -L10000           " << std::endl << std::endl;

	  std::cout << "      -t,--no-real-time accounting based on cpu time is turned off" << std::endl << std::endl;
	  
	  std::cout << "For production runs, don't use -s or --seed." << std::endl << std::endl;
	  std::cout << "For reproducable runs use -stio.  This will allow restoring from disk" << std::endl;
	  std::cout << "and still reprodustd::coutg the exact same run." << std::endl << std::endl;
	  std::cout << "The following two are identical:" << std::endl << std::endl;
	  std::cout << "         go_modeling -stio" << std::endl << std::endl;
	  std::cout << "         go_modeling --seed --no-real-time --input --output=saved_model" << std::endl << std::endl;
	  exit(0);
	  break;

	case 't':
	  std::cout << "Turning off real time accounting."<< std::endl;
	  real_time = false;
	  break;

	case '?':
	  break;

	default:
	  std::cout << "error: " << c << " with opt of " << optarg;
	  break;
	}
    }
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void
check_point_with_diagnostics(int observation)
{
  std::cout << "Writting out the state of the program: ./saved_model, /tmp/*" << std::endl;
  {
    ofstream out("/tmp/some");
    out << "\t" << observation << " @ " << time_bank::Tic_toc::cpu_time_only() << std::endl;
    time_bank::print_memory_on(out);
  }
  {
    ofstream out("./saved_model");
    persistence::Backup_and_restore::backup(out);
  }
  {
    ofstream out("/tmp/all");
    out << "\t" << observation << " @ " << time_bank::Tic_toc::cpu_time_only() << std::endl;
    time_bank::print_all(out);
  }
  {
    ofstream out("/tmp/accounts");
    out << "\t" << observation << " @ " << time_bank::Tic_toc::cpu_time_only() << std::endl;
    time_bank::recursive_print_on(out,true,0 * time_bank::Dollars());
  }
  {
    ofstream out("/tmp/accounts.large");
    out << "\t" << observation << " @ " << time_bank::Tic_toc::cpu_time_only() << std::endl;
    time_bank::recursive_print_on(out,true,10 * time_bank::Dollars());
  }
  {
    ofstream out("/tmp/class_map");
    out << "\t" << observation << " @ " << time_bank::Tic_toc::cpu_time_only() << std::endl;
    thing::print_map(out);
  }
  {
    ofstream regr("/tmp/regressions");
    regr << "\t" << observation << " @ " << time_bank::Tic_toc::cpu_time_only() << std::endl;
    regr << "FINAL_BLACK_CONTROL FINAL_BLACK_CONTROL FINAL_BLACK_CONTROL FINAL_BLACK_CONTROL FINAL_BLACK_CONTROL" << std::endl;
    dynamic_cast<pattern::Forecast&>(*go_thing::p_final_black_control).print_on(regr);
    regr << "FINAL_EMPTY FINAL_EMPTY FINAL_EMPTY FINAL_EMPTY FINAL_EMPTY" << std::endl;
    dynamic_cast<pattern::Forecast&>(*go_thing::p_final_empty).print_on(regr);
    regr << "PLAYING_NEXT PLAYING_NEXT PLAYING_NEXT PLAYING_NEXT PLAYING_NEXT PLAYING_NEXT PLAYING_NEXT" << std::endl;
    dynamic_cast<pattern::Forecast&>(*go_thing::p_playing_next).print_on(regr);
    //  regr << "GROUP_FRACTION_ALIVE  GROUP_FRACTION_ALIVE  GROUP_FRACTION_ALIVE  GROUP_FRACTION_ALIVE" << std::endl;
    //  dynamic_cast<pattern::Forecast&>(*go_thing::p_group_fraction_alive).print_on(regr);
  }
  std::cout << "Files written." << std::endl;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

