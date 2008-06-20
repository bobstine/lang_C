// $Id: readLine.cc,v 1.1 2001/10/12 12:51:00 bob Exp $ -*- c++ -*-

#include <iostream.h>
#include <string>

main ()
{
  string s;

  while (! cin.eof() )
  { getline(cin,s);
    cout << "(Read the string {" << s << "})" << endl;
  }
}
