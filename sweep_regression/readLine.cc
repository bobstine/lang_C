// $Id: readLine.cc,v 1.1 2005/06/13 20:47:51 bob Exp $ -*- c++ -*-

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
