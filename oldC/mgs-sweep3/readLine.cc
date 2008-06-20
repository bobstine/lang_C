// $Id: readLine.cc,v 1.1.1.1 2001/12/04 20:32:35 bob Exp $ -*- c++ -*-

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
