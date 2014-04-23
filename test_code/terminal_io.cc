#include <iostream>
using std::cout;
using std::cin;
using std::endl;

#include <string>
using std::string;
using std::getline;

int main(){

  int id;
  string name;
  double salary;

  cout<<"Enter tax ID: ";
  cin>>id;
  cin.ignore();               // Need this line to absorb newline hanging there
  cout<<"\nEnter name: ";
  getline(cin,name);
  cout<<"\nEnter salary: ";
  cin>>salary;

  cout<<"\n\n"<<id<<' '<<name<<' '<<salary<<endl;

  return 0; //indicates success
}//end main 
