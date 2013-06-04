#include <Eigen/Dense>

#include "read_utils.h"

#include <time.h>
#include <getopt.h>
#include <iostream>
#include <algorithm>

typedef Eigen::MatrixXf Matrix;
typedef Eigen::VectorXf Vector;
typedef Eigen::RowVectorXf RowVector;


using std::string;
using std::endl;


void
print_time(string const& s, clock_t const& start, clock_t const& stop)
{
  clock_t diff (stop-start);
  std::clog << "\nTIME: " << s << " [" << ((float)diff)/CLOCKS_PER_SEC << " sec]\n";
}


void
parse_arguments(int argc, char** argv, int &n, int &p, int &k);


int main(int argv, char **argc)
{
  int n=100, p=2, k=1;

  parse_arguments(argv, argc, n, p, k);
  std::clog << "\n\nMAIN: Options are n=" << n << " p=" << p << "  k=" << k << endl;
  
  int nShown = (p < 11) ? p : 10;

  clock_t start_time = clock();
  Matrix X = Matrix::Random(n,p);
  Vector Y = Vector::Random(n);
  print_time("Filling random data", start_time, clock());

  start_time = clock();
  {
    Matrix XtX = X.transpose() * X;
    Vector XtY = X.transpose() * Y;
    Vector b = XtX.colPivHouseholderQr().solve(XtY);
    print_time("Compute b = i(XpX)XtY", start_time, clock());
    std::cout <<            b.transpose()              << endl;
    std::clog << " b = " << b.transpose().head(nShown) << endl;
  }
  
  start_time = clock();
  {
    std::vector<int> shuffle(n);
    for(int i=0; i<n; ++i)      shuffle[i] = i;
    RowVector b  = RowVector::Zero(p);
    RowVector w(p);
    // one pass for time being until sort out how to do weighting
    // k++
    k = 1;
    while(k--)
    { std::random_shuffle (shuffle.begin(), shuffle.end());
      RowVector SS = RowVector::Ones(p);
      for(int i=0; i<n; ++i)
      { int r = shuffle[i];
	float e = Y(r)-X.row(r).dot(b);
	RowVector XtXix = X.row(r).array()/SS.array();
	float h = X.row(r).dot(XtXix);
	b  += (XtXix.array() * e/(1+h)).matrix();
	SS += X.row(r).array().square().matrix();
      }
    }
    print_time("Compute stochastic grad b", start_time, clock());
    std::cout <<            b              << endl;
    std::clog << " b = " << b.head(nShown) << endl;
  }
}


void
parse_arguments(int argc, char** argv, int &n, int &p, int &k)
{
  static struct option long_options[] = {
    // bigram prep options
    {"n",            required_argument, 0, 'n'},
    {"p",            required_argument, 0, 'p'},
    {"sg_passes",    required_argument, 0, 'k'},
    {0, 0, 0, 0}                             // terminator 
  };
  int key;
  int option_index = 0;
  while (-1 !=(key = getopt_long (argc, argv, "n:p:k:", long_options, &option_index))) // colon means has argument
  {
    // std::cout << "Option key " << char(key) << " for option " << long_options[option_index].name << ", option_index=" << option_index << std::endl;
    switch (key)
    {
    case 'n' :
      {
	n = read_utils::lexical_cast<int>(optarg);
	break;
      }
    case 'p' :
      {
	p = read_utils::lexical_cast<int>(optarg);
	break;
      }
    case 'k' :
      {
	k = read_utils::lexical_cast<int>(optarg);
	break;
      }
    default:
      {
	std::cout << "PARSE: Option not recognized; returning.\n";
      }
    } // switch
  }
}
