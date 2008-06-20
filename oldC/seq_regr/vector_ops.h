// $Id: vector_ops.h,v 1.1 2003/12/06 03:13:04 bob Exp $ -*- c++ -*-

/*
  Use these operators if run code with Vector defined to be a std::vector<double>

   5 Dec 03 ... Removed from the sequential regression code.
   
*/

inline double
average (std::vector<double> const& v)
{
  double sum(0.0);
  for (unsigned int i=0; i<v.size(); ++i)
  { 
    sum += v[i];
  }
  return sum/v.size();
}

inline double
sum_of_squares (std::vector<double> const& v, double avg)
{
  double ss(0.0);
  for (unsigned int i=0; i<v.size(); ++i)
  { double dev (v[i]-avg);
    ss += dev * dev;
  }
  return ss;
}

inline double
cross_product(std::vector<double> x, double xBar, std::vector<double> y, double yBar)
{
  double cp(0.0);
  for (unsigned int i=0; i<y.size(); ++i)
    cp += (y[i]-yBar)*(x[i]-xBar);
  return cp;
}

inline void
fill_cross_product_vector(std::vector<double> const& z, double zBar,
			  std::vector< std::vector<double> > const& X, std::vector<double> XBar,
			  double *cp)
{
  for (unsigned int j=0; j<X.size(); ++j)
    cp[j] = cross_product(z,zBar,X[j],XBar[j]);
}



inline
std::ostream&
operator<< (std::ostream& os, std::vector<double> const& x)
{
  os << "[" << x.size() << "] " ;
  for (unsigned int i=0; i<x.size(); ++i)
    os << x[i] << " ";
  return os;
}


