// $Id: svd.cc,v 1.1 2008/02/01 20:28:30 bob Exp $

#include "svd.h"
#include <vector>
#include <iostream>
#include <assert.h>
#include <sstream>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_eigen.h>
#include <getopt.h>
#include <math.h>

#include <time.h>
time_t           global_start_time = time(0);
double            global_minutes = 40 * 24 * 60;
bool
time_ended_p()
{
  return (difftime(time(0),global_start_time) >= 60.0 * (global_minutes - .05));
}

std::string serial_number_prefix = "0";
int serial_number_counter = 0;

std::string serial_number()
{
  ++serial_number_counter;
  std::stringstream s;  
  s << "s#" << serial_number_prefix << serial_number_counter;
  return s.str();
}


double l2_distance(const gsl_vector& d1, const gsl_vector& d2)
{  // (X-Y)^2 = |X|^2 + |Y|^2 - 2 X'*Y
  double xx = 0.0;
  double yy = 0.0;
  double xy = 0.0;
  gsl_blas_ddot(&d1,&d1,&xx);
  gsl_blas_ddot(&d2,&d2,&yy);
  gsl_blas_ddot(&d1,&d2,&xy);
  return xx + yy - 2 * xy;
}

class rbf: public kernel
{
public:
  virtual ~rbf(){};

  rbf(double sd,double ratio):
    m_sd_data(sd),
    m_sd_used(sd * ratio)
  {
  };

  double
  operator()(const gsl_vector& d1, const gsl_vector& d2) const
  {
    double ss = l2_distance(d1,d2);
    return exp(- ss / m_sd_used);
  }

  std::string
  name() const
  {
    std::stringstream s;
    s << m_sd_used / m_sd_data;
    std::string result = "rbf:sd=" + s.str();
    return result;
  }


private:
  double m_sd_data;
  double m_sd_used;
};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

class covariance: public kernel
{
  std::string m_name;
public:
  virtual ~covariance(){};
  covariance(const std::string& name)
    :m_name(name)
  {
  };

  double
  operator()(const gsl_vector& d1, const gsl_vector& d2) const
  {
    double ss = 0;
    gsl_blas_ddot(&d1,&d2,&ss);
    return ss;
  }
  
  std::string
  name() const
  {
    return m_name;
  }

};


namespace
{
  void parse_arg(int argc, char** argv,
		  int        & rounds)
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
	    {"variables",         1, 0, 'v'}, 
	    {"serial-number",     1, 0, 'n'}, 
	    {"minutes",           1, 0, 'm'}, 
	    {"help",              0, 0, 'h'}, 
	    {0, 0, 0, 0}                 // terminator I think
	  };

	c = getopt_long (argc, argv, "v:n:m:h", long_options, &option_index);
	if (c == -1)
	  break;
			 
	switch (c)
	  {
	  case 'v':
	    {
	      std::stringstream s(optarg);
	      s >> rounds;
	    }
	    break;
	  case 'm':
	    {
	      std::stringstream s(optarg);
	      s >> global_minutes;
	    }
	    break;
	  case 'n':
	    {
	      serial_number_prefix = optarg;
	    }
	    break;

	  case 'h':
	    std::cout << "primary switches:" << std::endl << std::endl;
	    std::cout << "      --variables=100   target number of variables" << std::endl;
	    std::cout << "      -v100            same thing" << std::endl << std::endl;
	    std::cout << "      -m5              run for less than 5 minutes" << std::endl << std::endl;
	    std::cout << "      --serial-number=200" << std::endl;
	    std::cout << "      -n200            serial number prefix: s#2001, s#2002, s#2003,..." << std::endl << std::endl;
	    exit(0);
	    break;
	  }
      }
  }
}


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
main(int argc, char** argv)
{
  int target = 1000000;  // default is work forever or at least a long time.
  parse_arg(argc,argv,target);
  if(time_ended_p())
    return 0;
  if(target <= 0)
    return 0;
  std::cerr << "SVD: Reading data from standard input." << std::endl;
  gsl_matrix* p_raw_data = read_data();
  std::cerr << "SVD: Data read:" << p_raw_data->size1 << " x " << p_raw_data->size2 << "."  << std::endl;
  int number_written = print_covariance(p_raw_data,covariance("raw PC"),.99,.001); 
  std::cerr << "SVD: Linear kernel wrote " << number_written << " variables." << std::endl;
  target -= number_written;
  if(target <= 0)
    return 0;

  if(!time_ended_p())
    {
      gsl_matrix* p_centered_data = center(*p_raw_data);
      number_written = print_covariance(p_centered_data,covariance("centered PC"),.99,.001);
      std::cerr << "SVD: Centered linear kernel wrote " << number_written << " variables." << std::endl;
      target -= number_written;
    }
  if(target <= 0 || time_ended_p())
    return 0;

  double sd = sqrt(MS_distance(p_raw_data));
  std::cerr << "SVD: SD = " << sd << "." << std::endl;
  
  double ratio = .001;
  int number_created = 10;
  int base_10 = 0;
  while(((number_created > 2) || ratio < 10000) && (target > 0) && !time_ended_p())
    {
      number_created = print_covariance(p_raw_data,rbf(sd,ratio),.99,.001);
      target -= number_created;
      ++base_10;
      if(base_10 % 3 == 2)
	ratio = 2.5 * ratio;
      else
	ratio = 2 * ratio;
    }
  return 0;
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

int
print_covariance(gsl_matrix* p_raw_data, const kernel& k,double ratio_spread, double smallest)
{
  gsl_matrix* p_V=0;
  gsl_vector* p_S=0;  // the diagonal "matrix"
  gsl_matrix* p_cov=0;

  int n = p_raw_data->size1;
  int sample_at = 500;
  bool use_SV = false;             // SLOW!  But called the ground truth

  bool use_all_eigen = (n < sample_at);     // FASTER: But still too slow to deal with 10k rows
  bool use_sampled_eigen = !use_all_eigen;  // FASTEST: But inaccurate.  Actually makes approximations
  if(use_SV)
    {
      p_cov = rkhs_covariance(p_raw_data,p_raw_data,k);
      int dim2 = p_cov -> size2;

      p_V = gsl_matrix_alloc(dim2,dim2);
      p_S = gsl_vector_alloc(dim2);
      gsl_vector* p_WORK = gsl_vector_alloc(dim2);  // tempary work space
      gsl_linalg_SV_decomp (p_cov,p_V,p_S,p_WORK);  // NOTE: p_cov is changed!
      std::cerr << "SVD: gsl_linalg_SV_decomp finished   (" << k.name() <<  ")   @ " << double(clock())/CLOCKS_PER_SEC << "s " << std::endl;
    }
  if(use_all_eigen)
    {
      p_cov = rkhs_covariance(p_raw_data,p_raw_data,k);
      int dim2 = p_cov -> size2;

      p_V = gsl_matrix_alloc(dim2,dim2);
      p_S = gsl_vector_alloc(dim2); 
      gsl_eigen_symmv_workspace* p_WORK = gsl_eigen_symmv_alloc(dim2);  // tempary work space
      gsl_eigen_symmv(p_cov, p_S,p_V,p_WORK);
      std::cerr << "SVD: gsl_eigen_symmv   (" << k.name() <<  ")   @ " << double(clock())/CLOCKS_PER_SEC << "s " << std::endl;
      gsl_eigen_symmv_free(p_WORK);
      gsl_eigen_symmv_sort (p_S, p_V, GSL_EIGEN_SORT_VAL_DESC);
    }
  if(use_sampled_eigen)
    {
      gsl_matrix* p_sampled = sample_rows(p_raw_data,sample_at); // currently grab whole data
      p_cov = rkhs_covariance(p_sampled,p_sampled,k);
      int dim2 = p_cov -> size2;

      p_V = gsl_matrix_alloc(dim2,dim2);
      p_S = gsl_vector_alloc(dim2); 
      gsl_eigen_symmv_workspace* p_WORK = gsl_eigen_symmv_alloc(dim2);  // tempary work space
      gsl_eigen_symmv(p_cov, p_S,p_V,p_WORK);
      std::cerr << "SVD: gsl_eigen_symmv   (" << k.name() <<  ")   @ " << double(clock())/CLOCKS_PER_SEC << "s " << std::endl;
      gsl_eigen_symmv_free(p_WORK);
      gsl_eigen_symmv_sort (p_S, p_V, GSL_EIGEN_SORT_VAL_DESC);
      p_cov = rkhs_covariance(p_raw_data,p_sampled,k);
    }

  // Friendly summary of results
  if(p_S->size > 20)
    std::cerr << "SVD: several of the " << p_S->size << " eigenvalues: "
	      << gsl_vector_subvector(p_S,0,10).vector << " . . . " 
	      << gsl_vector_subvector(p_S,p_S->size - 3, 3).vector << std::endl;
  else
    std::cerr << "SVD: all " << p_S->size << " eigenvalues: " << *p_S << std::endl;

  unsigned int current_index = 0;
  double current_eigen = gsl_vector_get(p_S,current_index);
  double max_eigen = current_eigen;
  double next_eigen = 0;
  double last_eigen = 2.0 * current_eigen;
  int result = 0;

  std::string holdem;
  while((current_index < p_S->size) &&	(current_eigen > smallest * max_eigen))
    {
      int next_index = current_index + 1;
      if(next_index != int(p_S->size))
	{
	  next_eigen = gsl_vector_get(p_S,next_index);
	}
      else
	next_eigen = 0.0;

      if((current_eigen / last_eigen < ratio_spread) &&
	 (next_eigen / current_eigen < ratio_spread))
	{
	  ++result;
	  std::cout << k.name() << "@" << round_to_2_digits(current_eigen/max_eigen) << serial_number() << std::endl;
	  if(!use_sampled_eigen)
	    std::cout << gsl_matrix_column(p_V,current_index).vector << std::endl;
	  else
	    {
	      gsl_vector* p_result = gsl_vector_alloc(p_raw_data->size1);
	      gsl_blas_dgemv (CblasNoTrans, 1 / current_eigen, p_cov, &gsl_matrix_column(p_V,current_index).vector, 0, p_result);
	      std::cout << *p_result << std::endl;
	    }
	}

      last_eigen = current_eigen;
      current_eigen = next_eigen;
      current_index = next_index;
    }

  return result;
};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

double
MS_distance(gsl_matrix* p_data)
{
  int n = p_data->size1;
  double total = 0;

  for(int i = 0; i < n; ++i)
    for(int j = 0; j < n; ++j)
      total += l2_distance(gsl_matrix_row(p_data,i).vector,gsl_matrix_row(p_data,j).vector);
  
  return total / (n * n);
};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

gsl_matrix*
sample_data()
{
  int dim1 = 15;
  int dim2 = 2;
  gsl_matrix* p_result = gsl_matrix_alloc(dim1,dim2);  
  for(int i = 0; i < dim1; ++i)
    {
      gsl_matrix_set(p_result,i,0, (i % 5) - 2.0);
      gsl_matrix_set(p_result,i,1, (i % 3) - 1.0);
    }
  return p_result;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
gsl_matrix*
read_data()
{
  int n;
  std::cin >> n;
  std::vector<std::vector<double> > data;
  std::string waste;
  getline(std::cin,waste);  // kills anything after the n
  getline(std::cin,waste);  // should be the name of the y
  assert(waste == "Y");
  getline(std::cin,waste);  // should be the data for y
  while(!std::cin.eof())
    {
      getline(std::cin,waste);  // should be the name of x
      std::vector<double> column(n);
      for(int i = 0; i < n; ++i)
	{
	  std::cin >> column[i];
	}
      data.push_back(column);
      std::cin >> std::ws;
    }
  int p = data.size();
  gsl_matrix* p_result = gsl_matrix_alloc(n,p);  
  for(int i = 0; i < n; ++i)
    for(int j = 0; j < p; ++j)
      gsl_matrix_set(p_result,i,j, data[j][i]);
  return p_result;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
gsl_matrix*
center(gsl_matrix& raw)
{
  int n = raw.size1;
  int p = raw.size2;
  gsl_matrix* p_result = gsl_matrix_alloc(n,p);

  gsl_vector* ones = gsl_vector_alloc(n);
  gsl_vector_set_all(ones,1.0);

  for(int i = 0; i < p;++i)
    {
      gsl_vector raw_col = gsl_matrix_column(&raw,i).vector;
      gsl_vector cen_col = gsl_matrix_column(p_result,i).vector;
      gsl_vector_memcpy(&cen_col,&raw_col);
      double total;
      gsl_blas_ddot(&cen_col,ones,&total);
      double center = total / n;
      gsl_vector_add_constant(&cen_col,-center);
      double scale;
      gsl_blas_ddot(&cen_col,&cen_col,&scale);
      scale = scale / sqrt(n);
      gsl_vector_scale(&cen_col,1/scale);
    }

  return p_result;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

gsl_matrix*
rkhs_covariance(gsl_matrix* p_left,gsl_matrix* p_right,const kernel& k)
{
  int n = p_left->size1;
  int m = p_right->size1;
  
  gsl_matrix* p_result = gsl_matrix_alloc(n,m);

  for(int i = 0; i < n; ++i)
    {
      for(int j = 0; j < m; ++j)
	{
	  gsl_matrix_set(p_result,i,j,
			 k(gsl_matrix_row(p_left,i).vector,gsl_matrix_row(p_right,j).vector));
	}
      if((n > 100) && (i % 100 == 0))
	std::cerr << "SVD: " << i << " / " << n << "  (m = " << m 
		  << ") @ " << double(clock())/CLOCKS_PER_SEC << "s " << std::endl;
    }
  return p_result;
}



/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

double rounding(double mu,double s, double scaling);
double round_to_4_digits(double mu);

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

double
rounding(double mu,double s)
{
  double scaling = 1;

  if(fabs(mu) < 1e-9)
    return 0.0;

  if(mu == 0)
    return mu;

  if(s < 0)
    s = - s;
  
  double log10 = log(s)/log(10);

  log10 = floor(log10);
  double d = exp(log10 * log(10));
   s = (rint(s*scaling/d))*(d/scaling);
   mu = (rint(mu*scaling/d))*(d/scaling);
   // the following two lines remove -0's from outputs
   if(mu == 0)
     mu = 0;
  if(s == 0)
    s = 0;
  return mu;
};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

double
round_to_4_digits(double mu)
{
  return rounding(mu,mu/1000.);
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
double
round_to_2_digits(double mu)
{
  return rounding(mu,mu/10.);
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

std::ostream&
operator<<(std::ostream& o, const gsl_matrix& m)
{
  for(unsigned int i = 0; i < m.size1; ++i)
    {
      o << "\t";
      for(unsigned int j = 0; j < m.size2; ++j)
	o << round_to_4_digits(gsl_matrix_get(&m,i,j)) <<  "\t";
      o << std::endl;
    };
  return o;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


std::ostream&
operator<<(std::ostream& o, const gsl_vector& m)
{
  for(unsigned int i = 0; i < m.size; ++i)
    {
      o << gsl_vector_get(&m,i);
      if(i != m.size-1)
	o <<  " ";
    };
  return o;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

gsl_matrix*
sample_rows(gsl_matrix* p_data, int n_target)
{
  int n = p_data->size1;
  
  gsl_matrix* p_result = gsl_matrix_alloc(n_target,p_data->size2);

  int slots_left = n_target;
  for(int i = 0; i < n; ++i)
    {
      if(double(rand())/RAND_MAX < slots_left / (n - i))
	{
	  gsl_vector_memcpy(&gsl_matrix_row(p_result,n_target - slots_left).vector,
			    &gsl_matrix_row(p_data,i).vector);
	  --slots_left;
	}
    }
  return p_result;
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
