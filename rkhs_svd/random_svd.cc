// $Id: random_svd.cc,v 3.4 2006/03/05 02:50:41 foster Exp $

#include "random_svd.h"
#include <vector>
#include <iostream>
#include <assert.h>
#include <sstream>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_eigen.h>
#include <getopt.h>


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
	    {"variables",         1, 0, 'v'},  // optional arg,    don't return strings, return 's'
	    {"help",         0, 0, 'h'},  // no arg,          don't return strings, return 'h'
	    {0, 0, 0, 0}                 // terminator I think
	  };

	c = getopt_long (argc, argv, "v:h", long_options, &option_index);
	if (c == -1)
	  break;
			 
	switch (c)
	  {
	  case 'v':
	    {
	      std::stringstream s(optarg);
	      s >> rounds;
	      std::cerr << "setting rounds to " << rounds << std::endl;
	    }
	    break;

	  case 'h':
	    std::cout << "primary switches:" << std::endl << std::endl;
	    std::cout << "      --variables=100   target number of variables" << std::endl;
	    std::cout << "      -v100            same thing" << std::endl << std::endl;
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
  int target = 12345678;  // this is just a check for zero to avoid reading the data
  parse_arg(argc,argv,target);
  if(target == 0)
    return 0;
  gsl_matrix* p_raw_data = read_data();
  if((target == 12345678) || (target == -1))
    target = 3 * int(1 + log(p_raw_data->size2));  // some theory suggests log(dim) is a good idea
  std::cerr << "RANDOM_SVD: Data read:" << p_raw_data->size1 << " x " << p_raw_data->size2
	    << ".    Creating " << target << " new variables."  << std::endl;

  print_random_vectors(p_raw_data,target);
  print_random_vector_pairs(p_raw_data,2 * target);  // you should have more since this is a richer space
  print_random_power(p_raw_data,3 * target,3);
  print_random_power(p_raw_data,target / 2,4);  // OK, these are getting kinda obscure, so not so many
  print_random_power(p_raw_data,target / 2,5);

  return 0;
}
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

int
print_random_vectors(gsl_matrix* p_raw_data, int total_number)
{
  int result = 0;
  double prob_of_non_zero = .2;
  for(int vector_count = 0; vector_count < total_number; ++vector_count)
    {
      int num_variables = p_raw_data -> size2;
      int num_samples = p_raw_data -> size1;
      gsl_vector* p_result = gsl_vector_alloc(num_samples);
      bool got_one = false;
      for(int i = 0; i < num_variables; ++i)
	{
	  if(double(rand())/RAND_MAX < prob_of_non_zero)
	    {  // we only need a sample of the columns
	      got_one = true;
	      if(double(rand())/RAND_MAX < .5)
		gsl_vector_add(p_result, &gsl_matrix_column(p_raw_data,i).vector);
	      else
		gsl_vector_sub(p_result, &gsl_matrix_column(p_raw_data,i).vector);
	    }
	}
      if(got_one)
	{
	  ++result;
	  std::cerr << "RANDOM_SVD: generating #" << vector_count << std::endl;
	  std::cout << "RND_SVD (" << num_variables << "*" << prob_of_non_zero <<  ") #" << vector_count << std::endl;
	  std::cout << *p_result << std::endl;
	}
    }
  return result;
};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
print_random_vector_pairs(gsl_matrix* p_raw_data, int total_number)
{
  int result = 0;
  double prob_of_non_zero = .2;
  int num_variables = p_raw_data -> size2;
  int num_samples = p_raw_data -> size1;
  double upper_bound = 2 * double(num_samples) / (num_variables*num_variables);
  if(prob_of_non_zero > upper_bound)
    prob_of_non_zero = upper_bound;
  for(int vector_count = 0; vector_count < total_number; ++vector_count)
    {
      gsl_vector* p_result = gsl_vector_alloc(num_samples);
      gsl_vector* p_tmp    = gsl_vector_alloc(num_samples);
      bool got_one = false;
      for(int i = 0; i < num_variables; ++i)
	for(int j = i; j < num_variables; ++j)
	  {
	    if(double(rand())/RAND_MAX < prob_of_non_zero)
	      {  // we only need a sample of the columns
		got_one = true;
		gsl_vector_memcpy(p_tmp,&gsl_matrix_column(p_raw_data,i).vector);
		gsl_vector_mul(p_tmp, &gsl_matrix_column(p_raw_data,j).vector);
		if(double(rand())/RAND_MAX < .5)
		  gsl_vector_add(p_result, p_tmp);
		else
		  gsl_vector_sub(p_result, p_tmp);
	      }
	  }
      if(got_one)
	{
	  ++result;
	  std::cerr << "RANDOM_SVD: interaction #" << vector_count << std::endl;
	  std::cout << "RND_SVD_interactions ("  << num_variables*num_variables
		    << "*" << prob_of_non_zero <<  ") #" << vector_count << std::endl;
	  std::cout << *p_result << std::endl;
	}
    }
  return result;
};

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
int
print_random_power(gsl_matrix* p_raw_data, int total_number, int power)
{
  int result = 0;
  int num_variables = p_raw_data -> size2;
  int num_samples = p_raw_data -> size1;
  double target_total_number = pow(num_variables,power);
  int number_to_combine = int(target_total_number);
  if(number_to_combine > num_samples)
    number_to_combine = num_samples; // why do so many?
  for(int vector_count = 0; vector_count < total_number; ++vector_count)
    {
      gsl_vector* p_result = gsl_vector_alloc(num_samples);
      gsl_vector* p_tmp    = gsl_vector_alloc(num_samples);
      for(int i = 0; i < number_to_combine; ++i)
	{
	  gsl_vector_set_all(p_tmp,1.0);
	  for(int j = 0; j < power; ++j)
	    {
	      int index = int(double(rand())/RAND_MAX * num_variables);
	      gsl_vector_mul(p_tmp, &gsl_matrix_column(p_raw_data,index).vector);
	    }
	  if(double(rand())/RAND_MAX < .5)
	    gsl_vector_add(p_result, p_tmp);
	  else
	    gsl_vector_sub(p_result, p_tmp);
	}
      ++result;
      std::cerr << "RANDOM_SVD: power " << power << " #" << vector_count << std::endl;
      std::cout << "RND_SVD_power=" << power << " (" << target_total_number
		<< "*" << number_to_combine/target_total_number << ") #" << vector_count << std::endl;
      std::cout << *p_result << std::endl;
    }
  return result;
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
  unsigned int rotor = 1;
  std::cin >> n;
  std::vector<std::vector<double> > data;
  std::string waste;
  getline(std::cin,waste);  // kills anything after the n
  getline(std::cin,waste);  // should be the name of the y
  if(waste != "Y")
    {
      std::cerr << "RANDOM_SVD: You are supposed to have n and Y come first." << std::endl;
      assert(0);
    }
  else
    {
      std::cerr << "RANDOM_SVD: Thanks for providing n and Y in  the first three rows.  I'll ignore the Y." << std::endl;
      getline(std::cin,waste);  // should be the data for y
    }
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
      if(data.size() > rotor)
	{
	  std::cerr << "RANDOM_SVD: read " << rotor << " rows..." << std::endl;
	  rotor = 10*rotor;
	}
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
