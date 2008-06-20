// $Id: svd.h,v 3.0 2004/11/19 18:58:36 foster Exp $ 


#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_blas.h>
#include <iostream>
#include <string>

// int main();

struct kernel
{
  virtual double operator()(const gsl_vector&, const gsl_vector&) const = 0;
  virtual std::string name() const = 0;
};

double MS_distance(gsl_matrix* p_raw_data);

int print_covariance(gsl_matrix* p_raw_data, const kernel& k,
		     double necessary_neighbor_ratio, double smallest);  // returns number columns created
gsl_matrix* read_data();
gsl_matrix* center(gsl_matrix& raw);
gsl_matrix* rkhs_covariance(gsl_matrix* p_data,gsl_matrix* p_data,const kernel&);
double round_to_4_digits(double);
double round_to_2_digits(double);


// printing routines
std::ostream& operator<<(std::ostream& o, const gsl_matrix& m);
std::ostream& operator<<(std::ostream& o, const gsl_vector& m);

gsl_matrix* sample_rows(gsl_matrix* p_data,int);
