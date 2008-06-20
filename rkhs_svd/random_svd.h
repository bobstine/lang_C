// $Id: random_svd.h,v 3.3 2006/02/22 03:49:51 foster Exp $ 


#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_blas.h>
#include <iostream>
#include <string>

// int main();

int print_random_vectors(gsl_matrix* p_raw_data, int number_to_create);
int print_random_vector_pairs(gsl_matrix* p_raw_data, int number_to_create);
int print_random_power(gsl_matrix* p_raw_data, int total_number, int power);

gsl_matrix* read_data();
gsl_matrix* center(gsl_matrix& raw);

double round_to_4_digits(double);
double round_to_2_digits(double);


// printing routines
std::ostream& operator<<(std::ostream& o, const gsl_matrix& m);
std::ostream& operator<<(std::ostream& o, const gsl_vector& m);

gsl_matrix* sample_rows(gsl_matrix* p_data,int);
