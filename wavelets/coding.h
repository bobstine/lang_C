// $Id: coding.h,v 1.2 2000/04/21 16:41:28 bob Exp $-*- c++ -*-
#ifndef _coding_
#define _coding_

#include <vector.h>

int          binary_bit_length (int j);
vector<bool> binary_bits (int j);

int   signed_cauchy_bit_length (int j);
int          cauchy_bit_length(int j);
vector<bool> cauchy_bits (int j);

int   signed_geometric_bit_length (int j);
int          geometric_bit_length (int j);
vector<bool> geometric_bits (int j);

int   signed_universal_bit_length(int j);
int          universal_bit_length(int j);
vector<bool> universal_bits(int j);

double       monotone_universal_length(int j);

double log_2 (const double x);
double log_star (const double x);

double entropy (const int n, const double p);
double relative_entropy (const double est, const double z);

int compressed_bit_length(double p, int n);
int compressed_bit_length(int x, int n);
int compressed_bit_length(const vector<bool> &b);

int optimal_block_bit_length (const vector<int> &x);

int normal_data_bit_length (const double ss);

#endif

//////////////////////// eof ///////////////////////////////
