// $Id: sparse_iterator.test.cc,v 1.1 2003/06/07 11:47:51 foster Exp $


#include "sparse_iterator.h"
#include <vector>
#include <utility>
#include <iterator>
#include <time.h>
#include <iostream>

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

//  STUFF USED FOR TESTING (stolen from iterators.test.cc)



class Operator : public std::unary_function<double,double> 
{
  double mX;
public:
  Operator(double x)
    : mX(x) { }
  
  double operator()(double x) const
  { return x + mX; }
  
  // virtual
  Operator& operator=(const Operator& op)
  { this->mX = op.mX+1; return *this; }
};


template<class Iter>
void
print_it(int n, Iter iter)
{
  for (int i=0; i<n; ++i, ++iter)
    std::cout << *iter << " ";
  std::cout << std::endl;
}

template <class Range>
inline typename range_traits<Range>::value_type
fast_accumulate(Range range, typename range_traits<Range>::value_type init)
{
  //      typename range_traits<Range>::next_type n(begin(range));
  typename range_traits<Range>::const_iterator::advance_to_next_non_zero next(begin(range));
  typename range_traits<Range>::const_iterator last = end(range);
  for( typename range_traits<Range>::const_iterator i = begin(range); i < last;next(i))
    init = init + *i;

  return init;
}


int
main()
{
  {

    typedef std::vector<std::pair<int,double> > m_type;
    typedef m_type::const_iterator ci_type;
    m_type m;
    m.push_back(std::make_pair(1,10.));
    m.push_back(std::make_pair(2,20.));
    m.push_back(std::make_pair(3,30.));
    m.push_back(std::make_pair(5,50.));
    m.push_back(std::make_pair(8,80.));
    m.push_back(std::make_pair(12,120.));
    m.push_back(std::make_pair(18,180.));
    m.push_back(std::make_pair(25,0));   // must be a sentinel for the last valid position.

    ci_type i = m.begin();
    const_sparse_iterator<int,double> j(m,0), dup(m);

    {

      std::cout << "strat of base vector:       ";
      print_it(20, j);
    }



    j.external_index();
    std::cout<< (*i).first << " " << j.external_index() << std::endl;
  
    std::cout << dup << std::endl;
    std::cout << j   << std::endl;
    std::cout << *dup << std::endl;
    std::cout << *j   << std::endl;
  
    dup += 1; std::cout << dup  << *(j+1) << std::endl;
    dup += 1; std::cout << dup << *(j+2) << std::endl;
    dup += 1; std::cout << dup << *(j+3) << std::endl;

    dup = const_sparse_iterator<int,double>(m);
    for (int j=0; j<25; ++j)
      {
	std::cout << j << " " << *dup << std::endl;
	++dup;
      }
    for (int j=24; j>0; --j)
      {
	--dup;
	std::cout << j << " " << *dup << std::endl;
      }
    copy(begin(m),end(m),std::ostream_iterator<double>(std::cout," "));

    std::cout << "\n\n";
    std::cout << "Trying out advance_to_next_non_zero() function." << std::endl;
    while(dup-begin(m) < 25)
      {
	std::cout << dup << std::endl;
	dup.advance_to_next_non_zero();
      };
    std::cout << "\n\n";
    std::cout << j << std::endl;
    std::cout << (j+4) << std::endl;
    const_sparse_iterator<int,double> k(j+4);
    std::cout << *(k+(-3)) << std::endl;
    std::cout << *(k+(-1)) << std::endl;
    std::cout << *(k) << std::endl;
    std::cout << *(k+1) << std::endl;
    std::cout << *(k+2) << std::endl;
    std::cout << *(k+3) << std::endl;
    std::cout << *(k+4) << std::endl;
    std::cout << *(k+5) << std::endl;

    std::cout << "Accumulating a sparse range: " << fast_accumulate(m,0.0) << std::endl;

    std::cout << "DONE" << std::endl;
  }
  {
    std::cout << "SPEED CHECK    SPEED CHECK    SPEED CHECK    SPEED CHECK" << std::endl;
    std::vector<double> raw(5000);
    double nReps = 20000.;
    int skip  = 10;

    // optimzation          size of raw vector     1/density   time ratio    sec/billion ops
    //                                              (skip)                  raw        sparse
    //
    //   NONE                    50M                100           22         52          2.2*
    //   -O2                     50M                100            1         .6          .6*
    //   -O4                     50M                100           23         15          .6*

    //   NONE                    500000             100           35         52           1.4
    //   -O2                     500000             100           2.7        .6          .21
    //   -O4                     500000             100           50         15          .3*
    //
    //   NONE                    5000                10            1.4       28          19*
    //   -O2                     5000                10            .27       .6          2.2
    //   -O4                     5000                10            1         3.2         2.9*
    //
    //   NONE                    5000                 7            1         28          27*
    //
    //  * = before improved advance_to_next_non_zero() was implemented
    //   -O6 is all the same as -O4

    for(int i = 0; i < int(raw.size()); i += skip) 
      raw[i] = i;
    std::vector<std::pair<int,double> > sparse;
    copy(raw.begin(),raw.end(),sparse_back_insert_iterator<double>(sparse));
    std::cout << "sparse size: " << sparse.size() << std::endl;
    std::cout << "sparse begin: " << begin(sparse) << std::endl;
    std::cout << "sparse end: " << end(sparse) << std::endl;
    // the following confirms that we got things right
    //    for(const_sparse_iterator<int,double> i = begin(sparse); i < end(sparse); i.advance_to_next_non_zero())
    //      std::cout << i << std::endl;
    std::cout<< std::endl;

    //    clock_t start = clock();
    //    for (int j=0; j < nReps; ++j)
    //      accumulate(raw,0.0);
    //    double raw_time = double(clock() - start)/CLOCKS_PER_SEC;
    //    std::cout << "second  " << raw_time/10000. << std::endl;
    //    std::cout << "clicks  " << raw_time/100. << std::endl;
    //    std::cout << "Sec/billion  ops  raw  " << 100000. * raw_time/(nReps * raw.size()) << std::endl;

    clock_t start = clock();
    for (int j=0; j < nReps; ++j)
      fast_accumulate(sparse,0.0);
    double sparse_time = double(clock() - start)/CLOCKS_PER_SEC;
    std::cout << "Sec/billion ops sparse " << 100000. * sparse_time/(nReps * raw.size()) << std::endl;
    //    std::cout << "Ratio = " << raw_time / sparse_time << std::endl;
  }

}
