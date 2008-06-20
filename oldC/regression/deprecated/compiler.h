// Code removed from variable.h

/*
  This limit controls how deep the compiler can try to go.  The limit
  must be at least 3 in order to compile a binary function (which
  generates an input parse list with 3 items... two variables and the
  function itself.
*/

const unsigned int compiler_step_limit(3);

/*
  The nil class is used to initialize the stack in the variable sum
  compiler. So it must look like a function and be able to answer the
  sort of questions that recursive pairs get.
*/

namespace
{
  class NIL : public std::unary_function<Observation, double>        // signals end of type list for compiler
    {
    public:
      typedef NIL first_type;
      typedef NIL second_type;
      double operator()(Observation const& obs) const           // kludge for compiling like a pair, operator
	{ assert("this function ought never be called" == 0);
	  return 0.0 * obs.size();
	}
    };
  template <class T1, class T2>
    T1 First (std::pair<T1,T2> x)
    {
      return x.first;
    };
  NIL First(NIL x)
    { return x; }

  template <class T1, class T2>
    T2 Second (std::pair<T1,T2> x)
    {
      return x.second;
    };
  NIL Second(NIL x)
    { return x; }
}

///////////////////////  Sum Compiler  ////////////////////////////////

template <int i, class T>
  class variable_sum_compiler
{
 public:
  
  template<class R>
    std::pair<double,bool>  sum(std::list<Variable> lst, T compiled_stack, R range);
};

template <class T>
class variable_sum_compiler<0,T>
{
 public:
  
  template<class R>
    std::pair<double,bool>  sum(std::list<Variable> lst, T compiled_stack, R range);
};

////////////////////  Accumulation compilation  ////////////////////////////////

/*
  The 'compiler' works by transforming the input list of variables
  into a composition of functions, which is gradually constructed in
  the type_stack argument.  The type stack *is* the compiled version
  of the input parsed list of variables.  It is not long, but acts as
  a stack to hold intermediate stages of compilation.  For example, it
  will hold two index functions when parsing a binary variable until
  the binary var itself gives up its funtion.
*/
/*
  template <int step, class T>
template <class R>
std::pair<double,bool>
variable_sum_compiler<step,T>::sum(std::list<Variable> lst, T compiled_stack, R range)
{
  using boost::compose_f_gx   ; using boost::compose_f_gx_t;
  using boost::compose_f_gx_hx; using boost::compose_f_gx_hx_t;
  using Function_Utils::Index;
  using Function_Utils::Square;

  if (lst.size() == 0)  // typelist has it all accumulated
    return std::make_pair(range_ops::accumulate(make_unary_range(First(compiled_stack), range), 0.0), true);
  else  // more to parse
  { Variable head = lst.front();
    // std::clog << "  ### compiling: lst.size()=" << lst.size() << " with head " << head << std::endl;
    lst.pop_front();
    if (head.type_matches(typeid(IndexVariable)))
    { std::clog << "  ** index conversion **  " << std::endl;
      int varIndex = *(head.base_indices().begin());
      return variable_sum_compiler<step-1,
	                           std::pair<Index<Observation,double>,T> >()
	.sum(lst,
	    std::make_pair(Index<Observation,double>(varIndex),
			   compiled_stack),
	    range);
    }
    else if (head.type_matches(typeid(TransformedVariable<Square>)))
    { std::clog << "  ** square conversion **  " << std::endl;
      return variable_sum_compiler<step-1,
	                           std::pair<compose_f_gx_t<Square,typename T::first_type>,
	                                     typename T::second_type > >()
	.sum(lst,
	    std::make_pair(compose_f_gx(Square(),First(compiled_stack)),
			   Second(compiled_stack)),
	    range);
    }
    else if (head.type_matches(typeid(BinaryVariable<std::multiplies<double> >)))
    { std::clog << "  ** product conversion **  " << std::endl;
      return variable_sum_compiler<step-1,
	                           std::pair<compose_f_gx_hx_t<std::multiplies<double>,
	                                                       typename T::first_type,
                                         	               typename T::second_type::first_type >,
                                      	     typename T::second_type::second_type > >()
	.sum(lst,
	     std::make_pair(compose_f_gx_hx(std::multiplies<double>(),First(compiled_stack),First(Second(compiled_stack))),
			    Second(Second(compiled_stack))),
	     range);
    }
    else
    { std::clog << "  ** fall back to recursive accumulation **  " << std::endl;
      return std::make_pair(0.0,false);
    }
  }
}

template<class T>
template<class R>
std::pair<double,bool>
variable_sum_compiler<0,T>::sum(std::list<Variable> lst, T compiled_stack, R range)
{
  assert(lst.size()==0);
  return std::make_pair(range_ops::accumulate(make_unary_range(First(compiled_stack), range),0.0), true);
}


  // functions related to compiling virtual via templates
  // belongs in class Variable
  template<class Iter>
    double
    sum(range<Iter> rng)
    {
      using range_ops::accumulate;
      std::list<Variable> contents = parse_list();
      
      if(contents.size() > compiler_step_limit) // use virtual evaluation
      { std::clog << "Size " << contents.size() << " exceeds step limit; using virtual sum." << std::endl;
	return accumulate(make_unary_range(*this, rng), 0.0);
      }
      else // try to compile
      {
	std::clog << "Size " << contents.size() << " of content list allows compiling." << std::endl;
	std::copy(contents.begin(), contents.end(), std::ostream_iterator<Variable>(std::clog,","));
	std::clog << std::endl;
	//  initial compiled stack has to match nested parse of 3 elements for binary function
	std::pair<double,bool> result (variable_sum_compiler <compiler_step_limit, NIL>()
				       .sum(contents,
					    NIL(),
					    rng));
	if (result.second == true)      // compiled function worked
	  return result.first;
	else                            // could not compile, fall back to virtual
	{ std::clog << "Compiler failed; falling back to virtual." << std::clog;
	  return accumulate(make_unary_range(*this, rng), 0.0);
	}
      }
    }

