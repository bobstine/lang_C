# preliminaries

LIBTOOL_CMD = libtool  -static -o $@ $(above_infinity)
CFLAGS = -g -Wall -USINGLE -W -Woverloaded-virtual $(OPT) -fmessage-length=0
GCC = g++ $(INCLUDES) $(LDFLAGS) 
CC  = g++ $(INCLUDES) $(LDFLAGS)

%.o: %.cc %.h
	$(GCC) $(CFLAGS) -c $< -o $@ 
%.o: %.cc
	$(GCC) $(CFLAGS) -c $< -o $@ 

%.test: %.test.o %.o
	@echo Includes are: $(INCLUDES)
	@echo Load libs are: $(LDLIBS)
	@echo Making executable test program. $@
	$(GCC) $^ $(LDLIBS) -o  $@


EXTERNAL_USES = gsl gslcblas

LDLIBS = $(patsubst %,-l%,$(USES)) $(patsubst %,-l%,$(EXTERNAL_USES)) -lm 

####################

utils = read_utils.o stat_utils.o function_utils.o print_utils.o operator_traits.o file_utils.o data_registry.o bennett.o coding.o cyclic_iterator.o

random = random.o

spline = smoothing_spline.o linpack.o

ranges = range.o range_traits.o range_ops.o range_stats.o anonymous_iterator.o evaluator.o column.o composer.o function_iterators.o

gsl = gsl_utils.o gsl_eigen.o gsl_engine.o gsl_iterator.o gsl_regr.o gsl_data.o gsl_model.o

auction = featureABC.o feature_streams.o my_features.o experts.o bidders.o adapter.o build_model.o auction.o

csv_parser: csv_parser.cc
	g++ -I.  -I /usr/local/include -g -Wall -USINGLE -W -Woverloaded-virtual -O2 -fmessage-length=0 -c csv_parser.cc -o csv_parser.o 
	g++ csv_parser.o  -lboost_regex-xgcc40-mt-1_38 -lm  -o csv_parser

build_model : $(utils) $(random) $(spline) $(ranges) $(gsl) $(auction) 
	$(GCC) $^ $(LDLIBS) -o  $@


# h option tars the files rather than the symbolic links
auction.tgz: *.h *.cc
	rm -f auction.tgz
	tar -cvzhf auction.tgz *.h *.cc
