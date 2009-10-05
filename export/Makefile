# preliminaries

all: csv_parser build_model

.PHONY : clean 

clean:
	-rm -f     *.o *.d *.dd *.exec lib*.a
	-rm        core


LIBTOOL_CMD = libtool  -static -o $@ $(above_infinity)
CFLAGS = -g -O3 -Wall -USINGLE -W -Woverloaded-virtual -fmessage-length=0
GCC = g++ -DNDEBUG $(INCLUDES) $(LDFLAGS) 
CC  = g++ -DNDEBUG $(INCLUDES) $(LDFLAGS)

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

utils = debug.o read_utils.o stat_utils.o function_utils.o print_utils.o operator_traits.o file_utils.o data_registry.o bennett.o coding.o cyclic_iterator.o

random = random.o

spline = smoothing_spline.o linpack.o

ranges = range.o range_traits.o range_ops.o range_stats.o anonymous_iterator.o evaluator.o column.o composer.o function_iterators.o

gsl = gsl_utils.o gsl_eigen.o gsl_engine.o gsl_iterator.o gsl_regr.o gsl_data.o gsl_model.o

auction = featureABC.o feature_streams.o my_features.o experts.o bidders.o adapter.o build_model.o auction.o


all: csv_parser build_model


csv_parser: csv_parser.cc
	g++ -I.  -I /usr/local/include -g -Wall -USINGLE -W -Woverloaded-virtual -O2 -fmessage-length=0 -c csv_parser.cc -o csv_parser.o 
	g++ csv_parser.o  -lboost_regex-xgcc40-mt-1_38 -lm  -o csv_parser
#    For linux change the above line to the following:
#	g++ csv_parser.o  -lboost_regex -lm  -o csv_parser


build_model : $(utils) $(random) $(spline) $(ranges) $(gsl) $(auction) 
	$(GCC) $^ $(LDLIBS) -o  $@

test: csv_parser build_model
	./csv_parser --input-file test/input_data.csv --output-file test/model.dat
	mkdir -p test/log
	./build_model --input-file test/model.dat --output-path test/log/ -r 200 -p 0 -a 0.05


crunch/model.dat: csv_parser crunch/input.csv
	./csv_parser --input-file crunch/input.csv --output-file crunch/model.dat

crunch: build_model crunch/model.dat
	mkdir -p crunch/log
	./build_model --input-file crunch/model.dat --output-path crunch/log/ -r 10000 -p 0 -a 15


# h option tars the files rather than the symbolic links
build_model.tgz: *.h *.cc README.txt test/input_data.csv
	rm -f build_model.tgz
	tar --transform 's,^,auction/,' -cvzhf build_model.tgz *.h *.cc README.txt test/input_data.csv Makefile