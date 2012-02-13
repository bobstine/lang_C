include ../c_flags

###########################################################################
#
#        Options
#
#   Examples of depending on libaries
#        LDLIBS =  -lthing -lregression 
#        libs_used =  ../lib/libthing.a ../lib/libregression.a 
# 
###########################################################################


PROJECT_NAME = tools

OPT = -O3

# OPT = -O3 -DNDEBUG

USES = ranges utils random

# EXTERNAL_USES = gsl

level_0 = dummy.o

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
###########################################################################


# -------------------------------------------------------------------
# bellman recursion for competitive value
# -------------------------------------------------------------------

bellman.o: bellman.cc bellman.h

bellman_solver.o: bellman_solver.cc

bellman: bellman.o bellman_solver.o
	$(GCC) $^ $(LDLIBS) -o  $@

bellman_test: bellman
	./bellman --constrain --gamma 1.1 --rounds 25 --prob u --spend 0.5    # add   --write    for details


# ---  constrained    $^ are prereq    $@ is target    $* is stem
#      change n to change path, file names, and the length of run;  gp is gamma path

n = 1000
gp = gamma.$(n)/

#  $(gp)0.6 $(gp)0.65 $(gp)0.7 $(gp)0.75 $(gp)0.8 $(gp)0.85 $(gp)0.9 $(gp)0.95 $(gp)0.960 $(gp)0.970 $(gp)0.980 $(gp)0.990 $(gp)1.0 $(gp)1.05 $(gp)1.1 $(gp)1.15 $(gp)1.2 $(gp)1.3 $(gp)1.4 $(gp)1.5 $(gp)1.6 $(gp)1.7
#  $(gp)0.91 $(gp)0.92 $(gp)0.93 $(gp)0.94 $(gp)0.95 $(gp)0.96 $(gp)0.97 $(gp)0.98 $(gp)0.99
#  $(gp)0.952 $(gp)0.954 $(gp)0.956 $(gp)0.958  $(gp)0.951 $(gp)0.953 $(gp)2.0 $(gp)2.2

bellman_results.c$(n): bellman bellman.sh $(gp)0.6 $(gp)0.65 $(gp)0.7 $(gp)0.75 $(gp)0.8 $(gp)0.85 $(gp)0.9 $(gp)0.95 $(gp)0.960 $(gp)0.970 $(gp)0.980 $(gp)0.990 $(gp)1.0 $(gp)1.05 $(gp)1.1 $(gp)1.15 $(gp)1.2 $(gp)1.3 $(gp)1.4 $(gp)1.5 $(gp)1.6 $(gp)1.7
	cat $(filter $(gp)%,$^) >> $@

$(gp)%: bellman bellman.sh
	./bellman --constrain --spend 0.1 --gamma $* --rounds $(n) > $@


# ---  unconstrained
#   solutions for testing
#      2.5 25 1 u 0.5 -0.0332263 0.181596 0.0859291
#      2.5 10 1 u 0.5 -0.0651093 0.086978 0.0608351 	
bellman_results.u500: bellman bellman.sh  
	rm -f bellman_results.500
	./bellman.sh u 500 >  bellman_results.500
	./bellman.sh e 500 >> bellman_results.500
	./bellman.sh g 500 >> bellman_results.500


bellman_results.u1000: bellman bellman.sh
	rm -f bellman_results.1000
	./bellman.sh u 1000 >  bellman_results.1000
	./bellman.sh e 1000 >> bellman_results.1000
	./bellman.sh g 1000 >> bellman_results.1000


bellman_results.u2000: bellman bellman.sh
	rm -f bellman_results.2000
	./bellman.sh u 2000 >  bellman_results.2000
	./bellman.sh e 2000 >> bellman_results.2000
	./bellman.sh g 2000 >> bellman_results.2000


# -------------------------------------------------------------------
#   trend data
# -------------------------------------------------------------------

trendata_build_state.o: trendata_build_state.cc trendata.h

trendata_build_county.o: trendata_build_county.cc trendata.h

trendata_build_county: trendata_build_county.o trendata.h
	$(GCC) trendata_build_county.o $(LDLIBS) -o $@
	cp -f $@ ~/data/TrenData/county/build_data

trendata_build_state: trendata_build_state.o trendata.h
	$(GCC) trendata_build_state.o $(LDLIBS) -o $@
#	run it directly from C/tools directory rather than copy

trendata_build_nation: trendata_build_nation.o trendata.h
	$(GCC) trendata_build_nation.o $(LDLIBS) -o $@
	cp -f $@ ~/data/TrenData/nation/build_data


# generate 0/1 according to bernoulli process

bernoulli: bernoulli.o
	$(GCC) $^ $(LDLIBS)  -o  $@



# random CV indicator
random_indicator: random_indicator.o
	$(GCC) random_indicator.o $(LDLIBS) -o $@

doit: random_indicator
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s1772
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s345
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s5634
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s2639
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s4675


# Summary of scores on exam from scan sheet
process_608: process_grades
	/Users/bob/C/tools/process_grades -f/Users/bob/courses/stat608/grades/2010/ag238001.sdf -i29 -a38 -q44

process_grades: process_grades.o
	$(GCC) process_grades.o $(LDLIBS) -o $@



csv_to_table: csv_to_table.o
	$(GCC) csv_to_table.o $(LDLIBS) -o $@

cols_to_rows: columns_to_rows.o
	$(GCC) columns_to_rows.o $(LDLIBS) -o $@
	cp -f $@ ~/bin/ctor

rows_to_cols: rows_to_columns.o
	$(GCC) rows_to_columns.o $(LDLIBS) -o $@
	cp -f $@ ~/bin/rtoc

insert_column: insert_column.o
	$(GCC) $(CFLAGS) $(INCLUDES) insert_column.o $(LDLIBS) -o $@
	cp -f $@ ~/bin/insert_column

insert_column.test: insert_column test_data/test_stream.dat
	./insert_column -ftest_data/test_stream.dat  -o test_data/test_added.dat -y0 -x2 -r0.8
	cat test_data/test_added.dat

mands: mands.o
	$(GCC) mands.o $(LDLIBS) -o $@

cutt: cutt.o
	$(GCC) cutt.o $(LDLIBS) -o $@

###########################################################################

include ../rules_for_makefiles

