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

OPT = -O2

# OPT = -O3 -DNDEBUG

USES = ranges utils random

# EXTERNAL_USES = gsl

level_0 = dummy.o

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
###########################################################################

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



bernoulli: bernoulli.o
	$(GCC) $^ $(LDLIBS)  -o  $@


# bellman recursion for competitive value

bellman: bellman.o
	$(GCC) $^ $(LDLIBS) -o  $@

bellman_solution: bellman    # ans g=2.5, s=0.5 u n=10 is -0.0651093 0.0869784 0.0608351 
	./bellman --gamma 2.5 --rounds 10 --prob u --spend 0.5 --write 

bellman_results.500: bellman bellman.sh
	rm -f bellman_results.500
	./bellman.sh u 500 >  bellman_results.500
	./bellman.sh e 500 >> bellman_results.500
	./bellman.sh g 500 >> bellman_results.500


bellman_results.1000: bellman bellman.sh
	rm -f bellman_results.1000
	./bellman.sh u 1000 >  bellman_results.1000
	./bellman.sh e 1000 >> bellman_results.1000
	./bellman.sh g 1000 >> bellman_results.1000


bellman_results.2000: bellman bellman.sh
	rm -f bellman_results.2000
	./bellman.sh u 2000 >  bellman_results.2000
	./bellman.sh e 2000 >> bellman_results.2000
	./bellman.sh g 2000 >> bellman_results.2000




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

