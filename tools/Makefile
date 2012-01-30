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

bellman_solution: bellman
	./bellman --gamma 2.5 --write --rounds 10

# random CV indicator
random_indicator: random_indicator.o
	$(GCC) random_indicator.o $(LDLIBS) -o $@

doit: random_indicator
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s1772
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s345
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s5634
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s2639
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s4675


# the last option is dying...
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

