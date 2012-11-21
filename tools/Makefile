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

USES = utils random

# EXTERNAL_USES = boost  // not using the compiled libraries, just header

level_0 = dummy.o

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
###########################################################################



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


# -------------------------------------------------------------------
# generate 0/1 according to bernoulli process
# -------------------------------------------------------------------

bernoulli: bernoulli.o
	$(GCC) $^ $(LDLIBS)  -o  $@



# -------------------------------------------------------------------
# random CV indicator
# -------------------------------------------------------------------

random_indicator: random_indicator.o
	$(GCC) random_indicator.o $(LDLIBS) -o $@

doit: random_indicator
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s1772
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s345
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s5634
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s2639
	/Users/bob/C/tools/random_indicator -p8 -n63 -c8 -b1 -s4675


# -------------------------------------------------------------------
# Summary of scores on exam from scan sheet
# -------------------------------------------------------------------
process_608: process_grades
	./process_grades -f/Users/bob/courses/stat608/grades/2012/st234001.dat -o/Users/bob/courses/stat608/grades/2012/student_scores.txt -i29 -a45 -q50

process_grades: process_grades.o
	$(GCC) process_grades.o $(LDLIBS) -o $@


# -------------------------------------------------------------------
#  Manipulate tables
# -------------------------------------------------------------------

csv_to_table: csv_to_table.o
	$(GCC) csv_to_table.o $(LDLIBS) -o $@

cols_to_rows: columns_to_rows.o
	$(GCC) columns_to_rows.o $(LDLIBS) -o $@
	cp -f $@ ~/bin/ctor

rows_to_cols: rows_to_columns.o
	$(GCC) rows_to_columns.o $(LDLIBS) -o $@
	cp -f $@ ~/bin/rtoc

cutt: cutt.o
	$(GCC) cutt.o $(LDLIBS) -o $@


# -------------------------------------------------------------------
#  Statistical tools
# -------------------------------------------------------------------

# insert proxy of response into x's
insert_column: insert_column.o
	$(GCC) $(CFLAGS) $(INCLUDES) insert_column.o $(LDLIBS) -o $@
	cp -f $@ ~/bin/insert_column

insert_column.test: insert_column test_data/test_stream.dat
	./insert_column -ftest_data/test_stream.dat  -o test_data/test_added.dat -y0 -x2 -r0.8
	cat test_data/test_added.dat

random_projection: random_projection.o
	$(GCC) $(CFLAGS) $(INCLUDES) random_projection.o $(LDLIBS) -o $@
	cp -f $@ ~/bin/random_projection

random_projection.test: random_projection test_data/matrix.dat
	./random_projection --input /Users/bob/data/Arcene/arcene.dat  -o test_data/test_projections.dat --projections 100 --quadratic

mands: mands.o
	$(GCC) mands.o $(LDLIBS) -o $@

###########################################################################

include ../rules_for_makefiles

