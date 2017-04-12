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

.PHONY: do_crsp


PROJECT_NAME = tools

OPT = -Ofast -mfpmath=sse -msse3 -m64 -march=native
# OPT = -O3
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
# read selected lines from std input
# -------------------------------------------------------------------

get_lines: get_lines.o
	$(GCC) $^ $(LDLIBS)  -o  $@



# -------------------------------------------------------------------
# random CV indicator
# -------------------------------------------------------------------

random_indicator: random_indicator.o
	$(GCC) random_indicator.o $(LDLIBS) -o $@

doit: random_indicator
	./random_indicator --header -n 10 -c 0.8 
	./random_indicator --header -n 10 -c 5 -s 353
	./random_indicator -p8 -n63 -c8 -b1 -s1772
	./random_indicator -p8 -n63 -c8 -b1 -s345
	./random_indicator -p8 -n63 -c8 -b1 -s5634
	./random_indicator -p8 -n63 -c8 -b1 -s2639
	./random_indicator -p8 -n63 -c8 -b1 -s4675


# -------------------------------------------------------------------
# Summary of scores on exam from scan sheet
# -------------------------------------------------------------------
process_608: process_grades
	./process_grades -f/Users/bob/courses/stat608/grades/2012/st234001.dat -o/Users/bob/courses/stat608/grades/2012/student_scores.txt -i29 -a45 -q50

process_grades: process_grades.o
	$(GCC) process_grades.o $(LDLIBS) -o $@

#							-n name column -i penn id -a answer column -q questions -v for mult versions
process_grades_test.out: process_grades  process_grades_test.txt
	./process_grades -fprocess_grades_test.txt -o $@ -n0 -i6 -a15 -q10 -v


# -------------------------------------------------------------------
#  Process movie data into 'real-estate' format
# -------------------------------------------------------------------

merge_movies: merge_movies.o
	$(GCC) $< $(LDLIBS) -o $@

# -------------------------------------------------------------------
#  Play with NAN for use as missing data code
# -------------------------------------------------------------------

missing_test: missing_test.o
	$(GCC) $< $(LDLIBS) -o $@


# -------------------------------------------------------------------
#  Process CRSP data from WRDS
# -------------------------------------------------------------------

crsp_path = ~/data/crsp/

stack_crsp: stack_crsp.o
	$(GCC) $< $(LDLIBS) -o $@

project_crsp: project_crsp.o
	$(GCC) $< $(LDLIBS) -o $@

spread_crsp: spread_crsp.o
	echo $(GCC)
	$(GCC) $< -o $@

spread_crsp_wprice: spread_crsp_wprice.o
	echo $(GCC)
	$(GCC) $< -o $@

#	map the WRDS output into a date x company table (full table is 2.44 million lines)
$(crsp_path)stock_table_wprice.tsv: spread_crsp_wprice $(crsp_path)monthly_stock_90_16_wprice.tsv
	rm -rf $@
	head -n 1000000 $(crsp_path)monthly_stock_90_16_wprice.tsv | cut -f 1-3,6-8 | ./$< > $@

$(crsp_path)stock_table.tsv: spread_crsp $(crsp_path)monthly_stock_90_16.tsv
	rm -rf $@
	head -n 100000 $(crsp_path)monthly_stock_90_16.tsv | cut -f 1-3,6 | ./spread_crsp > $@

do_crsp: $(crsp_path)stock_table_wprice.tsv


#	head of crsp data, filter, pull off complete lines
$(crsp_path)crsp_out.txt: stack_crsp $(crsp_path)crsp_in.txt
	rm -rf $@
	head -n 2500000 $(word 2,$^) | ./$<    | grep -e '^502'     >  $@

#	build random projection
nProj = 50

$(crsp_path)crsp_proj_$(nProj): project_crsp $(crsp_path)crsp_out.txt
	./$< -f $(word 2,$^) -o $@ -k $(nProj)  -p 4  -w
	touch $@            # remove empty target to rebuild files

# do_crsp: $(crsp_path)crsp_proj_$(nProj)
#	echo Done
 

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

# quadratic distance matrix between observations
distance_matrix: distance_matrix.o
	$(GCC) $(CFLAGS) $(INCLUDES) $^ $(LDLIBS) -o $@
	cp -f $@ ~/bin/$@

# random projections
random_projection: random_projection.o 
	$(GCC) $(CFLAGS) $(INCLUDES) $^ $(LDLIBS) -o $@
	cp -f $@ ~/bin/random_projection

# Arcene examples
arcene_data   = /Users/bob/data/Arcene/arcene_x.dat
arcene_kernel = /Users/bob/data/Arcene/arcene_kernel.dat

arcene_kernel: distance_matrix $(arcene_data)
	distance_matrix -s < $(arcene_data) > $(arcene_kernel)


n_projections = 100
linear        = test_data/arcene_linear_proj_$(n_projections)
quadratic     = test_data/arcene_quadratic_proj_$(n_projections)

$(linear)_%.dat : random_projection $(arcene_data)
	./random_projection -f $(arcene_data) -o $@ -d $(n_projections) --power_iterations $*

$(quadratic)_%.dat : random_projection $(arcene_data) 
	./random_projection -f $(arcene_data) -o $@ -d $(n_projections) --power_iterations $* --standardize --quadratic

arcene_linear_example: $(linear)_0.dat $(linear)_1.dat $(linear)_2.dat 
	echo "Done"

arcene_quad_example: $(quadratic)_0.dat $(quadratic)_1.dat $(quadratic)_2.dat
	echo "Done"


# means and SDs

mands: mands.o
	$(GCC) mands.o $(LDLIBS) -o $@

###########################################################################

include ../rules_for_makefiles

