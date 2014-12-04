include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

PROJECT_NAME = boost

OPT = -O2 -std=c++0x     # -DNDEBUG  this option kills benefit of assert

USES = utils 

EXTERNAL_USES = boost_regex boost_thread

###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = csv_parser.o  regex.o
level_1   =

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
############################################################################

hello:	hello.o
	$(GCC) $<  -o $@
	echo 1 2 3 | ./hello

lambda:	lambda.o
	$(GCC) $<  -o $@
	./lambda 

regex: regex.o
	$(GCC) $< $(LDLIBS) -o $@
	echo "subject: these\n" "from joe\n" "help\n" "Subject: Re:test message\n"| ./regex


csv_parser_demo: csv_parser_demo 
	$(GCC) $< $(LDLIBS) -o $@
	./csv_parser_demo


csv_parser: csv_parser.o
	$(GCC) $< $(LDLIBS) -o $@


# writes variables to separate files within indicated subdirectory
test_parser_dir: csv_parser csv_test_data.txt
	mkdir -p csv.out
	rm csv.out/*; ./csv_parser -f csv_test_data.txt -d csv.out

# writes variables to one file
test_parser_file: csv_parser csv_test_data.txt
	rm csv.out/*; ./csv_parser -f csv_test_data.txt -o csv_test_out.txt

parse_diabetes: csv_parser
	rm diab.out/*; ./csv_parser -f ~foster/diab_smaller.csv -d diab.out
#	rm diab.out/*; ./csv_parser -f diabetes_test.csv -d diab.out

test_parser_data: csv_parser.o test-data.csv
	$(GCC) $< $(LDLIBS) -o $@
	./csv_parser -o model.dat -f test-data.csv


###########################################################################

include ../rules_for_makefiles

