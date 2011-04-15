include ../c_flags

default: auction.test

../c_flags: 
	ln -s ~/dot_files/c_flags.pb ../c_flags


###########################################################################
#
#        Options
# 
###########################################################################

PROJECT_NAME = auction

USES =  eigen spline ranges random utils 

EXTERNAL_USES = boost_thread

# OPT = 

###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = featureABC.o      bidders.o      light_threads.o
level_1   = features.o      feature_transformations.o
level_2   = feature_predicates.o
level_3   = feature_iterators.o      adapter.o
level_4   = feature_streams.o
level_5   = experts.o
level_6   = auction.o

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
############################################################################
include ../rules_for_makefiles


logistic_test: auction.test
	./auction.test -f test/logistic_test.dat -r 10 -c 0 -a 2

data/%.dat: data/%.csv
	rm -rf $@
	/Users/bob/C/boost/csv_parser < $<  > $@

baseball_test: auction.test data/baseball.dat
	./auction.test -f data/baseball.dat -p 1 -r 300 -c 5

crash_test: auction.test
	./auction.test -f data/crash.dat -r 100 -c 5


# ---  credit models  -----------------------------------------------------------------------------

# Set options to auction: -r rounds  -p protection from overfitting  -c calibration   -x initial exclude   -b block size  -d debug output level
#   block size (-b) is the number of eligible counties in credit (0 means use ols estimates, 1 implies white, larger means corr blocked)
#   exclude (-x) is a multiple of the number of eligible counties (eg, 7 times)
# Build can use a pre-build data file constructed like this
#	 sh index.sh > data
#   or read data from a pipe like this if the expanded data file (constructed from manifest) is compressed
#	 gunzip -c data/credit/auction.data/short.gz | ./auction.test -o data/credit/ -r 50 -a 0.5 -p 3 -b 3000 -c 0 -x 9000
#

penn_credit_test: auction.test
	 ./auction.test -f data/credit/auction_pa.data/data -o data/credit/ -r  100 -a 1.5 -p 3 -b 67 -c 3 -x 469 -d 3 -s 1
#	 ./auction.test -f data/credit/auction_pa.data/data -o data/credit/ -r  200 -a 0.5 -p 3 -b 67 -c 3 -x 469 -d 3 -s 1


short_credit_test: auction.test
	 ./auction.test -f data/credit/auction.data/short.dat -o data/credit/ -r 100 -a 0.5 -p 3 -b 2886 -c 3 -x 20202 -d 3

credit_test: auction.test
	 ./auction.test -f data/credit/auction.data/data      -o data/credit/ -r 500 -a 0.5 -p 3 -b 2886 -c 3 -x 20202 -d 3

short_credit_%: auction.test short_cv%.data
	./auction.test -f data/credit/auction.data/short.dat -o data/credit/ -r 400 -a 0.5 -p 3 -b 2886 -c 3 -x 20202 -d 3
	cd $(resultpath); rm -f short_ss$*.csv; cp progress.csv short_ss$*.csv

credit_%: auction.test cv%.data
	./auction.test -f data/credit/auction.data/data -o data/credit/ -r 1000 -a 0.5 -p 3 -b 2886 -c 3 -x 20202 -d 3
	cd $(resultpath); rm -f ss$*.csv; cp progress.csv ss$*.csv

#            random seeds for generating cv files that define the runs
seed1 = 4675
seed2 = 345
seed3 = 5634
seed4 = 2639
seed5 = 3154

datapath = /Users/bob/C/auctions/data/credit/auction.data/
resultpath = /Users/bob/C/auctions/data/credit/

cv%.indicator: 
	/Users/bob/C/tools/random_indicator -p8 -n62 -c54 -b2886 -s$(seed$*) > $(datapath)cv.indicator\[in\]

cv%.indicator: 
	/Users/bob/C/tools/random_indicator -p8 -n62 -c54 -b1 -s$(seed$*) > cv$*.indicator

cv%.data: 
	/Users/bob/C/tools/random_indicator -p8 -n62 -c54 -b2886 -s$(seed$*) > $(datapath)cv.indicator\[in\]
	cd $(datapath); rm -f data; ./index.sh > data

short_cv%.data: 
	/Users/bob/C/tools/random_indicator -p8 -n62 -c54 -b2886 -s$(seed$*) > $(datapath)cv.indicator\[in\]
	cd $(datapath); rm -f short.dat; ./short.sh > short.dat


# ---  ann arbor models  -----------------------------------------------------------------------------

anes_run: auction.test
	./auction.test -d 3 -f data/anes/anes2008_data/data -o data/anes/ -r 1000 -a 0.05 -p 3 -c 0 -s 1



# ---  dean text data models  ----------------------------------------------------------------------
#                         Note: run the index file to put results into a file named "data"
#

theher_run: auction.test
	./auction.test -d 2 -f data/the_her/n10000/data -o data/the_her/ -r 1000 -a 0.05 -p 3 -c 0


text_run: auction.test
	 ./auction.test -f data/text/auction.data/data -o data/text/ -r 10000 -a 0.25 -p 3 -k 500 -c 10 -d 0 -b 1 -s 1




# ---  miscellaneous test applications  -------------------------------------------------------------

crunch_test: auction.test data/crunch.dat
	./auction.test -f data/crunch.dat -r 1000 -a 50 -p 0 -c 0

jason_dat: ../boost/csv_parser data/jason_97.csv
	rm -rf data/jason.tar; mkdir data/jason.tar
	../boost/csv_parser -f data/jason_97.csv -d data/jason.tar
	cd data/jason.tar; chmod +x index.sh; ./index.sh > jason.dat

jason_test: auction.test data/jason.tar/index.sh
	 ./auction.test -f data/jason.tar/jason.dat -o data/jason.tar/ -r 100 -a 2 -p 3 -c 5




lag_test: auction.test
	./auction.test -f data/lagtest.txt -o test/log/ -r 100 -a 0.5 -p 3 -b 5 -c 0 -x 50

wiki_test: auction.test 
	./auction.test -f ../wiki/wiki.sdata -o ../wiki/auction_output/ -r 1000 -a 1 -p 3 -c 0

stop: auction.test.o
	echo "Stopping"



model/costs: model/test.dat model/auction.model estimator validator
	./estimator -f test/test.dat est2.dat -n test/bank_post45.names -m model/auction.model -o model/refit.model
	./validator -f test/bank_post45.rows -n test/bank_post45.names -m model/auction.model -o model/costs

model/auction.model model/test.dat: auction.test.exec 
	./auction.test -f test/bank_post45.dat -r 100 -c 5
	cat test/bank_post45.n_rows test/bank_post45.rows > model/test.dat

c45_parser:
	-rm -f c45_parser
	cp ../seq_regr/c45_syntax.test.exec c45_parser

quicktest.o: quicktest.cc
	$(GCC) $(CFLAGS) -c quicktest.cc -o $@

quicktest: quicktest.o
	$(GCC) $^ $(LDLIBS) -o  $@
	./quicktest

expander.o: expander.cc
	$(GCC) $(CFLAGS) -c expander.cc -o $@

expander: expander.o
	$(GCC) $^ $(LDLIBS) -o  $@

validate.o: validate_model.cc $(above_infinity)
	$(GCC) $(CFLAGS) -c validate_model.cc -o $@

validator: validate.o $(above_infinity)
	@echo Making executable
	$(GCC) $^ $(LDLIBS) -o  $@

estimate.o: estimate_model.cc $(above_infinity)
	$(GCC) $(CFLAGS) -c estimate_model.cc -o $@

estimator: estimate.o $(above_infinity)
	@echo Making executable
	$(GCC) $^ $(LDLIBS) -o  $@

#---------------------------------------------
### Merged files with common over-sampling
# 						   (1.25 min / 25,000, gosset O2)

big%.names:
	ln -s data/bank.names $@

small%.names:
	ln -s data/bank.names $@

big%.data: big%.names
	head -n 100000 data/ok.$* | cat data/br.$* - > $@

small%.data: small%.names
	head -n  50000 data/ok.$* | cat data/br.$* - > $@

# merge%.dat in transposed, streaming format

merge%.dat: big%.data big%.names small%.data small%.names
	-rm -f data/merge$*.*
	c45_parser --input=big$* -n           > data/merge$*.names
	c45_parser --input=big$* -v           > data/merge$*.rows
	c45_parser --input=big$* --raw-data   > data/$@              # can use small here and re-estimate
	ln -s data/$@ $@

# *.rows is row-oriented, transposed format

est%.rows: data/merge%.rows
	-rm -f est$*.rows
	grep e$* data/counts | cut -f 2 | cat - data/merge$*.rows > $@

val1.rows:
	-rm -f val1.rows
	grep v1 data/counts | cut -f 2 | cat - data/merge2.rows data/merge3.rows data/merge4.rows data/merge5.rows > $@

val2.rows:
	-rm -f val2.rows
	grep v2 data/counts | cut -f 2 | cat - data/merge1.rows data/merge3.rows data/merge4.rows data/merge5.rows > $@

val3.rows:
	-rm -f val3.rows
	grep v3 data/counts | cut -f 2 | cat - data/merge1.rows data/merge2.rows data/merge4.rows data/merge5.rows > $@

val4.rows:
	-rm -f val4.rows
	grep v4 data/counts | cut -f 2 | cat - data/merge1.rows data/merge2.rows data/merge3.rows data/merge5.rows > $@

val5.rows:
	-rm -f val5.rows
	grep v5 data/counts | cut -f 2 | cat - data/merge1.rows data/merge2.rows data/merge3.rows data/merge4.rows > $@


# .INTERMEDIATE: est3.rows val3.rows
# use -c 5 for calibrating models.

run%: auction.test.exec validator estimator merge%.dat est%.rows val%.rows
	-rm -f $@
	auction.test.exec -f merge$*.dat -r 800 -p model$* -c 5 > $@
	cat est$*.rows | estimator -n data/merge$*.names -m model$*/auction.model -o model$*/updated.model>> $@
	rm -f model$*/costs
	cat val$*.rows | validator -n data/merge$*.names -m model$*/updated.model -o model$*/costs >> $@
	rm -f model$*/$@
	cp $@ model$*/
	cat model$*/costs

cost%: validator val%.rows
	rm -f model$*/costs
	cat val$*.rows | validator -n data/merge$*.names -m model$*/auction.model -o model$*/costs
	cat model$*/costs
