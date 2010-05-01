include ../c_flags

###########################################################################
#
#        Options
# 
###########################################################################

PROJECT_NAME = auction

# OPT      = -O3 -DNDEBUG
# OPT      = -O2

USES = gsl_tools spline ranges random utils 

EXTERNAL_USES = gsl gslcblas

###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = featureABC.o       bidders.o
level_1   = features.o
level_2   = adapter.o          feature_streams.o
level_3   = experts.o
level_4   = auction.o

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


anes_test: auction.test data/anes2008.dat
	./auction.test -f data/anes2008.dat -r 2500 -c 5

baseball_test: auction.test data/baseball.dat
	./auction.test -f data/baseball.dat -p 1 -r 300 -c 5

crash_test: auction.test
	./auction.test -f data/crash.dat -r 100 -c 5

credit_test: auction.test
#	./auction.test -f data/credit/credit_all.txt -o data/credit/ -r 250 -a 0.5 -p 3 -b 3000 -c 0 -x 9000
	cat data/credit/credit.txt data/credit/credit_nation.txt | ./auction.test -o data/credit/ -r 250 -a 0.5 -p 3 -b 3000 -c 0 -x 9000

crunch_test: auction.test data/crunch.dat
	./auction.test -f data/crunch.dat -r 1000 -a 50 -p 0 -c 0

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
