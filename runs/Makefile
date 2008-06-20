# $Id: Makefile,v 3.9 2006/03/22 20:22:28 foster Exp $ 

#
#  The following command will fire off a parallel make on the grid computers:
#
#                   qmake -cwd -v PATH -pe make 10 -- -k all5forward
#
#                   NOTE: qmake needs commands to be all on one line
#                         or it will get confused as to order of execution.
#
#  To avoid stuff getting deleted run:
#
#		make -n target | tail -1
#
#  Now shove all these to-be-deleted files in a .SECONDARY and they will be saved.
#


############################################################################
#									   #
#            			TARGET	 				   #
#									   #
############################################################################


all: all5 all10

fast: digits

add2: add2.5forward.summary add2.10forward.summary

add: add.5forward.summary add.10forward.summary

digits: all.digits/digit0.5forward.summary \
        all.digits/digit1.5forward.summary \
        all.digits/digit2.5forward.summary \
        all.digits/digit3.5forward.summary \
        all.digits/digit4.5forward.summary \
        all.digits/digit5.5forward.summary \
        all.digits/digit6.5forward.summary \
        all.digits/digit7.5forward.summary \
        all.digits/digit8.5forward.summary \
        all.digits/digit9.5forward.summary

junwen: junwen.5forward.summary


all5: rehg.5forward.summary rehg.5reverse.summary all_nips_5forward all_nips_5reverse 

all5forward: rehg.5forward.summary all_nips_5forward

all10: rehg.10forward.summary rehg.10reverse.summary

# the following might be best made by "make" and then having everything else built by "qmake"

dirs: dir5 dir10

dir5:  arcene.dir/5folds/5_folds_made dexter.dir/5folds/5_folds_made dorothea.dir/5folds/5_folds_made\
       gisette.dir/5folds/5_folds_made madelon.dir/5folds/5_folds_made rehg.dir/5folds/5_folds_made


dir10:  arcene.dir/10folds/10_folds_made dexter.dir/10folds/10_folds_made dorothea.dir/10folds/10_folds_made\
       gisette.dir/10folds/10_folds_made madelon.dir/10folds/10_folds_made rehg.dir/10folds/10_folds_made

all_nips_5forward: arcene.5forward.summary \
                   dexter.5forward.summary \
                   dorothea.5forward.summary \
                   gisette.5forward.summary \
                   madelon.5forward.summary \
		   catalysis.5forward.summary

all_nips_5reverse: arcene.5reverse.summary \
                   dexter.5reverse.summary \
                   dorothea.5reverse.summary \
                   gisette.5reverse.summary \
                   madelon.5reverse.summary \
		   catalysis.5reverse.summary

clean:
	@echo 
	@echo "This is so dangorious! That you must type 'make MR_clean'"
	@echo 
	@echo "Mr_clean will blow away all executable AND all data."
	@echo 

mr_clean Mr_clean MR_clean:
	@touch foo.dir foo.summary
	-rm -rf `cat .cvsignore`
	-rm *summary
	-rm *snapshot

############################################################################
#									   #
#            BUILTING PARAMETER FILE					   #
#									   #
############################################################################

Makefile.parameters:
	@(echo "# Feel free to edit this file. (Just don't check it into cvs.) -*- Makefile -*-";\
          egrep -v "DON.T EDIT|Id" Makefile.parameters.default) > Makefile.parameters

-include Makefile.parameters

############################################################################
#									   #
#            BUILDING DIFFERENT RANDOMIZATION FOLDS			   #
#									   #
############################################################################

# The following is currently un-tested

A: rehg.A.10forward.summary rehg.A.5forward.summary

B: rehg.B.10forward.summary rehg.B.5forward.summary

%.A.dir/raw_data: %.dir/raw_data
	-mkdir $(@D); ln -s ../$*.dir/raw_data $@

%.B.dir/raw_data: %.dir/raw_data
	-mkdir $(@D); ln -s ../$*.dir/raw_data $@

%.A.dir/svd_data.$(NUMBER_SVD_VARIABLES): %.dir/svd_data.$(NUMBER_SVD_VARIABLES)
	-mkdir $(@D); ln -s ../$*.dir/svd_data.$(NUMBER_SVD_VARIABLES) $@

%.B.dir/svd_data.$(NUMBER_SVD_VARIABLES): %.dir/svd_data.$(NUMBER_SVD_VARIABLES)
	-mkdir $(@D); ln -s ../$*.dir/svd_data.$(NUMBER_SVD_VARIABLES) $@

%.A.dir/random_svd_data.$(NUMBER_RANDOM_SVD_VARIABLES): %.dir/random_svd_data.$(NUMBER_RANDOM_SVD_VARIABLES)
	-mkdir $(@D); ln -s ../$*.dir/random_svd_data.$(NUMBER_RANDOM_SVD_VARIABLES) $@

%.B.dir/random_svd_data.$(NUMBER_RANDOM_SVD_VARIABLES): %.dir/random_svd_data.$(NUMBER_RANDOM_SVD_VARIABLES)
	-mkdir $(@D); ln -s ../$*.dir/random_svd_data.$(NUMBER_RANDOM_SVD_VARIABLES) $@

############################################################################
#									   #
#            SPECIFIC DATA MANIPULATIONS				   #
#									   #
############################################################################

rehg.dir/raw_data: data/rehg/DataForUPenn.txt convert_rehg
	-mkdir $(@D); \
        ./convert_rehg < $< > $@

catalysis.dir/raw_data:  data/NIPS2004/CATALYSIS/catalysis_train.targets data/NIPS2004/CATALYSIS/catalysis_train.inputs convert_nips_2004
	-mkdir $(@D) ;\
	./convert_nips_2004 617 data/NIPS2004/CATALYSIS/catalysis_train > $@

arcene.dir/raw_data:  data/NIPS2003/ARCENE/arcene_train.labels data/NIPS2003/ARCENE/arcene_train.data convert_nips
	-mkdir $(@D) ;\
	./convert_nips dense 10000 data/NIPS2003/ARCENE/arcene_train > $@

dexter.dir/raw_data:  data/NIPS2003/DEXTER/dexter_train.labels data/NIPS2003/DEXTER/dexter_train.data convert_nips
	-mkdir $(@D) ;\
	./convert_nips sparse 20000 data/NIPS2003/DEXTER/dexter_train > $@

dorothea.dir/raw_data:  data/NIPS2003/DOROTHEA/dorothea_train.labels data/NIPS2003/DOROTHEA/dorothea_train.data convert_nips
	-mkdir $(@D) ;\
	./convert_nips binary 100000 data/NIPS2003/DOROTHEA/dorothea_train > $@

gisette.dir/raw_data:  data/NIPS2003/GISETTE/gisette_train.labels data/NIPS2003/GISETTE/gisette_train.data convert_nips
	-mkdir $(@D) ;\
	./convert_nips dense 5000 data/NIPS2003/GISETTE/gisette_train > $@

madelon.dir/raw_data:  data/NIPS2003/MADELON/madelon_train.labels data/NIPS2003/MADELON/madelon_train.data convert_nips
	-mkdir $(@D) ;\
	./convert_nips dense 500 data/NIPS2003/MADELON/madelon_train > $@

cleve.dir/raw_data:  data/uci/CLEVE/cleve_train.labels data/uci/cleve/cleve_train.data convert_nips
	-mkdir $(@D) ;\
	./convert_nips dense 13 data/uci/cleve/cleve_train > $@

add.dir/raw_data:  data/svn/WSD/add/xfull.txt  data/svn/WSD/add/y1.txt convert_add
	-mkdir $(@D) ;\
	./convert_add data/svn/WSD/add/xfull.txt data/svn/WSD/add/y1.txt > $@

add2.dir/raw_data:  data/svn/WSD/add/xfull.txt  data/svn/WSD/add/y2.txt convert_add
	-mkdir $(@D) ;\
	./convert_add data/svn/WSD/add/xfull.txt data/svn/WSD/add/y2.txt > $@

digit0.dir/raw_data.Y: digit_indicator ~/digit_labels
	./digit_indicator 0 < ~/digit_labels > $@



all.digits:
	-mkdir $@

all.digits/digit0.dir/raw_data:  data/mnist_digits convert_pixels select_digit all.digits
	-mkdir $(@D) ;\
	./select_digit     0 data/mnist_digits/digit_labels > $@
	./convert_pixels 784 data/mnist_digits/digit_pixels >> $@

all.digits/digit1.dir/raw_data:  data/mnist_digits convert_pixels select_digit all.digits
	-mkdir $(@D) ;\
	./select_digit     1 data/mnist_digits/digit_labels > $@
	./convert_pixels 784 data/mnist_digits/digit_pixels >> $@

all.digits/digit2.dir/raw_data:  data/mnist_digits convert_pixels select_digit all.digits
	-mkdir $(@D) ;\
	./select_digit     2 data/mnist_digits/digit_labels > $@
	./convert_pixels 784 data/mnist_digits/digit_pixels >> $@

all.digits/digit3.dir/raw_data:  data/mnist_digits convert_pixels select_digit all.digits
	-mkdir $(@D) ;\
	./select_digit     3 data/mnist_digits/digit_labels > $@
	./convert_pixels 784 data/mnist_digits/digit_pixels >> $@

all.digits/digit4.dir/raw_data:  data/mnist_digits convert_pixels select_digit all.digits
	-mkdir $(@D) ;\
	./select_digit     4 data/mnist_digits/digit_labels > $@
	./convert_pixels 784 data/mnist_digits/digit_pixels >> $@

all.digits/digit5.dir/raw_data:  data/mnist_digits convert_pixels select_digit all.digits
	-mkdir $(@D) ;\
	./select_digit     5 data/mnist_digits/digit_labels > $@
	./convert_pixels 784 data/mnist_digits/digit_pixels >> $@

all.digits/digit6.dir/raw_data:  data/mnist_digits convert_pixels select_digit all.digits
	-mkdir $(@D) ;\
	./select_digit     6 data/mnist_digits/digit_labels > $@
	./convert_pixels 784 data/mnist_digits/digit_pixels >> $@

all.digits/digit7.dir/raw_data:  data/mnist_digits convert_pixels select_digit all.digits
	-mkdir $(@D) ;\
	./select_digit     7 data/mnist_digits/digit_labels > $@
	./convert_pixels 784 data/mnist_digits/digit_pixels >> $@

all.digits/digit8.dir/raw_data:  data/mnist_digits convert_pixels select_digit all.digits
	-mkdir $(@D) ;\
	./select_digit     8 data/mnist_digits/digit_labels > $@
	./convert_pixels 784 data/mnist_digits/digit_pixels >> $@

all.digits/digit9.dir/raw_data:  data/mnist_digits convert_pixels select_digit all.digits
	-mkdir $(@D) ;\
	./select_digit     9 data/mnist_digits/digit_labels > $@
	./convert_pixels 784 data/mnist_digits/digit_pixels >> $@

digit.dir/raw_data:  ~/digit_training convert_digits
	-mkdir $(@D) ;\
	./select_digit     0 data/mnist_digits/digit_labels > $@
	./convert_pixels 784 data/mnist_digits/digit_pixels >> $@

#bankrupty.dir/data: data/bankruptcy/bank_1.data

junwen.dir/raw_data:  data/junwen/cpg2.arff  arffToBobsC45.pl c45_to_bob
	-mkdir $(@D)
	-ln -s data/junwen/cpg2.arff junwen.dir/cpg2.arff
	cd junwen.dir; perl ../arffToBobsC45.pl cpg2.arff 
	cd junwen.dir; ../c45_to_bob --input=cpg2 --raw-data > ../$@

############################################################################
#									   #
#            BUILD LOCAL CODE						   #
#									   #
############################################################################

bobcat: bobcat.cc
	g++ $^ -o $@

fold_generator: fold_generator.cc
	g++ $^ -o $@

bob2rows: bob2rows.cc
	g++ $^ -o $@

bob2names: bob2names.cc
	g++ $^ -o $@

c45_to_bob: c45_syntax.h c45_syntax.o c45_syntax.test.o
	g++ c45_syntax.test.o c45_syntax.o -o c45_to_bob

############################################################################
#									   #
#            GRAB CODE FROM OTHER DIRECTORIES				   #
#									   #
############################################################################

../rkhs_svd/svd:
	make -C $(@D) $(@F)

../rkhs_svd/random_svd:
	make -C $(@D) $(@F)

../auctions/validator:
	make -C $(@D) $(@F)

../auctions/estimator:
	make -C $(@D) $(@F)

../seq_regr/c45_syntax.test.exec:
	make ../seq_regr/c45_syntax.test.exec

../auctions/auction.test.exec:
	make -C $(@D) auction.test.exec

ifeq ($(COMPILE_CODE),1)

c45_parser: ../seq_regr/c45_syntax.test.exec

svd:        ../rkhs_svd/svd

random_svd: ../rkhs_svd/random_svd

validator:  ../auctions/validator

estimator:  ../auctions/estimator

auction:    ../auctions/auction.test.exec

endif

c45_parser: 
	make ../seq_regr/c45_syntax.test.exec ;\
	-rm -f $@ ;\
	cp ../seq_regr/c45_syntax.test.exec $@

svd: 
	make ../rkhs_svd/svd ;\
	-rm -f $@ ;\
	cp ../rkhs_svd/svd $@

random_svd: 
	make ../rkhs_svd/random_svd ;\
	-rm -f $@ ;\
	cp ../rkhs_svd/random_svd $@

validator: 
	make ../auctions/validator ;\
	-rm -f $@ ;\
	cp ../auctions/validator $@

estimator:
	make ../auctions/estimator ;\
	-rm -f $@ ;\
	cp ../auctions/estimator $@

auction: 
	make ../auctions/auction.test.exec ;\
	-rm -f $@ ;\
	cp ../auctions/auction.test.exec $@


############################################################################
#									   #
#            GENERAL DATA MANIPULATIONS					   #
#									   #
############################################################################

#                  RUNS LEVEL 

%.5forward.summary: %.dir/5forward/5forward.summary
	ln -sf $< $@ ;\

%.10forward.summary: %.dir/10forward/10forward.summary
	ln -sf $< $@ ;\

%.5reverse.summary: %.dir/5reverse/5reverse.summary
	ln -sf $< $@ ;\

%.10reverse.summary: %.dir/10reverse/10reverse.summary
	ln -sf $< $@ ;\

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#                 foo.dir/* 

%/raw_data.Y: %/raw_data
	head -3 $< > $@

%/raw_data.Xs: %/raw_data
	sed "4~1!d" $< > $@

%.dir/svd_data.$(NUMBER_SVD_VARIABLES):  %.dir/raw_data.Y %.dir/raw_data.Xs svd
	cat  $*.dir/raw_data.Y $*.dir/raw_data.Xs | ./svd --variables=$(NUMBER_SVD_VARIABLES) > $@
	@echo $@ created with `wc $@`

%.dir/random_svd_data.$(NUMBER_RANDOM_SVD_VARIABLES): %.dir/raw_data.Y %.dir/raw_data.Xs random_svd
	cat  $*.dir/raw_data.Y $*.dir/raw_data.Xs | ./random_svd --variables=$(NUMBER_RANDOM_SVD_VARIABLES) > $@
	@echo $@ created with `wc $@`

#    #    #    #    #    #    #    #    #    #    #    #    #    #    #    #    

# 	           foo.dir/5reverse/*

5%.summary: 0of5%/costs 1of5%/costs 2of5%/costs 3of5%/costs 4of5%/costs 
	cat $^ > $@
	cat $@ $(<D)/log >> $@.running_log

10%.summary: 0of10%/costs 1of10%/costs 2of10%/costs 3of10%/costs 4of10%/costs \
             5of10%/costs 6of10%/costs 7of10%/costs 8of10%/costs 9of10%/costs 
	cat $^ > $@
	cat $@ $(<D)/log >> $@.running_log

%/5reverse %/5reverse/5folds_link: %/5folds/5_folds_made
	-mkdir $*/5reverse ;\
	ln -sf  ../5folds $*/5reverse/5folds_link

%/5forward %/5forward/5folds_link: %/5folds/5_folds_made
	-mkdir $*/5forward ;\
	ln -sf  ../5folds $*/5forward/5folds_link

%/10reverse/ %/10reverse/10folds %/10reverse/10folds/fold_input %/10reverse/10folds/10_folds_made: %/10folds/10_folds_made
	-mkdir $(dir $@) ;\
	cd $(dir $@); ln -s ../10folds

%/10forward/  %/10forward/10folds_link: %/10folds/10_folds_made
	-mkdir $*/10forward ;\
	ln -sf  ../10folds $*/10forward/10folds_link

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

#               foo.dir/5folds/*

%.dir/5folds/fold_input: %.dir/raw_data.Y %.dir/svd_data.$(NUMBER_SVD_VARIABLES) %.dir/random_svd_data.$(NUMBER_RANDOM_SVD_VARIABLES) %.dir/raw_data.Xs
	-mkdir $(@D)
#	ln -sf ../raw_data $@
	cat $^ > $@

%.dir/10folds/fold_input: %.dir/raw_data.Y %.dir/svd_data.$(NUMBER_SVD_VARIABLES) %.dir/random_svd_data.$(NUMBER_RANDOM_SVD_VARIABLES) %.dir/raw_data.Xs
	-mkdir $(@D) ;\
	cat $^ > $@
#	ln -sf ../raw_data $@

%/5folds/fold_input.0 %/5folds/fold_input.1 %/5folds/fold_input.2 %/5folds/fold_input.3 %/5folds/fold_input.4: %/5folds/fold_input fold_generator
	./fold_generator $< 5

%/5folds/names: %/5folds/fold_input bob2names
	bob2names < $< > $@

%/10folds/fold_input.0 %/10folds/fold_input.1 %/10folds/fold_input.2 %/10folds/fold_input.3 %/10folds/fold_input.4 \
%/10folds/fold_input.5 %/10folds/fold_input.6 %/10folds/fold_input.7 %/10folds/fold_input.8 %/10folds/fold_input.9: %/10folds/fold_input fold_generator
	./fold_generator $< 10

%/10folds/names: %/10folds/fold_input bob2names
	bob2names < $< > $@

row_order.%: fold_input.% bob2rows
	./bob2rows < $< > $@

%/5_folds_made: %/row_order.0 %/row_order.1 %/row_order.2 %/row_order.3 %/row_order.4 %/names
	touch $@

%/10_folds_made: %/row_order.0 %/row_order.1 %/row_order.2 %/row_order.3 %/row_order.4 \
                 %/row_order.5 %/row_order.6 %/row_order.7 %/row_order.8 %/row_order.9 %/names
	touch $@

#    #    #    #    #    #    #    #    #    #    #    #    #    #    #    #

#            foo.dir/5reverse/0of5reverse/*

%/auction.model %/auction.model.txt %/log: %/all_input auction
	./auction -f $(@D)/all_input -r $(NUMBER_ROUNDS) --output-path=$(@D) -c 4 > $(@D)/log

%/all_input: %/input bobcat
	./bobcat $(@D)/input.* > $(@D)/all_input

%/predictions: validator %/auction.model %/validation_size %/all_input
	cat $(@D)/validation_size $(@D)/validation.* | ./validator -n $(@D)/names -m $(@D)/auction.model -o /dev/null -F $(@D)/predictions

5%.predictions: 0of5%/predictions 1of5%/predictions 2of5%/predictions 3of5%/predictions 4of5%/predictions
	cat $^ > $@

%/costs: validator %/auction.model %/validation_size  %/all_input
	cat $(@D)/validation_size $(@D)/validation.* | ./validator -n $(@D)/names -m $(@D)/auction.model -o $(@D)/costs

%/validation_size: %/input
	cat $(@D)/validation.* | wc -l > $@

#    #    #    #    #    #    #    #    #    #    #    #    #    #    #    #

%/0of5forward/input: %/5folds_link
	-mkdir $(@D) ;\
	ln -sf ../5folds_link/names              $(@D)/names  ;\
	ln -sf ../5folds_link/row_order.0 $(@D)/validation.data ;\
	ln -sf ../5folds_link/fold_input.1 $@.1 ;\
	ln -sf ../5folds_link/fold_input.2 $@.2 ;\
	ln -sf ../5folds_link/fold_input.3 $@.3 ;\
	ln -sf ../5folds_link/fold_input.4 $@.4
	touch $@

%/1of5forward/input: %/5folds_link
	-mkdir $(@D) ;\
	ln -sf ../5folds_link/names              $(@D)/names ;\
	ln -sf ../5folds_link/row_order.1 $(@D)/validation.data ;\
	ln -sf ../5folds_link/fold_input.0 $@.0 ;\
	ln -sf ../5folds_link/fold_input.2 $@.2 ;\
	ln -sf ../5folds_link/fold_input.3 $@.3 ;\
	ln -sf ../5folds_link/fold_input.4 $@.4 ;\
	touch $@

%/2of5forward/input: %/5folds_link
	-mkdir $(@D) ;\
	ln -sf ../5folds_link/names              $(@D)/names ;\
	ln -sf ../5folds_link/row_order.2 $(@D)/validation.data ;\
	ln -sf ../5folds_link/fold_input.0 $@.0 ;\
	ln -sf ../5folds_link/fold_input.1 $@.1 ;\
	ln -sf ../5folds_link/fold_input.3 $@.3 ;\
	ln -sf ../5folds_link/fold_input.4 $@.4 ;\
	touch $@

%/3of5forward/input: %/5folds_link
	-mkdir $(@D) ;\
	ln -sf ../5folds_link/names              $(@D)/names ;\
	ln -sf ../5folds_link/row_order.3 $(@D)/validation.data ;\
	ln -sf ../5folds_link/fold_input.0 $@.0 ;\
	ln -sf ../5folds_link/fold_input.1 $@.1 ;\
	ln -sf ../5folds_link/fold_input.2 $@.2 ;\
	ln -sf ../5folds_link/fold_input.4 $@.4 ;\
	touch $@

%/4of5forward/input: %/5folds_link
	-mkdir $(@D) ;\
	ln -sf ../5folds_link/names              $(@D)/names ;\
	ln -sf ../5folds_link/row_order.4 $(@D)/validation.data ;\
	ln -sf ../5folds_link/fold_input.0 $@.0 ;\
	ln -sf ../5folds_link/fold_input.1 $@.1 ;\
	ln -sf ../5folds_link/fold_input.2 $@.2 ;\
	ln -sf ../5folds_link/fold_input.3 $@.3 ;\
	touch $@
#    #    #    #    #    #    #    #    #    #    #    #    #    #    #    #

%/0of5reverse/input: %/5folds_link
	-mkdir $(@D) ;\
	ln -sf ../5folds_link/names              $(@D)/names ;\
	ln -sf ../5folds_link/fold_input.0 $(@D)/input.data ;\
	ln -sf ../5folds_link/row_order.1 $(@D)/validation.1 ;\
	ln -sf ../5folds_link/row_order.2 $(@D)/validation.2 ;\
	ln -sf ../5folds_link/row_order.3 $(@D)/validation.3 ;\
	ln -sf ../5folds_link/row_order.4 $(@D)/validation.4 ;\
	touch $@

%/1of5reverse/input: %/5folds_link
	-mkdir $(@D) ;\
	ln -sf ../5folds_link/names              $(@D)/names ;\
	ln -sf ../5folds_link/row_order.0 $(@D)/validation.0 ;\
	ln -sf ../5folds_link/fold_input.1 $(@D)/input.data ;\
	ln -sf ../5folds_link/row_order.2 $(@D)/validation.2 ;\
	ln -sf ../5folds_link/row_order.3 $(@D)/validation.3 ;\
	ln -sf ../5folds_link/row_order.4 $(@D)/validation.4 ;\
	touch $@

%/2of5reverse/input: %/5folds_link
	-mkdir $(@D) ;\
	ln -sf ../5folds_link/names              $(@D)/names ;\
	ln -sf ../5folds_link/row_order.0 $(@D)/validation.0 ;\
	ln -sf ../5folds_link/row_order.1 $(@D)/validation.1 ;\
	ln -sf ../5folds_link/fold_input.2 $(@D)/input.data ;\
	ln -sf ../5folds_link/row_order.3 $(@D)/validation.3 ;\
	ln -sf ../5folds_link/row_order.4 $(@D)/validation.4 ;\
	touch $@

%/3of5reverse/input: %/5folds_link
	-mkdir $(@D) ;\
	ln -sf ../5folds_link/names       $(@D)/names ;\
	ln -sf ../5folds_link/row_order.0 $(@D)/validation.0 ;\
	ln -sf ../5folds_link/row_order.1 $(@D)/validation.1 ;\
	ln -sf ../5folds_link/row_order.2 $(@D)/validation.2 ;\
	ln -sf ../5folds_link/fold_input.3 $(@D)/input.data ;\
	ln -sf ../5folds_link/row_order.4 $(@D)/validation.4 ;\
	touch $@

%/4of5reverse/input: %/5folds_link
	-mkdir $(@D) ;\
	ln -sf ../5folds_link/names              $(@D)/names ;\
	ln -sf ../5folds_link/row_order.0 $(@D)/validation.0 ;\
	ln -sf ../5folds_link/row_order.1 $(@D)/validation.1 ;\
	ln -sf ../5folds_link/row_order.2 $(@D)/validation.2 ;\
	ln -sf ../5folds_link/row_order.3 $(@D)/validation.3 ;\
	ln -sf ../5folds_link/fold_input.4 $(@D)/input.data ;\
	touch $@

#    #    #    #    #    #    #    #    #    #    #    #    #    #    #    #

%/0of10forward/input: %/10folds_link
	-mkdir $(@D) ;\
	ln -sf ../10folds_link/names              $(@D)/names ;\
	ln -sf ../10folds_link/row_order.0 $(@D)/validation.data ;\
	ln -sf ../10folds_link/fold_input.1 $@.1 ;\
	ln -sf ../10folds_link/fold_input.2 $@.2 ;\
	ln -sf ../10folds_link/fold_input.3 $@.3 ;\
	ln -sf ../10folds_link/fold_input.4 $@.4 ;\
	ln -sf ../10folds_link/fold_input.5 $@.5 ;\
	ln -sf ../10folds_link/fold_input.6 $@.6 ;\
	ln -sf ../10folds_link/fold_input.7 $@.7 ;\
	ln -sf ../10folds_link/fold_input.8 $@.8 ;\
	ln -sf ../10folds_link/fold_input.9 $@.9 ;\
	touch $@

%/1of10forward/input: %/10folds_link
	-mkdir $(@D) ;\
	ln -sf ../10folds_link/names              $(@D)/names ;\
	ln -sf ../10folds_link/fold_input.0 $@.0 ;\
	ln -sf ../10folds_link/row_order.1 $(@D)/validation.data ;\
	ln -sf ../10folds_link/fold_input.2 $@.2 ;\
	ln -sf ../10folds_link/fold_input.3 $@.3 ;\
	ln -sf ../10folds_link/fold_input.4 $@.4 ;\
	ln -sf ../10folds_link/fold_input.5 $@.5 ;\
	ln -sf ../10folds_link/fold_input.6 $@.6 ;\
	ln -sf ../10folds_link/fold_input.7 $@.7 ;\
	ln -sf ../10folds_link/fold_input.8 $@.8 ;\
	ln -sf ../10folds_link/fold_input.9 $@.9 ;\
	touch $@

%/2of10forward/input:  %/10folds_link
	-mkdir $(@D) ;\
	ln -sf ../10folds_link/names              $(@D)/names ;\
	ln -sf ../10folds_link/fold_input.0 $@.0 ;\
	ln -sf ../10folds_link/fold_input.1 $@.1 ;\
	ln -sf ../10folds_link/row_order.2 $(@D)/validation.data ;\
	ln -sf ../10folds_link/fold_input.3 $@.3 ;\
	ln -sf ../10folds_link/fold_input.4 $@.4 ;\
	ln -sf ../10folds_link/fold_input.5 $@.5 ;\
	ln -sf ../10folds_link/fold_input.6 $@.6 ;\
	ln -sf ../10folds_link/fold_input.7 $@.7 ;\
	ln -sf ../10folds_link/fold_input.8 $@.8 ;\
	ln -sf ../10folds_link/fold_input.9 $@.9 ;\
	touch $@

%/3of10forward/input:  %/10folds_link
	-mkdir $(@D) ;\
	ln -sf ../10folds_link/names              $(@D)/names ;\
	ln -sf ../10folds_link/fold_input.0 $@.0 ;\
	ln -sf ../10folds_link/fold_input.1 $@.1 ;\
	ln -sf ../10folds_link/fold_input.2 $@.2 ;\
	ln -sf ../10folds_link/row_order.3 $(@D)/validation.data ;\
	ln -sf ../10folds_link/fold_input.4 $@.4 ;\
	ln -sf ../10folds_link/fold_input.5 $@.5 ;\
	ln -sf ../10folds_link/fold_input.6 $@.6 ;\
	ln -sf ../10folds_link/fold_input.7 $@.7 ;\
	ln -sf ../10folds_link/fold_input.8 $@.8 ;\
	ln -sf ../10folds_link/fold_input.9 $@.9 ;\
	touch $@

%/4of10forward/input:  %/10folds_link
	-mkdir $(@D) ;\
	ln -sf ../10folds_link/names              $(@D)/names ;\
	ln -sf ../10folds_link/fold_input.0 $@.0 ;\
	ln -sf ../10folds_link/fold_input.1 $@.1 ;\
	ln -sf ../10folds_link/fold_input.2 $@.2 ;\
	ln -sf ../10folds_link/fold_input.3 $@.3 ;\
	ln -sf ../10folds_link/row_order.4 $(@D)/validation.data ;\
	ln -sf ../10folds_link/fold_input.5 $@.5 ;\
	ln -sf ../10folds_link/fold_input.6 $@.6 ;\
	ln -sf ../10folds_link/fold_input.7 $@.7 ;\
	ln -sf ../10folds_link/fold_input.8 $@.8 ;\
	ln -sf ../10folds_link/fold_input.9 $@.9 ;\
	touch $@

%/5of10forward/input:  %/10folds_link
	-mkdir $(@D) ;\
	ln -sf ../10folds_link/names              $(@D)/names ;\
	ln -sf ../10folds_link/fold_input.0 $@.0 ;\
	ln -sf ../10folds_link/fold_input.1 $@.1 ;\
	ln -sf ../10folds_link/fold_input.2 $@.2 ;\
	ln -sf ../10folds_link/fold_input.3 $@.3 ;\
	ln -sf ../10folds_link/fold_input.4 $@.4 ;\
	ln -sf ../10folds_link/row_order.5 $(@D)/validation.data ;\
	ln -sf ../10folds_link/fold_input.6 $@.6 ;\
	ln -sf ../10folds_link/fold_input.7 $@.7 ;\
	ln -sf ../10folds_link/fold_input.8 $@.8 ;\
	ln -sf ../10folds_link/fold_input.9 $@.9 ;\
	touch $@

%/6of10forward/input:  %/10folds_link
	-mkdir $(@D) ;\
	ln -sf ../10folds_link/names              $(@D)/names ;\
	ln -sf ../10folds_link/fold_input.0 $@.0 ;\
	ln -sf ../10folds_link/fold_input.1 $@.1 ;\
	ln -sf ../10folds_link/fold_input.2 $@.2 ;\
	ln -sf ../10folds_link/fold_input.3 $@.3 ;\
	ln -sf ../10folds_link/fold_input.4 $@.4 ;\
	ln -sf ../10folds_link/fold_input.5 $@.5 ;\
	ln -sf ../10folds_link/row_order.6 $(@D)/validation.data ;\
	ln -sf ../10folds_link/fold_input.7 $@.7 ;\
	ln -sf ../10folds_link/fold_input.8 $@.8 ;\
	ln -sf ../10folds_link/fold_input.9 $@.9 ;\
	touch $@

%/7of10forward/input:  %/10folds_link
	-mkdir $(@D) ;\
	ln -sf ../10folds_link/names              $(@D)/names ;\
	ln -sf ../10folds_link/fold_input.0 $@.0 ;\
	ln -sf ../10folds_link/fold_input.1 $@.1 ;\
	ln -sf ../10folds_link/fold_input.2 $@.2 ;\
	ln -sf ../10folds_link/fold_input.3 $@.3 ;\
	ln -sf ../10folds_link/fold_input.4 $@.4 ;\
	ln -sf ../10folds_link/fold_input.5 $@.5 ;\
	ln -sf ../10folds_link/fold_input.6 $@.6 ;\
	ln -sf ../10folds_link/row_order.7 $(@D)/validation.data ;\
	ln -sf ../10folds_link/fold_input.8 $@.8 ;\
	ln -sf ../10folds_link/fold_input.9 $@.9 ;\
	touch $@

%/8of10forward/input:  %/10folds_link
	-mkdir $(@D) ;\
	ln -sf ../10folds_link/names              $(@D)/names ;\
	ln -sf ../10folds_link/fold_input.0 $@.0 ;\
	ln -sf ../10folds_link/fold_input.1 $@.1 ;\
	ln -sf ../10folds_link/fold_input.2 $@.2 ;\
	ln -sf ../10folds_link/fold_input.3 $@.3 ;\
	ln -sf ../10folds_link/fold_input.4 $@.4 ;\
	ln -sf ../10folds_link/fold_input.5 $@.5 ;\
	ln -sf ../10folds_link/fold_input.6 $@.6 ;\
	ln -sf ../10folds_link/fold_input.7 $@.7 ;\
	ln -sf ../10folds_link/row_order.8 $(@D)/validation.data ;\
	ln -sf ../10folds_link/fold_input.9 $@.9 ;\
	touch $@

%/9of10forward/input:  %/10folds_link
	-mkdir $(@D) ;\
	ln -sf ../10folds_link/names              $(@D)/names ;\
	ln -sf ../10folds_link/fold_input.0 $@.0 ;\
	ln -sf ../10folds_link/fold_input.1 $@.1 ;\
	ln -sf ../10folds_link/fold_input.2 $@.2 ;\
	ln -sf ../10folds_link/fold_input.3 $@.3 ;\
	ln -sf ../10folds_link/fold_input.4 $@.4 ;\
	ln -sf ../10folds_link/fold_input.5 $@.5 ;\
	ln -sf ../10folds_link/fold_input.6 $@.6 ;\
	ln -sf ../10folds_link/fold_input.7 $@.7 ;\
	ln -sf ../10folds_link/fold_input.8 $@.8 ;\
	ln -sf ../10folds_link/row_order.9 $(@D)/validation.data ;\
	touch $@

#  Much nice output is destroyed when the make terminates.
#  This can be avoided by the following magic
#  It runs the existing Makefile with a "-n" option.  This is a no-op, just print the commands.
#  Then it grabs the "destroy the data line" at the end (i.e. the one with an rm in it, say "rm foo bar").
#  It then rewrites this line to make it into a .SECONDARY: foo bar
#  This tell make not to destroy all these temparary files if they get built correctly.

-include Makefile.SECONDARY.$(MAKECMDGOALS)

ifneq ($(MAKECMDGOALS),clean)
Makefile.SECONDARY.$(MAKECMDGOALS): 
	touch Makefile.SECONDARY.$(MAKECMDGOALS)
#	@echo " * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * "
#	make -n $(MAKECMDGOALS) |tail -3 | grep "rm" |sed  "s/rm/\.SECONDARY:/"
#	@echo " * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * "
	make -n $(MAKECMDGOALS) |tail -3 | grep "rm" |sed  "s/rm/\.SECONDARY:/" > Makefile.SECONDARY.$(MAKECMDGOALS)
endif

Makefile.SECONDARY.clean:
	touch Makefile.SECONDARY.clean

