include ../c_flags

###########################################################################
#
#        Options
#   Examples of depending on libaries
#        LDLIBS =  -lthing -lregression 
#        libs_used =  ../lib/libthing.a ../lib/libregression.a 
# 
###########################################################################


PROJECT_NAME = bellman

# OPT = -O3 -std=c++0x -DNDEBUG

OPT = -O3 -std=c++0x

USES = utils random

# EXTERNAL_USES = gsl

level_0 = wealth.o
level_1 = utility.o
level_2 = bellman.o

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
###########################################################################


# TAGS 
# find . | grep ".*\.\(h\|cc\)" | xargs etags -f TAGS

# -------------------------------------------------------------------
# bellman recursion for competitive value
#
# constrained:  univ univ 2 0.05   50   0.5 7     -0.015669 0.015669 0.015669
# uncons      uncon g50 2 0.05   50   0.05 7     37.1289 37.7115 0.291287
# -------------------------------------------------------------------

compiler: bellman
	gcc --version

constrained_test: bellman
	./bellman --gamma 2 --rounds 50 --constrain --oracleprob 0.5 --bidderprob 0.0 --write    # geometric oracle
	./bellman --gamma 2 --rounds 50 --constrain --oracleprob 0.0 --bidderprob 0.5 --write    # univ oracle
	./bellman --gamma 2 --rounds 50             --oracleprob 0.5 --bidderprob 0.0 --write    # warning message

unconstrained_test: bellman
	./bellman --gamma 2 --rounds 50                              --bidderprob 0.5 --write   



bellman_main.o: bellman_main.cc

bellman: bellman.o wealth.o utility.o bellman_main.o
	$(GCC) $^ $(LDLIBS) -o  $@

# Test geometric oracle, universal bidder
# With revised geometric wealth array and different top fill (19 Jun 12)
#           g01000 univ 2 0.05   200   0.5 7     515.584 -448.759 -482.172
# Done with sobolev and prior code the geometric got higher risk
#           g01000 univ 2 0.05   200   0.5 7     500.546 -463.415 -481.981
# With code set to find the minimum risk, get same answer (so make neg_risk to retain legacy figures)
#           g01000 univ 2 0.05   200   0.5 7     -515.584 448.759 482.172

bellman_test: bellman
	./bellman --gamma 2.0 --rounds 200 --constrain --oracleprob 0.01 --bidderprob 0 --write

# Unconstrained 0 2.5 0.05 7 0.5 1.5 6.5 -0.0691835 0.068553 0.0550946
bellman_check: bellman
	./bellman --gamma 2.0 --rounds   7 --constrain --oracleprob 0.01 --bidderprob 0 --write

# ---  $^ are prereq    $@ is target    $* is stem
#      change n to change path, file names, and the length of run;  gp is path
#      Once run, cat combines these lines to show all of the results.

# define these constants, then use a command like
#    make -j lots  -k runs/summary.reject_psi0090_n100
# or
#    make -j lots  -k runs/summary.risk_psi00100_n250
# with these values chosen to match (don't know how to pick them from make input
# so you have to define the constants here and match them in the make command.
# Builds a directory in runs for these results, then files for each.
n = 500

# define expert by geometric rate 
psi = 0.02500
ptxt=   02500

#--------------------------------------------------------------------------------------------
#  below here is automagic, building output in runs/   
#--------------------------------------------------------------------------------------------

# define path within runs subdirectory for each psi (oracle) and n combination; 0 ids universal
pp = runs/risk_psi$(ptxt)_n$(n)

$(pp)/.directory_built: 
	echo Building directory for $(pp)
	mkdir $(pp)
	touch $@

# main target with parameters that identify gamma over tasks
runs/summary.risk_psi$(ptxt)_n$(n): bellman bellman.sh $(pp)/0.05 $(pp)/0.1 $(pp)/0.2 $(pp)/0.3 $(pp)/0.35 $(pp)/0.4 $(pp)/0.45 $(pp)/0.5 $(pp)/0.55 $(pp)/0.6 $(pp)/0.65 $(pp)/0.7 $(pp)/0.75 $(pp)/0.8 $(pp)/0.825  $(pp)/0.85 $(pp)/0.875 $(pp)/0.9 $(pp)/0.925 $(pp)/0.95 $(pp)/0.975 $(pp)/1.0 $(pp)/1.025 $(pp)/1.05 $(pp)/1.075 $(pp)/1.1 $(pp)/1.125 $(pp)/1.15 $(pp)/1.175 $(pp)/1.2 $(pp)/1.25 $(pp)/1.3 $(pp)/1.35 $(pp)/1.4 $(pp)/1.45 $(pp)/1.5 $(pp)/1.55 $(pp)/1.6 $(pp)/1.65 $(pp)/1.7 $(pp)/1.8 $(pp)/1.9 $(pp)/2.0 $(pp)/2.25 $(pp)/2.5 $(pp)/3.0 $(pp)/4.0 $(pp)/5.0 $(pp)/7.5 $(pp)/10.0  $(pp)/15.0 $(pp)/20.0 $(pp)/30.0 $(pp)/50.0  $(pp)/100.0   $(pp)/200.0 
	rm -f $@
	cat $(filter $(pp)/%,$^) >> $@

runs/summary.reject_psi$(ptxt)_n$(n): bellman bellman.sh $(pp)/0.5 $(pp)/0.55 $(pp)/0.6 $(pp)/0.65 $(pp)/0.675 $(pp)/0.7 $(pp)/0.705 $(pp)/0.710 $(pp)/0.715 $(pp)/0.720 $(pp)/0.725 $(pp)/0.7275 $(pp)/0.7285 $(pp)/0.7290 $(pp)/0.7295 $(pp)/0.73 $(pp)/0.7305 $(pp)/0.731 $(pp)/0.7315 $(pp)/0.7320 $(pp)/0.7325 $(pp)/0.735 $(pp)/0.740 $(pp)/0.745 $(pp)/0.75 $(pp)/0.755 $(pp)/0.760 $(pp)/0.765 $(pp)/0.770 $(pp)/0.775 $(pp)/0.8 $(pp)/0.825  $(pp)/0.85 $(pp)/0.875 $(pp)/0.9 $(pp)/0.925 $(pp)/0.95 $(pp)/0.975 $(pp)/0.990 $(pp)/1.0 $(pp)/1.01 $(pp)/1.05 $(pp)/1.1 $(pp)/1.15 $(pp)/1.2 $(pp)/1.3 $(pp)/1.4 $(pp)/1.5 $(pp)/1.6 $(pp)/1.7 $(pp)/2.0 $(pp)/2.5 $(pp)/3.0 $(pp)/3.5 $(pp)/4.0  $(pp)/4.5  $(pp)/5.0  $(pp)/5.5  $(pp)/6.0  $(pp)/6.5  $(pp)/7.0 $(pp)/7.5 $(pp)/8.0  $(pp)/9.0 $(pp)/10.0 $(pp)/15.0 $(pp)/20.0
	rm -f $@
	cat $(filter $(pp)/%,$^) >> $@


# actual run command for contrained solution, with univ and geometric
$(pp)/%: bellman bellman.sh $(pp)/.directory_built
	./bellman --gamma $* --constrain --oracleprob $(psi) --bidderprob 0      --rounds $(n) >  $@
	./bellman --gamma $* --constrain --oracleprob 0      --bidderprob $(psi) --rounds $(n) >> $@


# ---  unconstrained
#   solutions for testing
#      2.5 25 1 u 0.5 -0.0332263 0.181596 0.0859291
#      2.5 10 1 u 0.5 -0.0651093 0.086978 0.0608351 	
bellman_results.u500: bellman bellman.sh  
	rm -f bellman_results.500
	./bellman.sh u 500 >  bellman_results.500
	./bellman.sh e 500 >> bellman_results.500
	./bellman.sh g 500 >> bellman_results.500



###########################################################################
include ../rules_for_makefiles

