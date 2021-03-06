########################################################################### 
# 
#                       TARGETS 
# 

all: libs auctions/auction.test

clean:
	-rm -f */*.o  */*.d */*~  */core */lib*.a
	-rm -f */*.exec */*.OK */test */*.results
	-rm -f */*.Errors */*.Errors_all */*.No_errors


c_flags: 
	cp c_flags.sample c_flags

libs: c_flags
	-$(MAKE) -sC utils         lib
	-$(MAKE) -sC random        lib
	-$(MAKE) -sC ranges        lib
	-$(MAKE) -sC spline        lib
	-$(MAKE) -sC eigen         lib
	-$(MAKE) -sC tools         lib

#	-$(MAKE) -sC auctions      lib
#	-$(MAKE) -sC gsl_tools     lib

details: c_flags
	-$(MAKE) -C utils         lib
	-$(MAKE) -C random        lib
	-$(MAKE) -C ranges        lib
	-$(MAKE) -C spline        lib
	-$(MAKE) -C eigen         lib

#	-$(MAKE) -C auctions      lib
#	-$(MAKE) -C seq_regr      lib

auctions/auctions.test.exec:
	-$(MAKE) -sC auctions auction.test.exec

gsl/.checkout_by_make: 
	git clone sob:/git/bob/C/gsl
	touch gsl/.checkout_by_make
