#  $Id: Makefile,v 3.0 2004/11/19 18:58:35 foster Exp $ 
#
#  $Source: /cvs/bob/c-all/Makefile,v $ 
# 
########################################################################### 
# 
#                       TARGETS 
# 

all: libs auctions/auctions.test.exec

clean:
	-rm -f */*.o  */*.d */*~  */core */lib*.a
	-rm -f */*.exec */*.OK */test */*.results
	-rm -f */*.Errors */*.Errors_all */*.No_errors

libs: 
	-$(MAKE) -sC utils         lib
	-$(MAKE) -sC random        lib
	-$(MAKE) -sC ranges        lib
	-$(MAKE) -sC spline        lib
	-$(MAKE) -sC auctions      lib
	-$(MAKE) -sC seq_regr      lib
#	$(MAKE) -sC calibrate     lib

details: 
	-$(MAKE) -C utils         lib
	-$(MAKE) -C random        lib
	-$(MAKE) -C ranges        lib
	-$(MAKE) -C spline        lib
	-$(MAKE) -C auctions      lib
	-$(MAKE) -C seq_regr      lib

auctions/auctions.test.exec:
	-$(MAKE) -sC auctions auction.test.exec

