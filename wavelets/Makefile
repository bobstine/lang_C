fwt: fwt.o wavelet.o wavecoef.o threshold.o cyclic.o coding.o encoder.o                           utils.h
	g++ -g -o fwt fwt.o wavelet.o wavecoef.o threshold.o cyclic.o encoder.o                              coding.o 

wavelet.test: wavelet.test.o wavelet.o wavecoef.o threshold.o cyclic.o coding.o                     encoder.o utils.h
	g++   -g -o wavelet.test wavelet.test.o wavelet.o wavecoef.o cyclic.o                                          threshold.o coding.o encoder.o

cyclic.test: cyclic.test.o cyclic.o utils.h
	g++   -g -o cyclic.test cyclic.test.o cyclic.o

threshold.test: threshold.test.o encoder.o utils.h threshold.o coding.o
	g++   -g -o threshold.test threshold.test.o threshold.o encoder.o coding.o

encoder.test: coding.o encoder.o encoder.test.o
	g++   -g -o encoder.test  coding.o encoder.o encoder.test.o

coding.test: coding.o coding.test.o
	g++   -g -o coding.test coding.o coding.test.o

####

fwt.o: fwt.cc threshold.h wavelet.h
	g++ -Wall -c -g fwt.cc

wavelet.o: wavelet.cc wavelet.h
	g++ -Wall -c -g wavelet.cc

wavecoef.o: wavecoef.cc wavecoef.h
	g++ -Wall -c -g wavecoef.cc

threshold.o: threshold.cc threshold.h
	g++ -Wall -c -g threshold.cc

cyclic.o: cyclic.h cyclic.cc
	g++ -Wall -c -g cyclic.cc

encoder.o: encoder.h encoder.cc
	g++ -Wall -c -g encoder.cc

coding.o: coding.cc coding.h
	g++ -Wall -c -g coding.cc

# utils.o: utils.h utils.cc
#	g++ -Wall -c -g utils.cc

cyclic.test.o: test_cyclic.cc
	g++ -Wall -c -g cyclic.test.cc

wavelet.test.o: wavelet.test.cc wavelet.o
	g++ -Wall -c -g wavelet.test.cc

threshold.test.o: threshold.test.cc threshold.o
	g++ -Wall -c -g threshold.test.cc

encoder.test.o: encoder.test.cc  coding.h  encoder.h
	g++ -Wall -c -g encoder.test.cc

coding.test.o: coding.test.cc coding.cc coding.h
	g++ -Wall -c -g coding.test.cc



