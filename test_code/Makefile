

#	$(GCC) $^ $(LDLIBS) -o $@
#	g++ -I /usr/local/include -I /usr/include  test_file_io.o  -lboost_system -lgomp -lm  -o test_file_io	

test_file_io: test_file_io.o
	g++-4.7 -I /usr/local/include -I /usr/include -I../utils -L../utils  test_file_io.o -lutils -lboost_system -lboost_regex -lm  -o test_file_io 
	./test_file_io


terminal_io: terminal_io.o
	$(GCC) $< -o $@
