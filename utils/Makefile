include ../c_flags

###########################################################################
#
#        Options
#
###########################################################################

INSTALL_PATH = ../utils

PROJECT_NAME = utils

OPT = -Ofast -mfpmath=sse -msse3 -m64 -march=native

# OPT = -O0

# USES = 

# EXTERNAL_USES = 


###########################################################################
#
#	Objects ... objects are listed in order of precedence
#
###########################################################################

level_0   = debug.o timing.o order.o utils.o light_threads.o thread_functions.o
level_1   = print_utils.o file_utils.o stat_utils.o function_utils.o bennett.o normal.o coding.o iterators.o operator_traits.o data_registry.o polyshrink.o c45_syntax.o property_list.o threshold.o read_utils.o line_search.o dynamic_array.o eigen_utils.o
level_2   = tags.o attributes.o

############################################################################
#
#            INCLUDING RULES AND DEFINITIONS
#
###########################################################################

compiler_version:
	$(GCC) --version

insert_columns: insert_columns.o
	$(GCC) $(CFLAGS) $(INCLUDES) insert_columns.o $(LDLIBS) -o $@
	cp -f $@ ~/bin/ctor

transpose: transpose.o column.o 
	$(GCC) $(CFLAGS) $(INCLUDES) transpose.o column.o -o $@

mands: mands.o
	$(GCC) mands.o $(LDLIBS) -o $@

cutt: cutt.o
	$(GCC) cutt.o $(LDLIBS) -o $@

###########################################################################

include ../rules_for_makefiles

