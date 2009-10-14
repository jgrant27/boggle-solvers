#GCC = /usr/bin/gcc-4.0 # The apple version 4.0.1
#GCC = /usr/bin/gcc-4.2 # The apple version 4.2.1
GCC = /opt/local/bin/gcc-mp-4.2 # The macports version 4.2.4
STD_FLAGS = -O3 -fomit-frame-pointer -fstrict-aliasing -momit-leaf-frame-pointer -falign-loops -funroll-loops
SRC_FILES = utils.c tests.c trie.c letter.c board.c solver.c main.c


all: clean bin_dbg bin bin_x8664

bin_dbg:
	$(GCC) -fprofile-arcs -ftest-coverage -g $(SRC_FILES) -o boggle_dbg

bin:
	$(GCC) $(STD_FLAGS) $(SRC_FILES) -o boggle

bin_x8664:
	$(GCC) $(STD_FLAGS) -m64 $(SRC_FILES) -o boggle_x8664

clean: 
	-rm -fr *.o *~ *.dSYM *.gcno boggle boggle_dbg boggle_x8664

