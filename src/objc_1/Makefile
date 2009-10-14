
all: clean
	gcc -g -O2 -w -std=c99 -lobjc -Wno-import utils.m tests.m solver.m Trie.m Letter.m Board.m main.m -o boggle

clean: 
	-rm boggle