Boggle Solvers implemented in various languages


The implementations are intentionally not threaded to ensure that they only use one cpu core on the test hardware. The goal is not to measure multi-core performance or algorithmic/datastructure optimizations against each implementation but to instead compare the performance of the code(in time and space usage) that the respective compilers generate using as similar as possible algorithms and datastructures.



The solver can be run like this :


Lisp :

(from the root directory)

$ shell/solve-boards.sh < boards/sample-100x100.txt > output/words-100x100-sbcl.txt ; tail -n 40 output/words-100x100-sbcl.txt


(have a look at solve-boards.sh if you want to use a different Lisp compiler).


C :

(from the root directory)

$ pushd src/c

$ make

$ popd

$ bin/boggle-c --test --dict dict/english_270k.txt < boards/sample-100x100.txt > output/words100x100-c.txt ; tail -n 40 output/words100x100-c.txt

