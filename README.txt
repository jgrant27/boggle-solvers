
The solver can be run like this :


Lisp :

(from the root directory)

./solve-boards.sh < output/sample-20x20.txt > words-20x20-sbcl.txt ; tail -n 40 words-20x20-sbcl.txt


(have a look at solve-boards.sh if you want to use a different Lisp compiler).


C :

(from the root directory)

cd src/c

make

./boggle --test --dict ../../dict/english_270k.txt < ../../output/sample-100x100.txt > words100x100.txt ; tail -n 40 words100x100.txt

