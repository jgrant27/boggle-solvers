#!/usr/bin/env bash

pushd src/lisp/ > /dev/null


### Board Generator

sbcl --dynamic-space-size 1024 --load solver.lisp \
    --eval "(format t \"saving executable SBCL lisp image ...\")" \
    --eval "(finish-output)" \
    --eval "(proclaim '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))" \
    --eval "(handler-bind ((sb-ext:defconstant-uneql (lambda (c) (abort c)))) (load (compile-file \"solver.lisp\" )))" \
    --eval "(save-lisp-and-die \"../../bin/gen-boards\" :toplevel #'run-board-generator :executable t :purify t)" \
    --eval "(quit)"

#ecl \
#    -eval "(compile-file \"utils.lisp\" :output-file \"utils.o\" :system-p t)" \
#    -eval "(compile-file \"structs.lisp\" :output-file \"structs.o\" :system-p t)" \
#    -eval "(compile-file \"generation.lisp\" :output-file \"generation.o\" :system-p t)" \
#    -eval "(compile-file \"solver.lisp\" :output-file \"solver.o\" :system-p t)" \
#    -eval "(format t \"saving executable ECL lisp image ...\")" \
#    -eval "(c:build-program \"../../bin/gen-boards\" :lisp-files '(\"utils.o\" \"structs.o\" \"solver.o\" \"generation.o\") :epilogue-code \"(run-board-generator)\")" \
#    -eval "(quit)"

#ccl64 -K utf-8 -n -Q -l solver.lisp \
#    -e "(format t \"saving executable CCL64 lisp image (board generator) ...\")" \
#    -e "(save-application \"../../bin/gen-boards\" :prepend-kernel t :toplevel-function #'run-board-generator)"



### Board solver

sbcl --dynamic-space-size 1024 --load solver.lisp \
    --eval "(boggle-init)" \
    --eval "(format t \"saving executable lisp image (this can take a while) ...\")" \
    --eval "(finish-output)" \
    --eval "(proclaim '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))" \
    --eval "(handler-bind ((sb-ext:defconstant-uneql (lambda (c) (abort c)))) (load (compile-file \"solver.lisp\" )))" \
    --eval "(save-lisp-and-die \"../../bin/solve-boards\" :toplevel #'run-solver-from-stdin :executable t :purify t)" \
    --eval "(quit)"


#ccl64 -K utf-8 -n -Q -l solver.lisp \
#    -e "(boggle-init)" \
#    -e "(format t \"saving executable CCL64 lisp image (board solver)...\")" \
#    -e "(save-application \"../../bin/solve-boards\" :prepend-kernel t :toplevel-function #'run-solver-from-stdin)"


popd > /dev/null

echo "Compressing images ..."
gzexe bin/gen-boards
gzexe bin/solve-boards
