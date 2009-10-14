#!/usr/bin/env bash

pushd src/lisp/ > /dev/null

### Compilers in order of speed of generated code (desc)


### Clozure Common Lisp

#ccl64 -b -n -K utf-8 -Q -l cl-boggle.lisp \
#   -e "(setf *COMMAND-LINE-ARGUMENT-LIST* '(\"\" \"$1\" \"$2\" \"$3\"))" \
#   -e "(cl-boggle:boggle-init)" \
#   -e "(cl-boggle:run-solver-from-stdin)" \
#   -e "(quit)"


### Steel Bank Common Lisp

sbcl --dynamic-space-size 1024 --noinform \
    --load cl-boggle.lisp \
    --eval "(setf sb-ext:*posix-argv* '(\"\" \"$1\" \"$2\" \"$3\"))" \
    --eval "(cl-boggle:boggle-init)" \
    --eval "(cl-boggle:run-solver-from-stdin)" \
    --eval "(quit)"
    #--disable-debugger \
    #--eval "(cl-boggle:run-solver-test)" \
    #--eval "(cl-boggle:run-solver-from-stdin)" \


### CMUCL (loading dict takes a long time and the solver is much slower than other lisps)
#lisp -quiet -dynamic-space-size 1024 \
#    -load cl-boggle.lisp \
#    -eval "(setf extensions:*command-line-strings* '(\"\" \"$1\" \"$2\" \"$3\"))" \
#    -eval "(cl-boggle:boggle-init)" \
#    -eval "(cl-boggle:run-solver-from-stdin)" \
#    -eval "(quit)"


popd > /dev/null
