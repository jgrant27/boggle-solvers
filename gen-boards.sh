#!/usr/bin/env bash

pushd src/lisp/ > /dev/null

### Compilers in order of speed of generated code (desc)


### Clozure Common Lisp

#ccl64 -b -n -K utf-8 -Q -l cl-boggle.lisp \
#    -e "(setf *COMMAND-LINE-ARGUMENT-LIST* '(\"\" \"$1\" \"$2\" \"$3\"))" \
#    -e "(cl-boggle:run-board-generator)" \
#    -e "(quit)"


### Steel Bank Common Lisp

sbcl --noinform \
    --load cl-boggle.lisp \
    --eval "(setf sb-ext:*posix-argv* '(\"\" \"$1\" \"$2\" \"$3\"))" \
    --eval "(cl-boggle:run-board-generator)" \
    --eval "(quit)"
    #--disable-debugger \


### CMUCL
#lisp -quiet \
#    -load cl-boggle.lisp \
#    -eval "(setf extensions:*command-line-strings* '(\"\" \"$1\" \"$2\" \"$3\"))" \
#    -eval "(cl-boggle:run-board-generator)" \
#    -eval "(quit)"


### ECL (does not compile)

#ecl -s -q -load cl-boggle.lisp \
#    -eval "(setf *ecl-args* '(\"\" \"$1\" \"$2\" \"$3\"))" \
#    -eval "(cl-boggle:run-board-generator)" \
#    -eval "(quit)"


popd > /dev/null
