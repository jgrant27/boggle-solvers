;;; Copyright  (c) 2009, Justin Grant <justin at imagine27 dot com>
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the <organization> nor the
;;;       names of its contributors may be used to endorse or promote products
;;;       derived from this software without specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY <copyright holder> ''AS IS'' AND ANY
;;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;
;;;
;;;
;;;
;;; Boggle Generator/Solver generation vars and functions
;;;
;;; Author : Justin Grant
;;; Date : 03/07/2009
;;;


;; vars

(defvar *boggle-4x4-die*
  ;; Standard die for a 4 X 4 boggle board
  '("FORIXB" "MOQABJ" "GURILW" "SETUPL"
    "CMPDAE" "ACITAO" "SLCRAE" "ROMASH"
    "NODESW" "HEFIYE" "ONUDTK" "TEVIGN"
    "ANEDVZ" "PINESH" "ABILYT" "GKYLEU"))

;; functions

(defun get-die (&key (dimensions '(4 4)))
  "Gets the die for the dimensions"
  (let* ((number-of-die (* (first dimensions)
                           (second dimensions)))
         (die-list (make-array 0
                               :element-type '(unsigned-byte *)
                               :fill-pointer 0
                               :adjustable t)))
    (if (> (length *boggle-4x4-die*)
           number-of-die)
        (subseq *boggle-4x4-die* 0 number-of-die)
      (progn
        (dotimes (n (ceiling
                     (/ number-of-die
                        (length *boggle-4x4-die*))))
          (dolist (o *boggle-4x4-die*)
            (vector-push-extend o die-list)))))
    (setf (fill-pointer die-list) (- number-of-die 1))

    ;; now create the 2d list
    (map 'list
         #'(lambda (die-str)
             (map 'list
                  #'(lambda (c)
                      (string c)) die-str))
         die-list)))


(defun new-board-config(&key (dimensions '(4 4)))
  ;; generates a new 2d board based on the dimensions
  ;; randomly arrange and roll the die
  (let* ((rolled-die
          (randomize-sequence
           (get-die :dimensions dimensions))) ; randomly arrange die
         (board (make-sequence 'list (first dimensions))))
    ;; Generate the board config from the rolled die
    (dotimes (num (first dimensions) board)
      (setf (nth num board)
            (map 'list
                 #'(lambda (die)
                     (nth (random (length die)) die)) ; roll the die
                 (slice rolled-die
                        (if (= 0 num)
                            0
                          (+ (* num (second dimensions) 1)))
                        (second dimensions)))))
    board))

(defun print-board-config-text (config)
  (format t "~%~{~{~A~}~%~}" config))

;;(setq ext:*help-message* "
;;gen-boards [--help | -?] board_count rows cols
;;
;;     Generates the boggle board/s (output as text) according to the params.
;;")

#+ecl (defconstant +ls-rules+
        '(("--help" 0 (progn (princ ext:*help-message* *standard-output*) (ext:quit 0)))
          ("-?" 0 (progn (princ ext:*help-message* *standard-output*) (ext:quit 0)))
          ("*DEFAULT*" 1 nil)))
#+ecl (setf *ecl-args* nil)
#+ecl (ext:process-command-args :rules +ls-rules+ :args *ecl-args*)

(defun run-board-generator ()
  ;; Main entry point if run from command line
  (setf *random-state* (make-random-state t))
  (let*
      (
       #+ecl (args *ecl-args*)
             #+ccl (args *COMMAND-LINE-ARGUMENT-LIST*)
             #+sbcl (args sb-ext:*posix-argv*)
             #+cmu (args extensions:*command-line-strings*)
             (count (get-int-arg (second args) "1"))
             (rows (get-int-arg (third args) "4"))
             (cols (get-int-arg (fourth args) "4")))

    (dotimes (n count)
      (print-board-config-text
       (new-board-config :dimensions (list rows cols))))))
