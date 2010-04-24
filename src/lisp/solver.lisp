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
;;;
;;; A Boggle Generator/Solver
;;;
;;; Author : Justin Grant
;;; Date : 03/21/2009
;;;
;;; Takes a list of boards
;;; and solves them checking for valid words using a
;;; dictionary. Also shows maximum board scores.
;;; (Uses a Trie / Prefix Tree to store and find words.)
;;;


;; prefs

;;#+sbcl (require :sb-sprof)

(declaim (optimize (speed 3)
                   (space 0)
                   (safety 0)
                   (debug 0)
                   (compilation-speed 0)))

;;(setf (bytes-consed-between-gcs) (* 128 1024 1024))

;; global consts
(defvar *dict-path*
  (concatenate 'string
               (directory-namestring *load-truename*)
               "../../dict/english_270k.txt"))

(defvar *test-board-dimensions* '(100 100))
(defconstant +min-word-length+ 3)
(defparameter *max-search-depth* 16)

(defvar *dict-trie*)

;; functions
(defun boggle-init()
  "Initialize things"

  (format t "loading dictionary ~A ..." *dict-path*)
  (finish-output)

  (setf *dict-trie* (trie-from-dict (make-trie) *dict-path*))
  (setf *max-search-depth* (trie-depth *dict-trie*))

  (format t "done.~%")
  (finish-output)

  (format t "loaded ~D words.~%" (trie-word-count *dict-trie*)))


(defun find-words (ltr word fi tr)
  "Recursively find words by searching dictionary"
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (declare
   (type (simple-array character (*)) word)
   (type trie tr)
   (type letter ltr)
   (type (unsigned-byte 8) fi))

  ;; the letter as used in the word
  (setf (letter-used ltr) t)

  (let ((cc (letter-value ltr)))
    (declare (type character cc))

    ;; create a new word according to the search depth
    (setf (aref word fi) cc)
    (setq fi (+ fi 1))
    (setf (aref word fi) #\Null)

    ;; handle Q's
    (when (eql cc #\Q)
      (setq cc #\U)
      (setf (aref word fi) cc)
      (setq fi (+ fi 1))
      (setf (aref word fi) #\Null)
      (setq tr (get-child tr #\Q)))

    ;; Any words in the trie ?
    (when (and (>= (length word) +min-word-length+)
               (trie-include-prune-p tr word))
      (setf (aref word fi) #\Newline)
      (write-string word nil :end (1+ fi)))

    (let ((child (aref (trie-children tr)
                       (- (char-code cc) (char-code #\A)))))
      ;; try longer words with neighbors
      (if (and (not (eql child 0))
               (<= fi (trie-level child)))
          (let ((neighbors (letter-neighbors ltr)))
            (dotimes (i (length neighbors))
              (let ((neighbor (aref neighbors i)))
                #+ccl (declare (type letter neighbor))
                (if (not (letter-used neighbor))
                    (find-words neighbor word fi child)))))))

    ;; the letter is no longer in use
    (setf (letter-used ltr) nil)))

(defun run-solver (stream)
  (let* ((the-board
          (create-board
           (board-from-stream stream)))
         (word (make-array 20
                           :initial-element #\Null
                           :element-type 'character))
         (board-letters (board-letters the-board)))
    (declare (type board the-board))
    ;; Print out the board config
    (print-board-config-text (board-config the-board))
    (format t "~%")

    (time
     (progn
       (dotimes (i (board-rows the-board))
         (let ((row (aref board-letters i)))
           (declare (type simple-vector row))
           (dotimes (j (board-cols the-board))
             (let ((letter (aref row j)))
               (find-words
                letter
                word
                0
                *dict-trie*)))))))))

(defun run-solver-from-stdin ()
  "reads boards on stdin and solves them. A word per line is output.
   An empty line separates the list of words for a board."
  (run-solver *standard-input*))

(defun run-solver-on-file (file)
  (with-open-file (stream file :direction :input
                          :element-type 'character)
                  (run-solver stream)))
