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
;;; Boggle Generator/Solver structs and functions
;;;
;;; Author : Justin Grant
;;; Date : 03/21/2009
;;;

;; consts

(defconstant +empty-children+ (make-array 30 :element-type 'trie))

;; structs

(defstruct (letter
             #+nil (:print-function print-letter))           
  ;; represents a letter on the board.
  (value nil :type character)
  (neighbors #() :type simple-vector)
  (used nil :type boolean))

(defstruct (board)
  ;; represents a boggle board as a 2d list of letters.
  (letters nil :type simple-vector) 
  (rows nil :type fixnum) 
  (cols nil :type fixnum) 
  (config nil :type list))

(deftype trie-children-vector () '(simple-vector 30))

(defstruct (trie)
  ;; data structure used to load 
  ;; and search the dictionary.
  (level 0 :type fixnum) 
  (depth 0 :type integer)
  (count 0 :type integer)
  (word-count 0 :type number)
  (is-word nil :type boolean)
  (text "" :type string)
  (parent nil);;:type trie)
  (children (make-array 30) :type trie-children-vector))

;; struct functions

(defun print-letter (l stream level)
  (declare (ignore level))
  (format 
   stream
   "#S(LETTER :value ~A :used ~A :neighbors '~A)"
   (letter-value l) 
   (letter-used l)
   (map 'list
        #'(lambda (nletter) 
            (letter-value nletter))
        (letter-neighbors l))))

(defun create-board (config)
  ;; creates the board and sets up the letters
  (let* ((board
          (make-board 
           :letters 
           (map 'vector
                #'(lambda (row)
                    (map 'vector 
                         #'(lambda (letter) 
                             (make-letter
                              :value (char letter 0))) row)) config)
           :rows (length config)
           :cols (length (car config))
           :config config))
         (bletters (board-letters board)))
    
    ;; set up the neighbors for each letter
    (dotimes (y (board-rows board))
      (dotimes (x (board-cols board))
        (loop with letter = (aref (aref bletters y) x)
           with neighbors = nil
           with ymax-ind = (board-rows board)
           with xmax-ind = (board-cols board)
           for offsety from -1 to 1
           do (let ((yoi (+ y offsety)))
                (when (and (>= yoi 0) (< yoi ymax-ind))
                  (let ((row (aref bletters yoi)))
                    (loop for offsetx from -1 to 1
                       unless (and (eql offsety 0) (eql offsetx 0))
                       do (let ((xoi (+ x offsetx)))
                            (when (and (>= xoi 0) (< xoi xmax-ind))
                              ;; now add the neighbor
                              (push (aref row xoi) neighbors)))))))
           finally (setf (letter-neighbors letter)
                         (coerce (nreverse neighbors) 'simple-vector)))))
    board))


(defun curr-letter (tr str)
  "returns the current letter code for the level"
  (declare (type trie tr)
           (type string str))
  (let ((level (trie-level tr)))
    (if (> (length str) level)
        (char-code (aref str level))
        0)))

(defun inc-word-count (tr)
  (declare (type trie tr))
  (let ((parent (trie-parent tr)))
    (declare (type (or null trie) parent))
    (when parent
      (setf (trie-word-count parent) (+ (trie-word-count parent) 1))
      (inc-word-count parent))))

(defun dec-word-count(tr)
  (declare (type trie tr))
  (let ((parent (trie-parent tr)))
    (declare (type (or null trie) parent))
    (when parent
      (setf (trie-word-count parent) (- (trie-word-count parent) 1))
      (dec-word-count parent))))

(defun trie-insert (tr str)
  "inserts the string into the trie"
  (declare (type trie tr)
           (type string str))
  (let* ((letter (curr-letter tr str)))
    ;;((letter (aref str (trie-level tr))))
    (if (eql letter 0)
        (progn
          (setf (trie-is-word tr) t)
          (setf (trie-text tr) str)
          (inc-word-count tr))
        (progn 
          (let* ((ind (- letter (char-code #\A)))
                 (children (trie-children tr))
                 (ttr (aref children ind)))
            (declare (type (or (unsigned-byte 8) trie) ttr)
                     (type trie-children-vector children))
            (if (eql ttr 0)
                (progn 
                  (setf ttr (make-trie :level (+ (trie-level tr) 1)))
                  (setf (trie-parent ttr) tr)))
            (setf (aref children ind) ttr)
            (trie-insert ttr str))))))

(defun trie-include-p (tr str)
  "Tests whether the str is a complete word in the trie"
  
  (declare (type trie tr)
           (type string str))
  (let* ((byte (aref str (trie-level tr)))
         (letter (char-code byte)))
    (if (eql letter 0)
        (progn
          (if (equalp (trie-text tr) nil) 
              nil
              (string-equal (trie-text tr) str)))
        (progn
          (let* ((ind (- letter (char-code #\A)))
                 (children (trie-children tr))
                 (ttr (aref children ind)))
            (declare (type (unsigned-byte 8) ind))
            (and (not (eql ttr 0)) 
                 (trie-include-p ttr str)))))))

(defun trie-include-prune-p (tr str)
  "Tests whether the str is a complete word in the trie.
   If true then the word is pruned from the trie."
  (declare (type trie tr)
           (type (simple-array character (*)) str))
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (let* ((byte (aref str (trie-level tr)))
         (letter (char-code byte))) 
    (if (eql letter 0)
        (if (eql (trie-text tr) nil)
            nil
            (prog1 (trie-is-word tr)
              (setf (trie-is-word tr) nil)))
        (let* ((ind (- letter (char-code #\A)))
               (children (trie-children tr))
               (ttr (aref children ind)))
          (declare (type (unsigned-byte 8) ind)
                   (type trie-children-vector children)
                   (type (or (unsigned-byte 8) trie) ttr))
          (and (not (eql ttr 0))
               (trie-include-prune-p ttr str))))))

(defun trie-begin-p (tr str)
  "Tests whether any words exist in the trie that begin with str"
  (declare (type trie tr)
           (type string str))
  (let* ((byte (aref str (trie-level tr)))
         (letter (char-code byte))) 
    (if (eql letter 0)
        (not (eql nil (trie-children tr)))
        (let* ((ind (- letter (char-code #\A)))
               (children (trie-children tr))
               (ttr (aref children ind)))
          (and (not (eql ttr 0)) (trie-begin-p ttr str))))))

(defun get-child (tr ch)
  (declare (type trie tr)
           (type character ch))
  (let* ((ind (- (char-code ch) (char-code #\A)))
         (tt (aref (trie-children tr) ind)))
    (declare (type (or trie (integer 0 0)) tt)
             (type (unsigned-byte 8) ind))
    (when (eql 0 tt)
      (setf tt (make-trie :level (+ (trie-level tr) 1)))
      (setf (aref (trie-children tr) ind) tt)
      (setf (trie-parent tt) tr))
    tt))

(defun board-from-stream (istr)
  "Reads in a board from an input stream returning a string"
  (let ((board (make-array 0 
                           :fill-pointer 0 
                           :element-type 'list 
                           :adjustable t))
        (row (make-array 0 
                         :fill-pointer 0 
                         :element-type 'string
                         :adjustable t))
        (prev-char nil))
    (with-open-stream (s istr)
      (do ((char (read-char s nil)
                 (read-char s nil)))
          ((null char))
        (if (eql char #\Newline) 
            (progn
              (if (> (length row) 1)
                  (vector-push-extend (coerce row 'list) board))
              (setq row (make-array 0 
                                    :fill-pointer 0 
                                    :element-type 'string 
                                    :adjustable t)))
            (progn
              (if (and (alpha-char-p char) 
                       (not (and (eql char #\U) (eql prev-char #\Q))))
                  (vector-push-extend (format nil "~A" char) row))))
        (setf prev-char char)))
    (coerce board 'list)))

(defun trie-from-dict (tr fpath)
  "Read in a dictionary file to create the trie"
  
  (declare (type trie tr))
  (declare (type string fpath))
  
  (with-open-file (stream fpath)
    (declare (type stream stream))
    (let ((depth 0))
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (let ((cline (string-upcase (string-trim '(#\Space #\Tab) line))))
          (if (> (length cline) depth) (setf depth (length cline)))
          (trie-insert tr cline)))
      (setf (trie-depth tr) depth))) tr)
