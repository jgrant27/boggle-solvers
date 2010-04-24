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
;;; Boggle generator / solver utils
;;;
;;; Date : 03/21/2009
;;; Author : Justin Grant
;;;


(defun randomize-sequence (sequence)
  "Fast enough ;-)"
  (loop
   :with vector = (coerce sequence 'vector)
   :for i :from (1- (length vector)) :downto 1
   :do (rotatef (aref vector i) (aref vector (random i)))
   :finally (return (coerce vector (type-of sequence)))))

;; Need to play with this function
;; to determine performance
(defun fisher-yates-shuffle (lst)
  (let ((length (length lst)) (j 0))
    (loop for i from (1- length) downto 1 do
          (setf j (random (1+ i)))
          (psetf (nth i lst) (nth j lst)
                 (nth i lst) (nth j lst)))))

(defun get-n-items (lst num)
  "Get n items from the list"
  (if (> num 0)
      (cons (car lst) (get-n-items (cdr lst) (- num 1)))
    ()))

(defun slice (lst start count)
  "returns a slice from a list"
  (if (> start 1)
      (slice (cdr lst) (- start 1) count)
    (remove nil (get-n-items lst count))))

(defun range (start end)
  (loop for i from start below end collect i))

(defun get-int-arg (str default)
  (parse-integer (if (> (length str) 0) str default)))

(defun concatenate-strings (strings)
  (loop
   :with result = (make-string (loop :for s :in strings :sum (length s)))
   :for s :in strings
   :for start = 0 :then (+ start (length s))
   :do (replace result s :start1 start)
   :finally (return result)))

(defun vector-push-extend* (vector &rest items)
  (let ((element-type (array-element-type vector)))
    (dolist (item items)
      (cond
       ((typep item element-type) ;; item can be put directly into the
        (vector-push-extend item vector))
       ((typep item `(vector ,element-type)) ;; item should be a vector
        (loop
         for i across item
         do (vector-push-extend i vector)))
       (t
        (error "Bad type for item ~S." item))))
    vector))
