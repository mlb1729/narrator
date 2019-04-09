;; -*- mode: fi:common-lisp-mode; package: triple-store-user; -*-

(eval-when (compile load eval)
(require :agraph)
)

(in-package :triple-store-user)

;;;;; utilities

;;;; miscellany

(defmacro with-format-stream (stream-var &body body)
  (let ((continuation (gensym "with-format-stream-continuation-")))
    `(let ((,continuation #'(lambda (,stream-var) . ,body)))
       (if ,stream-var
           (funcall ,continuation ,stream-var)
         (with-output-to-string (,stream-var)
           (funcall ,continuation ,stream-var)
           ,stream-var)))))

(defmacro doseq ((item-var sequence) &body body)
  `(map nil #'(lambda (,item-var) . ,body) ,sequence))

(defun overlap? (r s)
  (some #'(lambda (q) (find q s)) r))

;;;; tables

;;;; a table maps keys to values
;;;; (formerly "table" was "dictionary" which was too typo-prone!)

(defclass table ()
  ((hashtable :accessor hashtable :initform (make-hash-table))
   (parent :accessor parent :initform nil)))

(defun make-table (&rest initargs)
  (apply #'make-instance 'table initargs))

(defmethod clear-table (table)
  (clrhash (hashtable table)))

(defun table-key? (item)
  (symbolp item))

(defparameter *table-key-external-characters* " ().")
(defparameter *table-key-narrator-characters* "_<>~")

(defun map-chars (from to string)
  (map 'string
    #'(lambda (char)
        (let ((position (position char from)))
          (if position
              (char to position)
            char)))
    (string string)))

(defun table-key (item)
  (if (table-key? item)
      item
    (intern
     (narrator-table-key-string item)
     :keyword)))

(defun narrator-table-key-string (item)
  (string-downcase
   (map-chars 
    *table-key-external-characters*
    *table-key-narrator-characters*
    (string-trim " " item))))

(defun external-table-key-string (item)
  (string-capitalize
   (map-chars 
    *table-key-narrator-characters*
    *table-key-external-characters*
    (table-key item))))

(defmethod table-value (table key)
  (or (gethash key (hashtable table))
      (let ((parent (parent table)))
        (and parent
             (table-value parent key)))))

(defmethod (setf table-value) (value table key)
  (setf (gethash key (hashtable table)) value))

;;; utility to load up a table from an assoc-list
(defun load-table (table assoc-list)
  (dolist (pair assoc-list)
    (destructuring-bind (key &optional value &rest ignore)
        pair
      (declare (ignore ignore))
      (setf (table-value table key) value)))
  table)

(defun dump-table (table)
  (format t "~&")
  (maphash
   #'(lambda (k v) (format t "~a:~a; " (external-table-key-string k) v))
   (hashtable table))
  (terpri))

;;;; stuff for parsing CSV files

;;; hacking up Jans's code for now

(defun find-next-comma (string start)
  (let ((i start)
	(ch nil)
	(in-quote nil))
    (loop
      (when (= i (length string))
	(return (values (subseq string start i) nil)))
      (setf ch (schar string i))
      (cond ((char= ch #\")
	     (if* (char= (schar string (1+ i)) #\")
		     then (incf i)
		     else (setf in-quote (not in-quote))))
	    ((char= ch #\,)
	     (if* in-quote
		then nil
		else (return (values (subseq string start i) (incf i))))))
      (incf i))))

(defun get-words (line)
  (let ((next 0)
	(word)
        (res))
    (setf line (string-trim '(#\Newline #\Linefeed #\Return) line))
    (loop
      (setf (values word next)
	(find-next-comma line next))
      (push (string-trim '(#\") word) res)
      (unless next
        (return)))
      (nreverse res)))

;;; load list of words into keyed structures

(defun assoc-from-words (keys words)
  (loop 
    for word in words
    for key in keys
    unless (string= word "")
    collect `(,key ,word)))

(defun table-from-words (keys words &optional (table (make-table)))
  (clear-table table)
  (loop 
    for word in words
    for key in keys
    unless (string= word "")
    do (setf (table-value table key) word))
  table)


