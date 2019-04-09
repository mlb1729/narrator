;; -*- mode: fi:common-lisp-mode; package: triple-store-user; -*-

(eval-when (compile load eval)
(require :agraph)
)

(in-package :triple-store-user)

(defparameter *default-triple-store-name* "~/Devo/allegro/hoovers/data/narrator-test")

(defun open-narrator-triple-store (&rest rest &key (name *default-triple-store-name*) (if-exists :open) &allow-other-keys)
  (apply #'create-triple-store
         name
         :if-exists if-exists
         rest))

(defvar *narrator-triple-store* nil)

(defun get-narrator-triple-store ()
  (unless (and *narrator-triple-store*
               (triple-store::open-p *narrator-triple-store*))
    (setf *narrator-triple-store* (open-narrator-triple-store)))
  *narrator-triple-store*)

(register-namespace "hh" "http://www.fxpt.com/hoovers/Narrator#"  
                    :errorp nil)

(defmacro stringify (x)
  (let ((thing (gensym "stringify-arg-")))
    `(let ((,thing ,x))
       (if (stringp ,thing)
           ,thing
         (format nil "~a" ,thing)))))

(defun subject-node (subject)
  (resource (stringify subject) "hh"))

(defun property-node (property)
  (resource (stringify property) "hh"))

(defun value-literal (value)
  (literal (stringify value)))

(defun add-narrator-triple (subject predicate object)
  (add-triple subject predicate object :db (get-narrator-triple-store)))

(defun add-property (subject property value)
  (add-narrator-triple
   (subject-node subject)
   (property-node property)
   (value-literal value)))

(defparameter *default-subject-key* :d-u-n-s_number)

(defun add-table (table &optional (subject-key *default-subject-key*))
  (let* ((subject-value (table-value table subject-key))
         (subject-node (subject-node subject-value)))
    (maphash 
     #'(lambda (key value)
;;         (print key)
           (add-narrator-triple
            subject-node
            (property-node key)
            (value-literal value)))
     (hashtable table))))

;;;; ToDo MLB: this needs to control *db*...

(defun get-triples-for-value (value &optional (property *default-subject-key*))
  (select (?subject) (q- ?subject (?? (property-node property)) (?? (value-literal value)))))
  
(defun get-triples-for-subject (subject)
  (select (?property ?value) (q- (?? (subject-node subject)) ?property ?value)))

(defun get-subjects (&optional (subject-key *default-subject-key*))
  (let ((property-node (property-node subject-key)))
    (select0 (?subject ?value)
      (q- ?subject (?? property-node) ?value))))


;; should probably change this to use cursors...
(defun map-subjects (&optional (fn #'print) (subject-key *default-subject-key*))
  (let ((property-node (property-node subject-key)))
    (select0-distinct/callback (?subject ?value)
                      #'(lambda (solution)
                         (funcall fn (first solution)))
                      (q- ?subject (?? property-node) ?value))))

(defun printn (&rest args)
  (loop 
    for arg in args
    do (format t "~a " arg))
  (terpri))

(defun map-properties-for-subject (subject-node &optional (fn #'printn))
  (select0-distinct/callback (?property ?value)
                             #'(lambda (solution)
                                 (apply fn solution))
                             (q- (?? subject-node) ?property ?value)))

(defun show-subjects ()
  (map-subjects
   #'(lambda (subject)
       (format t "~%:::::::: ~a ::::::::~%" subject)
       (map-properties-for-subject subject)
       (format t "~&"))))

(defun get-table-for-subject (subject-node &optional (table (make-table)))
  (clear-table table)
  (select0-distinct/callback (?property ?value)
                             #'(lambda (solution)
;;                                 (print solution)
                                 (destructuring-bind (property value)
                                     solution
                                   (let ((key (table-key (part->terse property))))
                                   (setf (table-value table key) 
                                     (part->terse value)))))
                             (q- (?? subject-node) ?property ?value))
  table)

