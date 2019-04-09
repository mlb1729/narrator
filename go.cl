;; -*- mode: fi:common-lisp-mode; package: triple-store-user; -*-

(eval-when (compile load eval)
(require :agraph)
)

(in-package :triple-store-user)

(load "~/Devo/allegro/hoovers/gruff.fasl")
;; (load (compile-file "~/Devo/allegro/hoovers/code/utility"))
;; (load (compile-file "~/Devo/allegro/hoovers/code/narrator"))
;; (load (compile-file "~/Devo/allegro/hoovers/code/triples"))

;;; simple tests

(defparameter *test-table*
    (load-table
     (make-table)
     `((:company_name "Zulch Auto Werks")
       (:line_of_business "primered caruchas")
       (:revenue_<$_million> 6.9)
       (:contact_last_name "Z. Pinhead"))))
  
(eval-when (compile load eval)
(defparameter *test-template*
  `(:company_name " supplies " :line_of_business ", with an annual revenue of about $" :revenue_<$_million> "M."))
)

(defun test-render-template (&key (stream *standard-output*) 
                                  (template *test-template*)
                                  (table *test-table*))
  (render-template :stream stream :template template :table table))

(defparameter *test-pool*
    `(,*test-template*
      (:company_name " of " :primary_city ", " :primary_state " is a $" :revenue_<$_million> "M " :line_of_business " leader")
      (:company_name " is a player in the " :primary_industry " segment")
      ("The company offers " :line_of_business " in the " :primary_industry " industry")
      (:company_name " has an annual revenue of about $" :revenue_<$_million> "M")
      (:company_name " supplies " :line_of_business)
      ("A leading vendor of " :line_of_business " is " :company_name)
      (:company_name " boasts yearly gross sales of " :revenue_<$_million> " million dollars")
      ("A " :primary_industry " leader, the company books about $" :revenue_<$_million> "M")
      ("The company has an annual revenue of about $" :revenue_<$_million> "M")
      ("The company supplies " :line_of_business)
      ("Contact " :contact_prefix ". " :contact_last_name " for more info about the company")
      ("The company contact is " :contact_prefix ". " :contact_last_name)
      ("Headquartered in " :primary_city ", " :primary_state ", the company employs " :total_employees)
      ("This " :line_of_business " company has its home office in " :primary_city ", " :primary_state)
      ("The workforce of " :total_employees " produces " :line_of_business)
      ("The company's headcount is " :total_employees)
      ("Based in " :primary_city ", " :primary_state ", " :company_name " is " :total_employees " strong")
      ("Located near " :primary_city ", " :primary-state ", the company has " :total_employees " employees")
      ;; ("Coordinates are " :latitude " latitude, " :longitude " longitude")
      (:company_name " of " :primary_city ", " :primary_state ", " :near_metro " is a $" :revenue_<$_million> "M " :line_of_business " leader")
      ("Headquartered in " :primary_city ", " :primary_state ", " :near_metro", the company employs " :total_employees)
      ("This " :line_of_business " company has its home office in " :primary_city ", " :primary_state ", " :near_metro)
      ("Based in " :primary_city ", " :primary_state ", " :company_name " is " :near_metro) 
      ("The company is " :near_metro)
      ))
                     
(defparameter *key-priority*
    `((:company_name) (:revenue_<$_million> :line_of_business)))
                     
(defun test-render (&rest resources 
                          &key (pool *test-pool*) (table *test-table*) (priority *key-priority*)
                          &allow-other-keys)
  (apply #'render-narrative 
         :narrative (apply #'construct-narrative
                           :pool pool
                           :table table
                           :priority priority
                           resources)
         :table table
         resources))

(defun test ()
  (test-render-template))

(defun read-cols-and-row (file)
  (with-open-file (in file)
    (let ((cols (map 'list 
                #'table-key
                (get-words (read-line in nil nil)))))
      (table-from-words cols (get-words (read-line in nil nil))))))

(defun test-process-file (&optional (n 1) (file "~/Devo/allegro/hoovers/data/small_companies.csv"))
  (with-open-file (in file)
    (let ((cols (map 'list 
                  #'table-key
                  (get-words (read-line in nil nil)))))
      (loop
        repeat n
        for table = (table-from-words cols (get-words (read-line in nil nil)))
        do (terpri)
        do (test-render :table table)
        do (dump-table table)))))

(defun test-triple-file (&optional (n 1) (file "~/Devo/allegro/hoovers/data/small_companies.csv"))
  (with-open-file (in file)
    (let ((cols (map 'list 
                  #'table-key
                  (get-words (read-line in nil nil)))))
;;      (print cols)
      (loop
        repeat n
        for table = (table-from-words cols (get-words (read-line in nil nil)))
;;        do (dump-table table)
        do (add-table table)))))

(defun test-render-triples (&optional (stream *standard-output*))
  (let ((table (make-table)))
    (map-subjects
     #'(lambda (subject)
         (get-table-for-subject subject table)
         (augment-table-with-nearest-city-string table)
         (format stream "~&-------- ~a --------" subject)
         (test-render :table table :stream stream)
;;         (dump-table table)
         ))))  

(defun file-render-triples (&optional (file "~/Devo/allegro/hoovers/data/narratives.csv"))
  (with-open-file (out file 
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (let ((table (make-table)))
      (map-subjects
       #'(lambda (subject &aux (value (upi->value subject)) (id (subseq value (+ (position #\# value) 1))))
;;          (format t "~&|~a|~%" id)
           (get-table-for-subject subject table)
           (augment-table-with-nearest-city-string table)
           (format out "~&~a~c~c" id #\, #\")
           (test-render :table table :stream out)
           (format out "~c~%" #\")
           ;;         (dump-table table)
           )))))

