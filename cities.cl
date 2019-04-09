;; -*- mode: fi:common-lisp-mode; package: triple-store-user; -*-

(eval-when (compile load eval)
(require :agraph)
)

;;;;; get list of big US cities and their lon-lat

(in-package :triple-store-user)

(eval-when (compile load eval)
(enable-!-reader)
)

;; (load (compile-file "~/Devo/allegro/hoovers/code/make-geonames.cl"))

(defparameter *geo* 
  (open-triple-store "~/Devo/allegro/hoovers/data/us.db" :read-only t))

(register-namespace "geo" "http://www.geonames.org/Countries#")
(defparameter *lat-lon-5* (register-latitude-striping-in-miles 5.0s0))
(defparameter *lat-lon-100* (register-latitude-striping-in-miles 100.0s0))

(defun get-city-lon-lat (city state)
  (let (lon lat)
    (prolog
     (lisp ?city (literal city))
     (lisp ?state (literal state))
     (q- ?x !geo:asciiname ?city)
     (q- ?x !geo:admin1_code ?state)
     (q- ?x !geo:isAt5 ?pos)
     (lisp (setf (values lon lat) (upi->longitude-latitude ?pos))))
    (values lon lat)))

(defparameter *city-key* :city)
(defparameter *state-key* :state)

(defparameter *lon-key* :longitude)
(defparameter *lat-key* :latitude)

(defun augment-table-with-city-lon-lat (table)
  (let ((city (table-value table *city-key*))
        (state (table-value table *state-key*)))
    (when (and city state)
      (multiple-value-bind (lon lat)
          (get-city-lon-lat city state)
        (when (and lon lat)
          (setf (values (table-value table *lon-key*) (table-value table *lat-key*)) 
            (values (coerce lon 'single-float) (coerce lat 'single-float))))
        table))))
  
(defun process-city-file (&optional 
                          (table-processor #'dump-table)
                          (file "~/Devo/allegro/hoovers/data/BigUSCities.csv"))
  (with-open-file (in file)
    (let ((cols (map 'list 
                  #'table-key
                  (get-words (read-line in nil nil)))))
;;      (print (length cols))
      (loop
        for line = (read-line in nil nil)
        while line
        for table = (table-from-words cols (get-words line))
        for augment = (augment-table-with-city-lon-lat table)
        do (funcall table-processor augment)))))

(defun get-big-city-tables-from-file (&optional (file "~/Devo/allegro/hoovers/data/BigUSCities.csv"))
  (let ((cities nil))
    (process-city-file
     #'(lambda (table) (push table cities))
     file)
    (nreverse cities)))

(defparameter *big-city-tables* (get-big-city-tables-from-file))

(defun miles-to-city-in-table (table lon lat)
  (let ((city-lon (table-value table *lon-key*))
        (city-lat (table-value table *lat-key*)))
    (and city-lon city-lat
         (haversine-miles lon lat city-lon city-lat))))

(defun nearest-big-city-table (lon lat &optional (tables *big-city-tables*))
  (loop
    with nearest = nil
    with distance = nil
    for table in tables
    for miles = (miles-to-city-in-table table lon lat)
    when (and miles (or (null distance) (< miles distance)))
    do (setf (values nearest distance) (values table miles))
    finally (return (values nearest distance))))

(defun mileage-string (miles)
  (let ((tenth-miles (round miles 0.1)))
    (multiple-value-bind (whole-miles tenths) (floor tenth-miles 10)
      (format nil "~d~@[.~d~]" 
        whole-miles 
        (and (> tenths 0) tenths)))))

(defun nearest-city-string (lon lat &optional (tables *big-city-tables*))
  (multiple-value-bind (table miles)
      (nearest-big-city-table lon lat tables)
    (format nil "about ~a miles from central ~a, ~a"
      (mileage-string miles)
      (table-value table *city-key*)
      (table-value table *state-key*))))

(defparameter *nearest-city-key* :near_metro)

(defun augment-table-with-nearest-city-string (table &optional (tables *big-city-tables*))
  (let ((lon (table-value table *lon-key*))
        (lat (table-value table *lat-key*)))
    (when (and lon lat)
      (let ((string (nearest-city-string 
                     (read-from-string lon) 
                     (read-from-string lat) 
                     tables)))
        (when string
;;          (print string)
          (setf (table-value table *nearest-city-key*) string)))))
  table)


