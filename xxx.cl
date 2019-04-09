;; -*- mode: fi:common-lisp-mode; package: triple-store-user; -*-

(eval-when (compile load eval)
(require :agraph)
)

;;;;; temp. file for experimenting

(in-package :triple-store-user)

(enable-!-reader)

;; (load (compile-file "~/Devo/allegro/hoovers/code/make-geonames.cl"))

(<-- (print-triple ?x)
  (lisp (progn
	  (print '-------)
	  (print ?x))))

(<-- (pprint-subject ?x)
  (lisp (progn
	  (print '-------)
	  (terpri)
	  (pprint-subject ?x :format :concise))))

#||
(select0 (?x)
  (q- ?x !geo:asciiname !"Kansas City")
  (q- ?x !geo:admin1_code (literal "KS"))
  (print-triple ?x))

(select0 (?x)
  (q- ?x !geo:asciiname !"Kansas City")
  (q- ?x !geo:admin1_code !"MO")
  (pprint-subject ?x))
||#

(<-- (pos->lon/lat ?pos ?lon ?lat)
     (lisp (?lon ?lat) 
           (multiple-value-list (upi->longitude-latitude ?pos))))

(<-- (print-pos ?pos)
     (lisp (?lon ?lat) 
           (print (multiple-value-list (upi->longitude-latitude ?pos)))))


#||
;; Novato's wiki coords are 38°06′27″N 122°34′11″W

;; this prints (-98.53412584175084d0 75.04265223063973d0) WRONG!S
(select0 (?x)
  (q- ?x !geo:asciiname !"Novato")
  (q- ?x !geo:admin1_code !"CA")
  (print-pos ?x))

;;; this prints (-122.5697021043771d0 38.107421843434345d0) RIGHT!
(select0 (?x)
  (q- ?x !geo:asciiname !"Novato")
  (q- ?x !geo:admin1_code !"CA")
  (q- ?x !geo:isAt5 ?pos)
  (print-pos ?pos))

;; Dayton's wiki coords are 39°45′32″N 84°11′30″W
;; this prints (-137.4268408249158d0 0.7838429713804714d0) 
(select0 (?x)
  (q- ?x !geo:asciiname !"Dayton")
  (q- ?x !geo:admin1_code !"OH")
  (print-pos ?x))

(<-- (geo-distance ?x ?y ?dist)
     (ground ?x)(ground ?y)(not (ground ?dist)) \!
     (q- ?x !geo:isAt5 ?pos1)
     (q- ?y !geo:isAt5 ?pos2)
     (pos->lon/lat ?pos1 ?lon1 ?lat1)
     (pos->lon/lat ?pos2 ?lon2 ?lat2)     
     (lisp ?dist (haversine-miles ?lon1 ?lat1 ?lon2 ?lat2)))

;; this returns (("2048.9044529399603d0"))
;; Google Maps says they are 2352 miles "as the car drives"
(select (?dist)
        (q- ?x !geo:name !"Novato")
        (q- ?x !geo:admin1_code !"CA")
        (q- ?y !geo:name !"Dayton")
        (q- ?y !geo:admin1_code !"OH")
  (geo-distance ?x ?y ?dist))

;;; Las Vegas vs North Las Vegas

(select0 (?x)
  (q- ?x !geo:asciiname !"Las Vegas")
  (q- ?x !geo:admin1_code !"NV")
  (print-pos ?x))
;; this returns (-108.63529915824915d0 16.54389095117845d0)
;; Las Vegas's wiki coords are 36°10′30″N 115°08′11″W
;; spreadsheet coords are 36.1749705	-115.137223

(select0 (?x)
  (q- ?x !geo:asciiname !"North Las Vegas")
  (q- ?x !geo:admin1_code !"NV")
  (print-pos ?x))
;; this returns (110.97478013468013d0 -45.24718956228956d0)
;; North Las Vegas's wiki coords are 36°13′43″N 115°8′48″W
;; spreadsheet coords are 36.1988592	-115.1175013

(select (?dist)
        (q- ?x !geo:name !"Las Vegas")
        (q- ?x !geo:admin1_code !"NV")
        (q- ?y !geo:name !"North Las Vegas")
        (q- ?y !geo:admin1_code !"NV")
  (geo-distance ?x ?y ?dist))
;; Google Maps says the named places are 8.2 miles "as the car drives"
;; It says the spreadsheet delta is 2.7mi; however taking a diagonal gives ~1.9
;; this returns (("1.9824648541055712d0"))
||#