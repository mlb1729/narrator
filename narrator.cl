;; -*- mode: fi:common-lisp-mode; package: triple-store-user; -*-

(eval-when (compile load eval)
(require :agraph)
)

(in-package :triple-store-user)

;;;; templates
;;;; a template is a sequence of items which are either literal strings or keys for table lookup
;;;; (note: below, a "pool: is a *collection* of templates, whereas a "narrative" is a *sequence* of templates)

;;; collect the keys used by a template
(defun collect-template-keys (template &optional (keys nil))
  (map nil
    #'(lambda (item)
        (when (table-key? item)
          (pushnew item keys)))
    template)
  keys)

;;; render a template onto a stream using a given table
(defun render-template (&key (stream *standard-output*) template table)
  (with-format-stream stream
    (doseq (item template)
           (when (table-key? item)
             (setf item (table-value table item)))
           (format stream "~a" item))
    (format stream ".  ")
    ))

;;;; A narrative is a sequence of templates

;;; collect all the keys used by all the templates in a narrative
(defun collect-narrative-keys (narrative &optional (keys nil))
  (doseq (template narrative)
         (setf keys (collect-template-keys template keys)))
  keys)

(defun render-narrative (&key (stream *standard-output*) narrative table &allow-other-keys)
  (with-format-stream stream
    (doseq (template narrative)
;;           (terpri stream)
           (render-template :stream stream :template template :table table))))


;;;; narrative generation

;;; A backtracking system is used to generate narratives.
;;; The goal is, given a table (key-value map) and a pool (collection) of templates, 
;;; to produce a narrative using some subset of those templates such that it is 
;;;    1. Renderable: every key in the narrative has a non-nil value in the table
;;;    2. Concise: no key appears more than once in the narrative
;;;    3. Complete: every key in a designated set of required keys (eg "Big 7") appears

;;;  Initially, the narrative is empty and the pool contains some initial set of templates.
;;;  First, remove all templates which reference keys with null values
;;;  Then, repeatedly:
;;;    If the narrative is Complete, the process succeeds [but templates may be added if desired]
;;;    Next, effectively remove every template in the pool that duplicates a key in the narrative
;;;    If the pool becomes empty, the branch fails
;;;      else the pool contains templates: choose one (via some strategy passed as a parameter), 
;;;      remove from the pool and add to the narrative

;;; The “Concise” constraint is to avoid redundant babble like repeating the company's name
;;; or restating the revenues multiple ways.  However it may be OK to substitute "pronouns" or
;;; "generic phrases", such as "the company" or "the revenues", depending on context and usage.

;;; [Note: it's not clear how reversible updates in the backtracker should best be implemented--
;;; Bindings which automatically track the computation, or an explicit "undo stack"?]

;;;; Narrate -- Basic search driver -- returns "proposal", a list of choices satisfying conditions
;;; Keyword API arguments used in this system:
;;;   "resources" -- refers to the entire keyword-rest list passed to some function
;;;   additional key arguments not mentioned here may appear to support passing them inward
;;; :proposal -- the proposal currently under consideration
;;; :pool -- a set of items available for adding to the proposal 
;;; :finished? -- a predicate applied to the "resources" to see if the proposal is done
;;; :chooser -- sig (&key pool &allow-other-keys) function that chooses some element from the pool 
;;; :pruner -- sig (&key pool &allow-other-keys) function that may return filtered reduced pool

(defun narrate (&rest resources)
  (reverse (apply #'make-narrative resources)))

(defun make-narrative (&rest resources &key proposal pool finished? chooser &allow-other-keys)
  (if (apply finished? resources) 
      proposal
    (loop
      (when (null pool) (return nil))
      (let ((choice (apply chooser :pool pool resources)))
        (when (null choice) (return nil))
        (setf pool (remove choice pool)) 
        (let ((solution (apply #'try-choice choice :pool pool resources)))
          (when solution (return solution)))))))

(defun try-choice (choice &rest resources &key proposal pruner &allow-other-keys)
  (push choice proposal)
  (apply #'make-narrative
         :proposal proposal
         :pool (apply pruner :proposal proposal resources)
         resources))

;;; functional generic tests wherein pool is integers and goal is some numeric condition

(defun between? (x lo &optional hi)
  (if (null hi)
      (between? 0 lo)
    (and (>= x lo) (<= x hi))))


(defun test-narrator ()
  (narrate
   :pool '(1 2 3 4 5 6 7 8 9)
   :finished? #'(lambda (&key proposal &allow-other-keys)
                  (between? (apply #'+ proposal) 20 25))
   :chooser #'choose-any
   :pruner #'prune-nothing))

;;; some basic search utilities

(defun choose-first (&key pool &allow-other-keys)
  (first pool))

(defun choose-any (&key pool &allow-other-keys)
  (elt pool (random (length pool))))

;;; David's priority functions

; higher-priorityp returns t if list1 is contains elements of a higher priority 
; than list2 in accordance to the given priorities table; (represented as an ordered list of lists containing elements of shared priority)

(defun higher-priorityp (list1 list2 priorities)
  (if (null priorities) 
      nil    
    (let ((priority-set (car priorities)))      
      (check-within-priority-set list1 list2 priority-set priorities))))

(defun check-within-priority-set (list1 list2 priority-s priorities)  
  (let ((list1-is-of-priority-s (containsp list1 priority-s))        
        (list2-is-of-priority-s (containsp list2 priority-s)))    
    (cond ((and list1-is-of-priority-s (not list2-is-of-priority-s)) t)          
          ((and (not list1-is-of-priority-s) list2-is-of-priority-s) nil)          
          ((and (not list1-is-of-priority-s) (not list2-is-of-priority-s))           
           (higher-priorityp list1 list2 (cdr priorities))))))

;containsp returns t if ls contains any of the elements of set, and nil if otherwise.
(defun containsp (ls set)  
  (if (null set) 
      nil    
    (let ((set-elem (car set)))      
      (if (member set-elem ls)          
          t        
        (containsp ls (cdr set))))))

;;; priority-driven chooser
(defun collect-best-items (pool is-better?-fn)
  (loop
    with collection = nil
    with best-item = nil
    for item in pool
    for new-best = (or (null best-item)
                       (funcall is-better?-fn item best-item))
    when new-best  do (setf 
                       best-item item  
                       collection `(,item))
    when (and (not new-best) (not (funcall is-better?-fn best-item item)))  do (pushnew item collection)
    finally (return collection)))

(defun best-template-fn (priority)
  #'(lambda (t1 t2)
      (higher-priorityp
       (collect-template-keys t1)
       (collect-template-keys t2)
       priority)))

(defun prune-nothing (&key pool &allow-other-keys)
  pool)

;;; actual template-based narrative hacking

(defun satisfactory-narrative? (narrative required-keys)
  (let ((narrative-keys (collect-narrative-keys narrative)))
    (every
     #'(lambda (key)
         (find key narrative-keys))
     required-keys)))

(defun narrative-finished? (&key proposal required-keys &allow-other-keys)
  (satisfactory-narrative? proposal required-keys))

;;; a template is renderable when every key in it has a lookup value
(defun template-renderable? (template table)
  (every
   #'(lambda (key)
       (table-value table key))
   (collect-template-keys template)))

;;; use this to initially filter out all unrenderable templates
(defun renderable-templates (pool table)
  (remove-if-not
   #'(lambda (template) (template-renderable? template table))
   pool))

;;; remove any templates in the pool which have keys already in the narrative
(defun prune-redundant-templates (&key pool proposal &allow-other-keys)
  (let ((narrative-keys (collect-narrative-keys proposal)))
    (remove-if
     #'(lambda (template) (overlap? (collect-template-keys template) narrative-keys))
     pool)))

(defun construct-narrative (&rest resources &key pool table priority &allow-other-keys)
  (let ((pool (renderable-templates pool table))
        (selector (best-template-fn priority)))
    (apply #'narrate
           :pool pool
           :finished? #'narrative-finished?
           :required-keys (collect-narrative-keys pool)
           :chooser #'(lambda (&key pool &allow-other-keys)
                        (choose-any :pool (collect-best-items pool selector)))
;;           :chooser #'choose-any
           :pruner #'prune-redundant-templates
           resources)))

