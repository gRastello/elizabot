(defpackage :elizabot/pattern-matching
  (:use :cl)
  (:import-from :uiop)
  (:export :match))
(in-package :elizabot/pattern-matching)

;;; Elizabot

;;; Pattern matching.
;;;
;;; A pattern is a list of strings that contains "variables" such as
;;; (?x "is" "a" ?y)
;;; variables are symbols that start with a '?'. Each variable matches exactly
;;; one strubg in the input list. So matching (?x "is" "a" ?y) against
;;; ("foo" "is" "a" "bar") yield the bindings alist ("foo" "is" "a" "bar").
;;;
;;; Errors are perpetuated through secondaty return values for now.
;;;
;;; Some sort of jolly character matching is also possible
;;; ((?x *) "is" "a" "fool")
;;; will match every list that ends with ("is" "a" "fool"). Something like
;;; ((?x *) "is" "a" ?y)
;;; will match every sentence that describes something as something else (that is
;;; exactly a single word). This sord of arbitrary segment matching makes error
;;; reporting a bit harder so if something bad happens while matching a segment a
;;; 'segment-match-fail is returned and the the user has to figure out why it
;;; didn't worked.

(defun variable-p (x)
  "A variable is a symbol that start with a '?'."
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun match-length-error (pattern)
  "Produce an error message for when the pattern and the input are or different
   length."
  (if (null pattern)
      'excess-of-input
      'excess-of-pattern))

(defun lookup (key alist)
  "Lookup the value associated to key in alist."
  (cdr (assoc key alist)))

(defun bind-variable (var val bindings)
  "Bind var to val in bindings."
  (cond ((eql (lookup var bindings) val)
	 bindings)
	((null (lookup var bindings))
	 (cons (cons var val) bindings))
	(t (values nil 'different-values-for-the-same-variable))))

(defun segment-variable-p (x)
  "A segment variable is a list of the form (?x *)"
  (and (listp x)
       (= (length x) 2)
       (equal (char (symbol-name (first x)) 0) #\?)
       (eql (second x) '*)))

(defun match-segment (var pattern input bindings &optional (segment nil))
  "Matches var with a segment [...]"
  (cond ((null pattern)
	 (bind-variable var input bindings))
	((null input)
	 (values nil 'segment-match-fail))
	((equal (first pattern) (first input))
	 (multiple-value-bind (new-bindings err)
	     (pattern-match pattern input (bind-variable var segment bindings))
	   (if err
	       (match-segment var pattern (rest input) bindings
			      (append segment (list (first input))))
	       new-bindings)))
	(t (match-segment var pattern (rest input) bindings
			  (append segment (list (first input)))))))

(defun pattern-match (pattern input &optional (bindings nil))
  "Matches pattern agains input."
  (cond ((and (null pattern) (null input)) bindings)
	((or  (null pattern) (null input))
	 (values nil (match-length-error pattern)))
	((variable-p (first pattern))
	 (multiple-value-bind (new-bindings err)
	     (bind-variable (first pattern) (first input) bindings)
               (if err
		   (values nil err)
		   (pattern-match (rest pattern) (rest input) new-bindings))))
	((segment-variable-p (first pattern))
	 (match-segment (first (first pattern)) (rest pattern) input bindings))
	((equal (first pattern) (first input))
	 (pattern-match (rest pattern) (rest input) bindings))
	(t (values nil 'malformed-pattern))))

(defun match (pattern input)
  "Matches pattern agains an input string."
  (pattern-match pattern (remove-if #'(lambda (s) (string-equal s ""))
				    (uiop:split-string (string-downcase input)))))
