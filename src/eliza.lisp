(defpackage :elizabot/eliza
  (:use :cl)
  (:import-from :elizabot/pattern-matching
		:match)
  (:export
   :add-translation
   :eliza
   :*translation-rules*))
(in-package :elizabot/eliza)

(defparameter *translation-rules* nil
  "A list of translation rules. A translation rule is a pair (pattern . response) 
  such as ((?x is a fool) . (?x sure is))")

(defun add-translation (pattern response)
  "Add (pattern . response) to the global list of translations."
  (pushnew (cons pattern response) *translation-rules*))

(defun eliza-one (input translation-rule)
  "Checks if the input matches the translation rule. If so return a pair
  (bindings . response) otherwise return nil."
  (multiple-value-bind (bindings err)
      (match (car translation-rule) input)
    (if err
	nil
	(cons bindings (cdr translation-rule)))))

(defun random-elt (x)
  "Pick a random element of list x."
  (nth (random (length x)) x))

(defun flatten (words &optional (s ""))
  "Flatten a list of words."
  (cond ((null words) s)
	((stringp (first words))
	 (flatten (rest words) (concatenate 'string s (first words) " ")))
	((listp (first words))
	 (flatten (rest words) (concatenate 'string s (flatten (first words)))))))

(defun trim-last (s)
  "Trim last character out of a string."
  (subseq s 0 (1- (length s))))

(defun produce-response (rule)
  "Takes a (bindings . response) rule and produces the expected output."
  (trim-last (flatten (sublis (car rule) (cdr rule)))))

(defun eliza (input)
  "Run ELIZA on the input i.e. check if some translation's pattern matches the
  input and return one of the suitable translations randomly."
  (let* ((results (remove-if #'null
			     (mapcar #'(lambda (rule) (eliza-one input rule))
				     *translation-rules*))))
    
    (if results
	(let ((rule (random-elt results)))
	  (produce-response rule))
	(values nil 'no-match))))
