(defpackage :elizabot-tests/all
  (:use :cl :rove)
  (:import-from :elizabot/pattern-matching
   :bind-variable
   :pattern-match
   :match)
  (:import-from :elizabot/eliza
		:eliza-one))
(in-package elizabot-tests/all)

(deftest bind-variable-test
  (ok (equal '((foo . oof))
	     (bind-variable 'foo 'oof nil)))
  (ok (equal '((foo . oof) (a . a))
	     (bind-variable 'foo 'oof '((a . a)))))
  (ok (equal '((foo . oof))
	     (bind-variable 'foo 'oof '((foo . oof)))))
  (ok (equal '(nil elizabot/pattern-matching::different-values-for-the-same-variable)
	     (multiple-value-list (bind-variable 'foo 'oof '((foo . foo)))))))

(deftest pattern-match-test
  (ok (equal '(nil)
	     (multiple-value-list (pattern-match nil nil))))
  (ok (equal '(((?z . baz) (?y . bar) (?x . foo)))
	     (multiple-value-list (pattern-match '(?x ?y ?z) '(foo bar baz)))))
  (ok (equal '(nil elizabot/pattern-matching::excess-of-pattern)
	     (multiple-value-list (pattern-match '(?x ?y ?z) '(foo bar)))))
  (ok (equal '(nil elizabot/pattern-matching::excess-of-input)
	     (multiple-value-list (pattern-match '(?x ?y ?z) '(foo foo bar bar)))))
  (ok (equal '(((?x . foo)))
	     (multiple-value-list (pattern-match '(foo ?x baz) '(foo foo baz)))))
  (ok (equal '(((?x . oof)))
	     (multiple-value-list (pattern-match '(foo ?x baz) '(foo oof baz)))))
  (ok (equal '(((?y . baz) (?x . oof)))
	     (multiple-value-list (pattern-match '(foo ?x ?y) '(foo oof baz)))))
  (ok (equal '(((?z . baz) (?y . oof) (?x . foo)))
	     (multiple-value-list (pattern-match '(?x ?y ?z) '(foo oof baz)))))
  (ok (equal '(nil elizabot/pattern-matching::different-values-for-the-same-variable)
	     (multiple-value-list (pattern-match '(?x foo ?x) '(bar foo baz)))))
  (ok (equal '(((?x . bar)))
	     (multiple-value-list (pattern-match '(?x foo ?x) '(bar foo bar)))))
  (ok (equal '(nil)
	     (multiple-value-list (pattern-match '(foo bar baz) '(foo bar baz)))))
  (ok (equal '(((?Y fool) (?X he)))
	     (multiple-value-list (pattern-match '((?x *) is a (?y *))
				    '(he is a fool)))))
  (ok (equal '(((?Y big fool) (?X his friend)))
	     (multiple-value-list (pattern-match '((?x *) is a (?y *))
				    '(his friend is a big fool)))))
  (ok (equal '(((?Y big fool) (?X his friend is)))
	     (multiple-value-list (pattern-match '((?x *) is a (?y *))
				    '(his friend is is a big fool)))))
  (ok (equal '(nil elizabot/pattern-matching::segment-match-fail)
	     (multiple-value-list (pattern-match '((?x *) is a ?y)
				    '(his friend is a big fool)))))
  (ok (equal '(nil elizabot/pattern-matching::segment-match-fail)
	     (multiple-value-list (pattern-match '((?x *) is a ?x)
				    '(his friend is a fool))))))

(deftest eliza-one-test
  (ok (equal '(((?x "my" "cousin")) . (?x "sure" "is" "a" "fool"))
	     (eliza-one "My cousin is a fool"
			'(((?x *) "is" "a" "fool") . (?x "sure" "is" "a" "fool")))))
  (ok (equal nil
	     (eliza-one "My cousin is fool"
			'(((?x *) "is" "a" "fool") . (?x "sure" "is" "a" "fool")))))
  (ok (equal '(((?y . "fool") (?x "my" "friend")) . (?x "sure" "is" "a" ?y))
	     (eliza-one "My friend is a fool"
			'(((?x *) "is" "a" ?y) . (?x "sure" "is" "a" ?y))))))
