(defsystem "elizabot"
  :version "0.1.0"
  :author "Gabriele Rastello"
  :license ""
  :depends-on ("dexador"
	       "jonathan"
	       "uiop")
  :components ((:module "src"
		:serial t
		:components ((:file "pattern-matching")
			     (:file "eliza")
			     (:file "main"))))
  :in-order-to ((test-op (test-op "elizabot/tests"))))

(defsystem "elizabot/tests"
  :depends-on ("elizabot"
	       "rove")
  :components ((:module "tests"
		:components ((:file "all"))))
  :perform (test-op (op c)
		    (uiop:symbol-call :rove :run 'elizabot/tests)))
