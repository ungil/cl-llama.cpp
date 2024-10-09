(in-package #:asdf)

(defsystem #:llama
    :name "llama"
    :author "Carlos Ungil <ungil@mac.com>"
    :license "Apache License, Version 2.0"
    :description "Common Lisp interface to https://github.com/ggerganov/llama.cpp/"
    :in-order-to ((test-op (test-op "llama/test")))
    :depends-on (:trivial-garbage #-(or lispworks allegro) :cffi #-(or lispworks allegro) :cffi-libffi)
    :serial t
    :components ((:file "package")
		 (:file "libllama")
		 (:file #-(or lispworks allegro) "llama-cffi" #+lispworks "llama-dff" #+allegro "llama-ff")
		 (:file "wrapper")
		 (:file "circular-buffer")
		 (:file "defaults")
		 (:file "embedding")
		 (:file "perplexity")		 
		 (:file "simple")))

(defsystem #:llama/test
    :name "llama test suite"
    :author "Carlos Ungil <ungil@mac.com>"
    :license "Apache License, Version 2.0"
    :depends-on (:llama :fiveam)
    :components ((:file "test"))
    :perform (asdf:test-op (o s) (declare (ignore o s)) (uiop:symbol-call :llama.test '#:run)))
