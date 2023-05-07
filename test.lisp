(in-package :llama.test)

(defvar *test-model* (truename "~/llama.cpp/models/llama-7B/ggml-model.bin"))

(defvar *context*)

#+sbcl (sb-ext::set-floating-point-modes :traps nil)

(defun run ()
  (5am:run! 'llama-suite))

(5am:def-suite llama-suite)

(5am:in-suite llama-suite)

(5am:test bos-eos
  (5am:is (= 16 (+ (llama::llama-token-bos) (llama::llama-token-eos) (llama::llama-token-nl)))))

(5am:test info
  (5am:finishes (format t "~&~A~&" (llama:system-info))))

(5am:test mmap
  (5am:is (llama:mmap-supported)))

(5am:test mlock
  (5am:is (llama:mlock-supported)))

(5am:test create-context
  (5am:finishes (setf *context* (make-instance 'llama:context :model *test-model*))))

(5am:test describe-params
  (5am:finishes (describe (slot-value *context* 'llama:params))))

(5am:test n-vocab :depends-on 'create-context
  (5am:is (eq 32000 (llama:n-vocab *context*))))

(5am:test n-ctx :depends-on 'create-context
  (5am:is (eq 512 (llama:n-ctx *context*))))

(5am:test n-embd :depends-on 'create-context
  (5am:is (eq 4096 (llama:n-embd *context*))))

(5am:test vocab :depends-on 'create-context
  (5am:is (eq 32000 (length (llama:get-vocab *context*)))))

(5am:test tokenize-error :depends-on 'create-context
  (5am:signals simple-error (llama:tokenize *context* 10 "The quick brown fox jumps over the lazy dog")))

(5am:test tokenize-success :depends-on 'create-context
  (5am:is (= 11 (llama::n (llama:tokenize *context* 20 "The quick brown fox jumps over the lazy dog")))))

(5am:test tokenize-id :depends-on 'create-context
  (5am:is (equal '(10994)
		 (llama::list-tokens (llama:tokenize *context* 1 "Hello")))))

(5am:test tokenize-str :depends-on 'create-context
  (5am:is (equal '("Inv" "estig" "ation")
		 (list-tokens (tokenize *context* 10 "Investigation") :context *context*))))

(5am:test embedding
  (5am:is (equalp #+(or ARM ARM64) #(1.384214 -1.6681216 0.81773395)
		  #-(or ARM ARM64) #(1.3813722 -1.6728185 0.824559)
		  (subseq (embedding "testing") 0 3))))

(5am:test embeddings
  (5am:is (equalp #+(or ARM ARM64) '(#(1.384214 -1.6681216 0.81773395)
			    #(-2.4449072 1.4948448 -2.1945954)
			    #(1.384214 -1.6681216 0.81773395))
		  #-(or ARM ARM64) '(#(1.3813722 -1.6728185 0.824559)
			    #(-2.4465458 1.4901346 -2.1965084)
			    #(1.3813722 -1.6728185 0.824559))
		  (mapcar (lambda (x) (subseq x 0 3)) (embedding '("testing" "something else" "testing"))))))

#-ALLEGRO-CL-TRIAL
(5am:test perplexity
  (5am:is (= #+(or ARM ARM64) 1.183365221434424D0
	     #-(or ARM ARM64) 1.1833956274323658D0
	     (perplexity (apply #'concatenate 'string
				(with-open-file (in "~/lisp/llama/LICENSE" :external-format :utf-8 :element-type 'character)
				  (loop for line = (read-line in nil nil) while line append (list line line))))))))

(5am:test llama-greedy
  (5am:is (string= " in the first part of the book, the author describes the history of the development of the theory of the origin of the universe"
		   (with-output-to-string (*standard-output*)
		     (llama::llama :verbose 0 :predict 20 :seed 42 :prompt "in the first part of" :repeat-last-n 0 :temp -1)))))

(5am:test llama-repetition
  (5am:is (string= #+(or ARM ARM64) " in the first part of the book we learn about the main characters and the story takes us to meet some of the other characters"
		   #-(or ARM ARM64) " in the first part of the book we see the development of the first modern nation states with their attendant ideologies and political"
		   (with-output-to-string (*standard-output*)
		     (llama::llama :verbose 0 :predict 20 :seed 42 :prompt "in the first part of" :repeat-last-n 10))))) 
(5am:test llama-keep
  (5am:is (string= " in the first part of this series we have discussed the basic principles of the theory of gravity. (Incidentally, this is just a quick video to see if anyone"
		   (with-output-to-string (*standard-output*)
		     (llama::llama :verbose 0 :predict 30 :seed 42 :prompt "in the first part of" :n-ctx 10 :n-keep 2 :repeat-last-n 0)))))

(5am:test llama-batches
  (5am:is (string= " in the first part of this series we have discussed the concept of the digital twin in the form of a digital model."
		   (with-output-to-string (*standard-output*)
		     (llama::llama :verbose 0 :predict 20 :seed 42 :prompt "in the first part of" :repeat-last-n 0 :n-batch 2)))))

