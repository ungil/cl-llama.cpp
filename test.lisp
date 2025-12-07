;;;; -*- encoding:utf-8 -*-

(in-package :llama.test)

;; https://huggingface.co/unsloth/gemma-3-4b-it-GGUF/blob/main/gemma-3-4b-it-Q8_0.gguf
;; (defvar *test-model* (truename "~/llama.cpp/models/gemma-3-4b-it-Q8_0.gguf"))
;; https://huggingface.co/unsloth/Ministral-3-3B-Instruct-2512-GGUF/tree/main
;; (defvar *test-model* (truename "~/llama.cpp/models/Ministral-3-3B-Instruct-2512-Q8_0.gguf"))
;; https://huggingface.co/QuantFactory/SmolLM-135M-GGUF
(defvar *test-model* (truename "~/llama.cpp/models/SmolLM-135M.Q8_0.gguf"))

;; https://huggingface.co/Casual-Autopsy/snowflake-arctic-embed-l-v2.0-gguf/blob/main/snowflake-arctic-embed-l-v2.0-q8_0.gguf
;; (defvar *test-model-embedding* (truename "~/llama.cpp/models/snowflake-arctic-embed-l-v2.0-q8_0.gguf"))
(defvar *test-model-embedding* *test-model*)

(defvar *mdl*)

(defvar *ctx*)

(defun run ()
  (5am:run! 'llama-suite))

(5am:def-suite llama-suite)

(5am:in-suite llama-suite)

(5am:test info
  (5am:finishes (format t "~&~A~&" (system-info))))

(5am:test metal
  (5am:is-true *metal*))

(5am:test max-devices
  (5am:is (plusp (max-devices))))

(5am:test mmap
  (5am:is (mmap-supported)))

(5am:test mlock
  (5am:is (mlock-supported)))

(5am:test load-model
  (5am:finishes (setf *mdl* (make-instance 'mdl :file *test-model*))))

(5am:test (create-context :depends-on load-model)
  (5am:finishes (setf *ctx* (make-instance 'ctx :model *mdl*))))

(5am:test (describe-params :depends-on create-context)
  (5am:finishes (describe (slot-value *ctx* 'params))))

(5am:test (tokens :depends-on create-context)
  (5am:is (= 196 (+ (token *ctx* :bos) (token *ctx* :eos) (token *ctx* :nl)
		    (token *ctx* :sep) (token *ctx* :prefix)
		    (token *ctx* :middle) (token *ctx* :suffix) (token *ctx* :eot)))))

(5am:test (n-vocab :depends-on create-context)
  (5am:is (eql 49152 (n-vocab *mdl*))))

(5am:test (n-ctx :depends-on create-context)
  (5am:is (eql 512 (n-ctx *ctx*))))

(5am:test (n-ctx-train :depends-on create-context)
  (5am:is (eql 2048 (n-ctx-train *mdl*))))

(5am:test (n-batch :depends-on create-context)
  (5am:is (eql 512 (n-batch *ctx*))))

(5am:test (n-ubatch :depends-on create-context)
  (5am:is (eql 512 (n-ubatch *ctx*))))

(5am:test (n-seq-max :depends-on create-context)
  (5am:is (eql 1 (n-seq-max *ctx*))))

(5am:test (pooling-type :depends-on create-context)
  (5am:is (eql :none (pooling-type *ctx*))))

(5am:test (n-embd :depends-on create-context)
  (5am:is (eql 576 (n-embd *mdl*))))

(5am:test (n-layer :depends-on create-context)
  (5am:is (eql 30 (n-layer *mdl*))))

(5am:test (vocab-type :depends-on create-context)
  (5am:is (eql :bpe (vocab-type *mdl*))))

(5am:test (rope-type :depends-on create-context)
  (5am:is (eql :norm (rope-type *mdl*))))

(5am:test (size :depends-on create-context)
  (5am:is (eql 143025408 (size *mdl*))))

(5am:test (n-params :depends-on create-context)
  (5am:is (eql 134515008 (n-params *mdl*))))

(5am:test (vocab :depends-on create-context)
  (5am:is (eql 49152 (length (get-vocab *ctx*)))))

(5am:test (tokenize-error :depends-on create-context)
  (5am:signals simple-error (tokenize *mdl* 5 "The quick brown fox jumps over the lazy dog")))

(5am:test (tokenize-success :depends-on create-context)
  (5am:is (= 9 (llama::n (tokenize *mdl* t "The quick brown fox jumps over the lazy dog")))))

(5am:test (tokenize-id :depends-on create-context)
  (5am:is (equal '(19556)
		 (list-tokens (tokenize *mdl* t "Hello")))))

(5am:test (tokenize-str :depends-on create-context)
  (5am:is (equalp '("Extra" "ordinary")
		  (list-tokens (tokenize *mdl* t "Extraordinary") :context *ctx*))))

(5am:test (embedding :depends-on metal)
  (5am:is (equalp #+(or ARM ARM64 AARCH64) #(-0.049819496 0.012173356 0.0044705253)
		  #-(or LINUX ARM ARM64 AARCH64) #(-0.049819496 0.012173356 0.0044705253)
		  #+LINUX #(-0.049819496 0.012173356 0.0044705253)
		  (subseq (embedding "testing" :model *test-model-embedding*) 0 3))))

(5am:test embedding-no-metal
  (5am:is (equalp #+(or ARM ARM64 AARCH64) #(-0.048586186 0.0029827687 0.0010325225)
		  #-(or LINUX ARM ARM64 AARCH64) #(-0.048586186 0.0029827687 0.0010325225)
		  #+LINUX #(-0.048586186 0.0029827687 0.0010325225)
		  (subseq (embedding "testing" :model *test-model-embedding* :metal nil) 0 3))))

(5am:test (embeddings :depends-on metal)
	  (5am:is (equalp #+(or ARM ARM64 AARCH64) '(#(-0.049819496 0.012173356 0.0044705253)
						     #(-0.025256668 0.0029770537 0.013806367)
						     #(-0.049819496 0.012173356 0.0044705253))
		  #-(or LINUX ARM ARM64 AARCH64) '(#(-0.049819496 0.012173356 0.0044705253)
						   #(-0.025256668 0.0029770537 0.013806367)
						   #(-0.049819496 0.012173356 0.0044705253))
		  #+LINUX  '(#(-0.049819496 0.012173356 0.0044705253)
			     #(-0.025256668 0.0029770537 0.013806367)
			     #(-0.049819496 0.012173356 0.0044705253))
		  (mapcar (lambda (x) (subseq x 0 3)) (embedding '("testing" "something else" "testing")
								 :model *test-model-embedding*)))))

(5am:test embeddings-no-metal
	  (5am:is (equalp #+(or ARM ARM64 AARCH64) '(#(-0.048586186 0.0029827687 0.0010325225)
						     #(-0.026333824 -2.797078E-4 0.010385639)
						     #(-0.048586186 0.0029827687 0.0010325225))
		  #-(or LINUX ARM ARM64 AARCH64) '(#(-0.048586186 0.0029827687 0.0010325225)
						   #(-0.026333824 -2.797078E-4 0.010385639)
						   #(-0.048586186 0.0029827687 0.0010325225))
		  #+LINUX '(#(-0.048586186 0.0029827687 0.0010325225)
			    #(-0.026333824 -2.797078E-4 0.010385639)
			    #(-0.048586186 0.0029827687 0.0010325225))
		  (mapcar (lambda (x) (subseq x 0 3)) (embedding '("testing" "something else" "testing")
								 :model *test-model-embedding* :metal nil)))))

(5am:test simple
  (5am:is (equalp #-LINUX "Hello my name is <NAME> and I am a student at the University of California, Santa Barbara. I am currently studying Computer Science and I am interested in learning more about the"
		  #+LINUX "Hello my name is <NAME> and I am a student at the University of California, Santa Barbara. I am currently studying Computer Science and I am interested in learning more about the"
		  (simple :model *test-model* :print-while-generating nil :print-timings nil))))

(5am:test (perplexity :depends-on metal)
  (5am:is (= #+(or ARM ARM64 AARCH64) 2.116421
	     #-(or LINUX ARM ARM64 AARCH64) 2.116421
	     #+LINUX 2.116421
	     (perplexity (apply #'concatenate 'string
				(with-open-file (in (asdf:system-relative-pathname (asdf:find-system :llama) "LICENSE")
						    :external-format :utf-8 :element-type 'character)
				  (loop for line = (read-line in nil nil)
					while line append (list line line line line))))
			 :model *test-model* :verbose 0))))

(5am:test perplexity-no-metal
  (5am:is (= #+(or ARM ARM64 AARCH64) 2.1186126
	     #-(or LINUX ARM ARM64 AARCH64) 2.1186126
	     #+LINUX 2.1186126
	     (perplexity (apply #'concatenate 'string
				(with-open-file (in (asdf:system-relative-pathname (asdf:find-system :llama) "LICENSE")
						    :external-format :utf-8 :element-type 'character)
				  (loop for line = (read-line in nil nil)
					while line append (list line line line line))))
			 :metal nil :model *test-model* :verbose 0))))

#+NIL
(5am:test llama-greedy
  (5am:is (string= "in the first part of▁the▁book,▁the▁author▁describes▁the▁history▁of▁the▁development▁of▁the▁theory▁of▁the▁origin▁of▁the▁universe"
		   (with-output-to-string (*standard-output*)
		     (llama :verbose 0 :predict 20 :seed 42 :prompt "in the first part of" :repeat-last-n 0 :temp -1)))))

#+NIL
(5am:test llama-repetition
  (5am:is (string= "in the first part of▁the▁book▁we▁see▁the▁development▁of▁the▁first▁modern▁nation▁states▁with▁their▁attendant▁ideologies▁and▁political"
		   (with-output-to-string (*standard-output*)
		     (llama :verbose 0 :predict 20 :seed 42 :prompt "in the first part of" :repeat-last-n 10)))))

#+NIL
(5am:test llama-keep
  (5am:is (string= (if *metal* 
		       "in the first part of▁this▁series▁we▁have▁discussed▁the▁basic▁principles▁of▁the▁theory▁of▁gravity.▁To▁explain▁this,▁we▁have▁to▁use▁it.<0x0A>to▁the▁'Cathedral"
		       #+(or ARM ARM64 AARCH64) "in the first part of▁this▁series▁we▁have▁discussed▁the▁basic▁principles▁of▁the▁theory▁of▁gravity.▁To▁explain▁this,▁we▁have▁to▁use▁it.<0x0A>to▁the▁'Cab"
		       #-(or ARM ARM64 AARCH64) "in the first part of▁this▁series▁we▁have▁discussed▁the▁basic▁principles▁of▁the▁theory▁of▁gravity.▁To▁explain▁this,▁we▁have▁to▁use▁it.<0x0A>to▁the▁last▁byte,")
		   (with-output-to-string (*standard-output*)
		     (llama :verbose 0 :predict 30 :seed 42 :prompt "in the first part of" :n-ctx 10 :n-keep 2 :repeat-last-n 0)))))

#+NIL
(5am:test llama-batches
  (5am:is (string= (if *metal*
		       "in the first part of▁this▁series▁we▁have▁discussed▁the▁concept▁of▁the▁digital▁twin▁in▁the▁form▁of▁a▁digital▁prototype,"
		       #+(or ARM ARM64 AARCH64) "in the first part of▁this▁series▁we▁have▁discussed▁the▁concept▁of▁the▁digital▁twin▁in▁the▁form▁of▁a▁digital▁prototype,"
		       #-(or ARM ARM64 AARCH64) "in the first part of▁this▁series▁we▁have▁discussed▁the▁concept▁of▁the▁digital▁twin▁in▁the▁form▁of▁a▁digital▁model.")
		   (with-output-to-string (*standard-output*)
		     (llama :verbose 0 :predict 20 :seed 42 :prompt "in the first part of" :repeat-last-n 0 :n-batch 2)))))
