;;;; -*- encoding:utf-8 -*-

(in-package :llama.test)

;; https://huggingface.co/QuantFactory/Meta-Llama-3-8B-GGUF-v2/blob/main/Meta-Llama-3-8B.Q4_1.gguf
(defvar *test-model* (truename "~/llama.cpp/models/Meta-Llama-3-8B.Q4_1.gguf"))

;; https://huggingface.co/bartowski/gemma-2-9b-it-GGUF/blob/main/gemma-2-9b-it-Q6_K_L.gguf
(defvar *test-model-embedding* (truename "~/llama.cpp/models/gemma-2-9b-it-Q6_K_L.gguf"))

(defvar *mdl*)

(defvar *ctx*)

#+sbcl (sb-int:set-floating-point-modes :traps nil)

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

(5am:test create-context
  (5am:finishes (setf *ctx* (make-instance 'ctx :model *mdl*))))

(5am:test describe-params
  (5am:finishes (describe (slot-value *ctx* 'params))))

(5am:test (tokens :depends-on create-context)
  (5am:is (= 384133 (+ (token *ctx* :bos) (token *ctx* :eos) (token *ctx* :nl)
		       (token *ctx* :cls) (token *ctx* :sep) (token *ctx* :prefix)
		       (token *ctx* :middle) (token *ctx* :suffix) (token *ctx* :eot)))))

(5am:test (n-vocab :depends-on create-context)
  (5am:is (eql 128256 (n-vocab *mdl*))))

(5am:test (n-ctx :depends-on create-context)
  (5am:is (eql 512 (n-ctx *ctx*))))

(5am:test (n-ctx-train :depends-on create-context)
  (5am:is (eql 8192 (n-ctx-train *mdl*))))

(5am:test (n-batch :depends-on create-context)
  (5am:is (eql 512 (n-batch *ctx*))))

(5am:test (n-ubatch :depends-on create-context)
  (5am:is (eql 512 (n-ubatch *ctx*))))

(5am:test (n-seq-max :depends-on create-context)
  (5am:is (eql 512 (n-seq-max *ctx*))))

(5am:test (pooling-type :depends-on create-context)
  (5am:is (eql :none (pooling-type *ctx*))))

(5am:test (n-embd :depends-on create-context)
  (5am:is (eql 4096 (n-embd *mdl*))))

(5am:test (n-layer :depends-on create-context)
  (5am:is (eql 4096 (n-layer *mdl*))))

(5am:test (vocab-type :depends-on create-context)
  (5am:is (eql :bpe (vocab-type *mdl*))))

(5am:test (rope-type :depends-on create-context)
  (5am:is (eql :norm (rope-type *mdl*))))

(5am:test (size :depends-on create-context)
  (5am:is (eql 5122416640 (size *mdl*))))

(5am:test (n-params :depends-on create-context)
  (5am:is (eql 8030261248 (n-params *mdl*))))

(5am:test (vocab :depends-on create-context)
  (5am:is (eql 128256 (length (get-vocab *ctx*)))))

(5am:test (tokenize-error :depends-on create-context)
  (5am:signals simple-error (tokenize *mdl* 5 "The quick brown fox jumps over the lazy dog")))

(5am:test (tokenize-success :depends-on create-context)
  (5am:is (= 9 (llama::n (tokenize *mdl* t "The quick brown fox jumps over the lazy dog")))))

(5am:test (tokenize-id :depends-on create-context)
  (5am:is (equal '(9906)
		 (list-tokens (tokenize *mdl* t "Hello")))))

(5am:test (tokenize-str :depends-on create-context)
  (5am:is (equalp '("Invest" "igation")
		  (list-tokens (tokenize *mdl* t "Investigation") :context *ctx*))))

(5am:test (embedding :depends-on metal)
  (5am:is (equalp #+(or ARM ARM64 AARCH64) #(0.0027472726 -0.03923983 -0.002686364)
		  #-(or LINUX ARM ARM64 AARCH64) #(0.0028596406 -0.03940781 -0.0031085624)
		  #+LINUX #(0.00355976 -0.039729998 -0.0036821505)
		  (subseq (embedding "testing" :model *test-model-embedding*) 0 3))))

(5am:test embedding-no-metal
  (5am:is (equalp #+(or ARM ARM64 AARCH64) #(0.0026047684 -0.04103954 -0.002475477)		 
		  #-(or LINUX ARM ARM64 AARCH64) #(0.0031177837 -0.04023566 -0.00362199)
		  #+LINUX #(0.0036606288 -0.04072999 -0.0033451244)
		  (subseq (embedding "testing" :model *test-model-embedding* :metal nil) 0 3))))

(5am:test (embeddings :depends-on metal)
  (5am:is (equalp #+(or ARM ARM64 AARCH64) '(#(0.0027472726 -0.03923983 -0.002686364)
					     #(-0.00230485 -0.03767107 0.0024707825)
					     #(0.0027472726 -0.03923983 -0.002686364))
		  #-(or LINUX ARM ARM64 AARCH64) '(#(0.0028596406 -0.03940781 -0.0031085624)
						   #(-0.002174724 -0.037758734 0.0031787835)
						   #(0.0028596406 -0.03940781 -0.0031085624))
		  #+LINUX '(#(0.00355976 -0.039729998 -0.0036821505)
			    #(-0.0020913247 -0.037312973 0.0023225711)
			    #(0.00355976 -0.039729998 -0.0036821505))
		  (mapcar (lambda (x) (subseq x 0 3)) (embedding '("testing" "something else" "testing")
								 :model *test-model-embedding*)))))

(5am:test embeddings-no-metal
  (5am:is (equalp #+(or ARM ARM64 AARCH64) '(#(0.0026047684 -0.04103954 -0.002475477)
					     #(-0.0024215174 -0.038562134 0.002473093)
					     #(0.0026047684 -0.04103954 -0.002475477))
		  #-(or LINUX ARM ARM64 AARCH64) '(#(0.0031177837 -0.04023566 -0.00362199)
						   #(-0.002250815 -0.038325872 0.0030096115)
						   #(0.0031177837 -0.04023566 -0.00362199))
		  #+LINUX '(#(0.0036606288 -0.04072999 -0.0033451244)
			    #(-0.0019625456 -0.037582934 0.0021264562)
			    #(0.0036606288 -0.04072999 -0.0033451244))
		  (mapcar (lambda (x) (subseq x 0 3)) (embedding '("testing" "something else" "testing")
								 :model *test-model-embedding* :metal nil)))))

(5am:test (perplexity :depends-on metal)
  (5am:is (= #+(or ARM ARM64 AARCH64) 1.2597692
	     #-(or LINUX ARM ARM64 AARCH64) 1.2595763
	     #+LINUX 1.2502482
	     (perplexity (apply #'concatenate 'string
				(with-open-file (in (asdf:system-relative-pathname (asdf:find-system :llama) "LICENSE")
						    :external-format :utf-8 :element-type 'character)
				  (loop for line = (read-line in nil nil)
					while line append (list line line line line))))
			 :model *test-model* :verbose 0))))

(5am:test perplexity-no-metal
  (5am:is (= #+(or ARM ARM64 AARCH64) 1.2597548
	     #-(or LINUX ARM ARM64 AARCH64) 1.2595822
	     #+LINUX 1.2502482
	     (perplexity (apply #'concatenate 'string
				(with-open-file (in (asdf:system-relative-pathname (asdf:find-system :llama) "LICENSE")
						    :external-format :utf-8 :element-type 'character)
				  (loop for line = (read-line in nil nil)
					while line append (list line line line line))))
			 :metal nil :model *test-model* :verbose 0))))

(5am:test simple
  (5am:is (equalp #-LINUX "Hello my name is {name} and I am {age} years old. I am currently studying {course} at {university}. My hobbies include {"
		  #+LINUX "Hello my name is {name} and I am {age} years old. I am currently studying at {school} and my favorite subject is {subject}."
		  (simple :model *model* :print-while-generating nil :print-timings nil))))

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
