;;;; -*- encoding:utf-8 -*-

(in-package :llama.test)

(defvar *test-model* (truename "~/llama.cpp/models/7B/ggml-model-f16.gguf"))

(defvar *mdl*)

(defvar *ctx*)

#+sbcl (sb-ext::set-floating-point-modes :traps nil)

(defun run ()
  (5am:run! 'llama-suite))

(5am:def-suite llama-suite)

(5am:in-suite llama-suite)

(5am:test info
  (5am:finishes (format t "~&~A~&" (system-info))))

(5am:test max-devices
  (5am:is (= 1 (max-devices))))

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

(5am:test bos-eos :depends-on 'create-context
  (5am:is (= 16 (+ (token-bos *ctx*) (token-eos *ctx*) (token-nl *ctx*)))))

(5am:test n-vocab :depends-on 'create-context
  (5am:is (eq 32000 (n-vocab *mdl*))))

(5am:test n-ctx :depends-on 'create-context
  (5am:is (eq 512 (n-ctx *ctx*))))

(5am:test n-embd :depends-on 'create-context
  (5am:is (eq 4096 (n-embd *mdl*))))

(5am:test vocab :depends-on 'create-context
  (5am:is (eq 32000 (length (get-vocab *ctx*)))))

(5am:test tokenize-error :depends-on 'create-context
  (5am:signals simple-error (tokenize *mdl* 10 "The quick brown fox jumps over the lazy dog")))

(5am:test tokenize-success :depends-on 'create-context
  (5am:is (= 11 (llama::n (tokenize *mdl* 20 "The quick brown fox jumps over the lazy dog")))))

(5am:test tokenize-id :depends-on 'create-context
  (5am:is (equal '(15043)
		 (list-tokens (tokenize *mdl* 1 "Hello")))))

(5am:test tokenize-str :depends-on 'create-context
  (5am:is (equalp '("▁Investig" "ation")
		  (list-tokens (tokenize *mdl* 10 "Investigation") :context *ctx*))))

(5am:test embedding
  (5am:is (equalp (if *metal*
		      #(1.3826334 -1.6712512 0.81991553)		      
		      #+(or ARM ARM64) #(1.3917959 -1.6723749 0.8140562)
		      #-(or ARM ARM64) #(1.3847897 -1.6708059 0.8206482))
		  (subseq (embedding "testing") 0 3))))

(5am:test embeddings
  (5am:is (equalp (if *metal*
		      '(#(1.3826334 -1.6712512 0.81991553)
			#(-2.4520674 1.4927745 -2.1975682)
			#(1.3826334 -1.6712512 0.81991553))		      
		      #+(or ARM ARM64) '(#(1.3917959 -1.6723749 0.8140562)
					 #(-2.443811 1.4967367 -2.1949144)
					 #(1.3917959 -1.6723749 0.8140562))
		      #-(or ARM ARM64) '(#(1.3847897 -1.6708059 0.8206482)
					 #(-2.449856 1.492255 -2.1966687)
					 #(1.3847897 -1.6708059 0.8206482)))
		  (mapcar (lambda (x) (subseq x 0 3)) (embedding '("testing" "something else" "testing"))))))

#-ALLEGRO-CL-TRIAL
(5am:test perplexity
  (5am:is (= (if *metal*
		 1.1825091
		 #+(or ARM ARM64) 1.1821864
		 #-(or ARM ARM64) 1.1824327)
	     (perplexity (apply #'concatenate 'string
				(with-open-file (in (asdf:system-relative-pathname (asdf:find-system :llama) "LICENSE")
						    :external-format :utf-8 :element-type 'character)
				  (loop for line = (read-line in nil nil) while line append (list line line))))))))

(5am:test llama-greedy
  (5am:is (string= "in the first part of▁the▁book,▁the▁author▁describes▁the▁history▁of▁the▁development▁of▁the▁theory▁of▁the▁origin▁of▁the▁universe"
		   (with-output-to-string (*standard-output*)
		     (llama :verbose 0 :predict 20 :seed 42 :prompt "in the first part of" :repeat-last-n 0 :temp -1)))))

(5am:test llama-repetition
  (5am:is (string= "in the first part of▁the▁book▁we▁see▁the▁development▁of▁the▁first▁modern▁nation▁states▁with▁their▁attendant▁ideologies▁and▁political"
		   (with-output-to-string (*standard-output*)
		     (llama :verbose 0 :predict 20 :seed 42 :prompt "in the first part of" :repeat-last-n 10)))))

(5am:test llama-keep
  (5am:is (string= (if *metal* 
		       "in the first part of▁this▁series▁we▁have▁discussed▁the▁basic▁principles▁of▁the▁theory▁of▁gravity.▁To▁explain▁this,▁we▁have▁to▁use▁it.<0x0A>to▁the▁last▁byte,"
		       #+(or ARM ARM64) "in the first part of▁this▁series▁we▁have▁discussed▁the▁basic▁principles▁of▁the▁theory▁of▁gravity.▁To▁explain▁this,▁we▁have▁to▁use▁it.<0x0A>to▁the▁'Cab"
		       #-(or ARM ARM64) "in the first part of▁this▁series▁we▁have▁discussed▁the▁basic▁principles▁of▁the▁theory▁of▁gravity.▁To▁explain▁this,▁we▁have▁to▁use▁it.<0x0A>to▁the▁last▁byte,")
		   (with-output-to-string (*standard-output*)
		     (llama :verbose 0 :predict 30 :seed 42 :prompt "in the first part of" :n-ctx 10 :n-keep 2 :repeat-last-n 0)))))

(5am:test llama-batches
  (5am:is (string= (if *metal*
		       "in the first part of▁this▁series▁we▁have▁discussed▁the▁concept▁of▁the▁digital▁twin▁in▁the▁form▁of▁a▁digital▁model."
		       #+(or ARM ARM64) "in the first part of▁this▁series▁we▁have▁discussed▁the▁concept▁of▁the▁digital▁twin▁in▁the▁form▁of▁a▁digital▁prototype,"
		       #-(or ARM ARM64) "in the first part of▁this▁series▁we▁have▁discussed▁the▁concept▁of▁the▁digital▁twin▁in▁the▁form▁of▁a▁digital▁model.")
		   (with-output-to-string (*standard-output*)
		     (llama :verbose 0 :predict 20 :seed 42 :prompt "in the first part of" :repeat-last-n 0 :n-batch 2)))))

