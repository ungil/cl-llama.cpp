(in-package :llama)

(defun normalize-embeddings (input)
  (assert input)
  (let* ((sum (loop for x across input sum (* x x)))
	 (norm (if (> sum 0.0) (/ 1.0 (sqrt sum)) 0.0)))
    (loop for i below (length input)
	  do (setf (aref input i) (* (aref input i) norm)))
    input))

(defun %embedding (ctx tokens txt verbose)
  (tokenize (model ctx) tokens txt :add-special t)
  ;; add SEP if not present
#+NIL  (unless (= (token ctx :sep)
	     #+lispworks (fli:dereference (ptr tokens) :index (1- (n tokens)))
	     #+allegro (ff:fslot-value (ptr tokens) (1- (n tokens)))
	     #-(or lispworks allegro) (cffi:mem-aref (ptr tokens) :int (1- (n tokens))))
    (when (= (n tokens) (size tokens)) (error "cannot add a token"))
    (setf #+lispworks (fli:dereference (ptr tokens) :index (n tokens))
	  #+allegro (ff:fslot-value (ptr tokens) (n tokens))
	  #-(or lispworks allegro) (cffi:mem-aref (ptr tokens) :int (n tokens))
	  (token ctx :sep))
    (incf (n tokens)))
  (when (> verbose 0)
    (describe tokens)
    (print (list-tokens tokens))
    (print (list-tokens tokens :context ctx :limit nil)))
  (let ((batch (make-instance 'batch :n-tokens-max (n tokens))))
    (loop for token in (list-tokens tokens)
	  for pos from 0
	  do (add batch token pos t))
    (decode ctx batch :clear t) ;; previous kv_cache values irrelevant for embeddings
    (normalize-embeddings (get-embeddings-seq ctx 0))))

(defun embedding (prompt &key (pooling-type :mean) (model *model*) (n-batch *n-batch*) (n-ubatch *n-ubatch*)
			   (ntokens 512) (verbose 0) (numa *numa*)
			   (add-beginning-of-sentence t) (threads *threads*) (metal *metal*))
  "Calculate embeddings for the given prompt. If passed a list of prompts it will loop over them."
  #+sbcl (sb-int:set-floating-point-modes :traps nil)
  (llama-backend-init)
  (llama-numa-init numa)
  (let* ((mdl (make-instance 'mdl :file model
				  :params (model-parameters :n-gpu-layers (if metal 1 0))))
	 (ctx (make-instance 'ctx :model mdl
				  :params (context-parameters :embeddings t :n-batch n-batch
							      :pooling-type (case pooling-type
									      (:mean 1)
									      (:cls 2)
									      (:last 3)))))
	 (tokens (make-instance 'tokens :size ntokens)))
    (assert (>= (n-ctx-train mdl) (n-ctx ctx)))
    (assert (not (equal (pooling-type ctx) :none)))
    (when (> verbose 1) (print (pooling-type ctx)))
    (prog1
	(if (listp prompt)
	    (loop for text in prompt
		  do (format t ".")
		  collect (%embedding ctx tokens text verbose))
	    (%embedding ctx tokens prompt verbose))
      (when (> verbose 1) (print-timings ctx))
      (llama-backend-free))))

;; In examples/embedding/embedding.cpp need to add the following line before    print_build_info();
;;    params.pooling_type = LLAMA_POOLING_TYPE_MEAN;
;; and recompile - by default LLAMA_POOLING_TYPE_NONE is used because the gguf doesn't specify anything?

;; ./llama-embedding -m ~/llama.cpp/models/gemma-2-9b-it-Q6_K_L.gguf -p "testing" -ngl 1 | head -c 44
;; embedding 0:  0.002747 -0.039240 -0.002686

;; ./llama-embedding -m ~/llama.cpp/models/gemma-2-9b-it-Q6_K_L.gguf -p "testing" -ngl -0 | head -c 44
;; embedding 0:  0.002605 -0.041040 -0.002475 %

;; (subseq (embedding "testing" :model "~/llama.cpp/models/gemma-2-9b-it-Q6_K_L.gguf" :metal t) 0 3)
;; #(0.0027472726 -0.03923983 -0.002686364)

;; (subseq (embedding "testing" :model "~/llama.cpp/models/gemma-2-9b-it-Q6_K_L.gguf" :metal nil) 0 3)
;; #(0.0026047684 -0.04103954 -0.002475477)
