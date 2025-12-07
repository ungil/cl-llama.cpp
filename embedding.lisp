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
				  :params (model-parameters :n-gpu-layers (if metal 999 0))))
	 (ctx (make-instance 'ctx :model mdl
				  :params (context-parameters :embeddings t :n-batch n-batch
							      :pooling-type (case pooling-type
									      (:none 0)
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

;; ./llama-embedding -m ~/llama.cpp/models/snowflake-arctic-embed-l-v2.0-q8_0.gguf -p "testing" --pooling mean -c 512 -b 512 | head -c 44
;; embedding 0:  0.044910  0.042335  0.034874 %

;; ./llama-embedding -m ~/llama.cpp/models/snowflake-arctic-embed-l-v2.0-q8_0.gguf -p "testing" --pooling mean -c 512 -b 512 -ngl 0 | head -c 44
;; embedding 0:  0.044866  0.041521  0.033552 %

;; (subseq (embedding "testing" :model "~/llama.cpp/models/snowflake-arctic-embed-l-v2.0-q8_0.gguf" :metal t) 0 3)
;; #(0.044910387 0.042334624 0.034874283)

;; (subseq (embedding "testing" :model "~/llama.cpp/models/snowflake-arctic-embed-l-v2.0-q8_0.gguf" :metal nil) 0 3)
;; #(0.044866164 0.04152129 0.03355201)
