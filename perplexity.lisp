(in-package :llama)

(defun log-softmax (logits token)
  (let* ((max-logit (loop for x across logits maximize x))
	 (sum-exp (loop for x across logits sum (exp (* 1d0 (- x max-logit))))))
    (values (- (aref logits token) max-logit (log sum-exp))
	    (aref logits token)
	    (/ (exp (* 1d0 (- (aref logits token) max-logit))) sum-exp))))

(defun process-logits (all-logits tokens)
  (loop for token in (rest tokens)
	for logits in all-logits
	for x = (- (log-softmax logits token))
	sum x into nnl
	sum (* x x) into nnl2
	finally (return (values nnl nnl2))))

(defun %perplexity (ctx n-ctx tokens verbose &optional (n-chunk most-positive-fixnum))
  ;; N.B. the n-ctx variable is different than the one that was used to initialize the context
  (let* ((n-vocab (n-vocab ctx))
	 (n-chunk-max (floor (/ (n tokens) n-ctx)))
	 (n-chunk (min n-chunk n-chunk-max))
	 (n-batch (n-batch ctx))
	 (num-batches (floor (/ (+ n-ctx n-batch -1) n-batch)))
	 (n-seq (max 1 (floor (/ n-batch n-ctx))))
	 (batch (make-instance 'batch :n-tokens-max (min n-batch (* n-ctx n-seq))))
	 (first (/ n-ctx 2))
	 (add-bos (should-add-bos-token ctx)))
    (assert (not (plusp (add-eos-token ctx))))
    (when (> num-batches 1) (error "cannot handle multiple batches, need to implement logits aggregation"))
    (loop with nnl = 0
	  with nnl2 = 0
	  with count = 0
	  for i from 0 below n-chunk by n-seq
	  for start = (* i n-ctx)
	  for end = (+ start n-ctx)
	  for n-seq-batch = (min n-seq (- n-chunk i))
	  do (memory-clear ctx)
	     (loop for n-outputs = 0
		   for j from 0 below num-batches
		   for batch-start = (+ start (* j n-batch))
		   for batch-size = (min (- end batch-start) n-batch)
		   do (clear batch)
		   do (loop for seq from 0 below n-seq-batch
			    for seq-start = (+ batch-start (* seq n-ctx))
			    do (loop for k from 0 below batch-size
				     for idx = (+ (* seq n-ctx) k)
				     for token = (if (and (zerop k) add-bos (zerop j))
 						     ;; add BOS token for the first batch of each chunk
						     (token ctx :bos)
						     (first (list-tokens (subset tokens (+ seq-start k) 1))))
				     for pos = (+ (* j n-batch) k)
				     for logits = (>= pos first)
				     do (when logits (incf n-outputs))
					(add batch token pos logits seq))))
	     (assert (decode ctx batch))
	     (if (zerop i) (llama-synchronize (ptr ctx)))
	     ;; if (num_batches > 1 && n_outputs > 0) {
	     ;; const auto * batch_logits = llama_get_logits(ctx);
	     ;; logits.insert(logits.end(), batch_logits, batch_logits + n_outputs * n_vocab);
	     (loop for seq from 0 below n-seq-batch
		   do (let ((all-logits (if (> num-batches 1)
					    (error "cannot handle multiple batches")
					    (loop for i from (+ (* seq n-ctx) first)
						  repeat (- n-ctx first)
						  collect (get-logits-ith ctx i))))
			    (tokens (list-tokens (subset tokens (+ start (* seq n-ctx) first) (- n-ctx first))
						 :limit nil)))
			(multiple-value-bind (tmp tmp2) (process-logits all-logits tokens)
			  (incf nnl tmp)
			  (incf nnl2 tmp2)
			  (incf count (- n-ctx first 1))
			  (when (> verbose 0) (format t "[~D]~6,4F " (+ 1 i seq) (exp (/ nnl count)))))))
	  finally (let* ((nnl (/ nnl count))
			 (nnl2 (/ nnl2 count))
			 (ppl (exp nnl))
			 (err (* ppl (sqrt (/ (- nnl2 (* nnl nnl)) (1- count))))))
		    (when (> verbose 0) (format t "~&PPL = ~6,4F +/- ~6,4F~&" ppl err))
		    (return (coerce ppl 'single-float))))))
  
(defun perplexity (text &key (model *model*) (numa *numa*) (verbose 1) 
			  (n-ctx 512) (n-batch *n-batch*) (n-ubatch *n-ubatch*)
			  (add-special t) (metal *metal*))
  "Calculate perplexity as done in llama.cpp/examples/perplexity
Full blocks of length n-ctx are used (returns nil if there is not even one full block).
and the first half (or 512 tokens if larger than 1024) is discarded.
Cumulative values are printed after processing each block if <verbose> is at least 1.
If <text> is a pathname the contents of the file are used."
  #+sbcl (sb-int:set-floating-point-modes :traps nil)
  (when (pathnamep text)
      (setf text (with-open-file (in text :external-format :utf-8 :element-type 'character)
		   (let ((contents (make-array (file-length in) :element-type 'character :adjustable t)))
		     (setf contents (adjust-array contents (list (read-sequence contents in))))
		     (if (equal (aref contents (1- (length contents))) #\Newline)
			 (adjust-array contents (list (1- (length contents))))
			 contents)))))
  (llama-backend-init)
  (llama-numa-init numa)
  (let* ((n-seq (max 1 (/ n-batch n-ctx)))
	 (n-kv (* n-seq n-ctx))
	 (n-batch (min n-batch n-kv))
	 (mdl (make-instance 'mdl :file model
				  :params (model-parameters :n-gpu-layers (if metal 999 0))))
	 (ctx (make-instance 'ctx :model mdl
				  :params (context-parameters :n-ctx n-kv :kv-unified t)))
	 (tokens (tokenize (model ctx) t text :add-special add-special)))
    (assert (<= n-kv (n-ctx-train mdl)))
    (prog1
	(%perplexity ctx n-ctx tokens verbose)
      (when (> verbose 1) (print-timings ctx))
      (llama-backend-free))))

;; ./llama-perplexity -f ~/wikitext-2-raw/wiki.test.raw -m ~/llama.cpp/models/SmolLM-135M.Q8_0.gguf
;; [1]11.2329,[2]18.2887,[3]17.7327,[4]17.6559,[5]17.7286,[6]17.9963,[7]18.4397,[8]19.1192,

;; ./llama-perplexity -f ~/wikitext-2-raw/wiki.test.raw -ngl 0 -m ~/llama.cpp/models/SmolLM-135M.Q8_0.gguf
;; [1]11.2266,[2]18.2823,[3]17.7238,[4]17.6471,[5]17.7210,[6]17.9894,[7]18.4318,[8]19.1116,

;; (perplexity #P"~/wikitext-2-raw/wiki.test.raw" :metal t :model "~/llama.cpp/models/SmolLM-135M.Q8_0.gguf")
;; [1]11.2329 [2]18.2887 [3]17.7327 [4]17.6559 [5]17.7286 [6]17.9963 [7]18.4397 [8]19.1192

;; (perplexity #P"~/wikitext-2-raw/wiki.test.raw" :metal nil :model "~/llama.cpp/models/SmolLM-135M.Q8_0.gguf")
;; [1]11.2266 [2]18.2823 [3]17.7238 [4]17.6471 [5]17.7210 [6]17.9894 [7]18.4318 [8]19.1116 

