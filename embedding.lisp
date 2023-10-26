(in-package :llama)

(defun %embedding (ctx tokens txt verbose add-beginning-of-sentence threads)
  (tokenize (model ctx) tokens txt :add-beginning-of-sentence add-beginning-of-sentence)
  (when (> verbose 0) (print (list-tokens tokens :context ctx :limit nil)))  
  (evaluate ctx tokens 0 threads) ;; n-past = 0
  (get-embeddings ctx))

(defun embedding (prompt &key (model *model*) (n-ctx *n-ctx*) (ntokens n-ctx) (verbose 0) (numa *numa*)
			   (add-beginning-of-sentence t) (threads *threads*) (metal *metal*))
  "Calculate embeddings for the given prompt. If passed a list of prompts it will loop over them."
  #+sbcl (sb-ext::set-floating-point-modes :traps nil)
  (llama-backend-init numa)
  (let* ((mdl (make-instance 'mdl :file model
				  :params (model-parameters :n-gpu-layers (if metal 1 0))))
	 (ctx (make-instance 'ctx :model mdl
				  :params (context-parameters :embedding t :n-ctx n-ctx)))
	 (tokens (make-instance 'tokens :size ntokens)))
    (prog1
	(if (listp prompt)
	    (loop for text in prompt
		  do (format t ".")
		  collect (%embedding ctx tokens text verbose add-beginning-of-sentence threads))
	    (%embedding ctx tokens prompt verbose add-beginning-of-sentence threads))
      (when (> verbose 1) (print-timings ctx)))))

;; ./embedding -p "testing" -ngl 1 | head -c 28
;; 1.382774 -1.671184 0.820016 %

;; (subseq (embedding "testing" :metal t) 0 3)
;; #(1.3827736 -1.6711841 0.82001567)

;; ./embedding -p "testing" -ngl 0 | head -c 28
;; 1.387524 -1.680799 0.815600

;; (subseq (embedding "testing" :metal nil) 0 3)
;; #(1.3875245 -1.680799 0.8155995)
