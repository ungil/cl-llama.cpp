(in-package :llama)

(defun %embedding (ctx tokens txt verbose add-initial-space add-beginning-of-sentence threads)
  (when add-initial-space
    ;; // Add a space in front of the first character to match OG llama tokenizer behavior
    (setf txt (concatenate 'string " " txt)))
  (tokenize ctx tokens txt :add-beginning-of-sentence add-beginning-of-sentence)
  (when (> verbose 0) (print (list-tokens tokens :context ctx :limit nil)))
  (evaluate ctx tokens 0 threads) ;; n-past = 0
  (get-embeddings ctx))

(defun embedding (prompt &key (model *model*) (n-ctx *n-ctx*) (ntokens n-ctx) (verbose 0)
			   (add-initial-space t) (add-beginning-of-sentence t) (threads *threads*))
  "Calculate embeddings for the given prompt. If passed a list of prompts it will loop over them."
  #+sbcl (sb-ext::set-floating-point-modes :traps nil)
  (let* ((ctx (make-instance 'context :model model
				      :params (context-parameters :f16-kv t :embedding t :n-ctx n-ctx)))
	 (tokens (make-instance 'tokens :size ntokens)))
    (prog1
	(if (listp prompt)
	    (loop for text in prompt
		  collect (%embedding ctx tokens text verbose add-initial-space add-beginning-of-sentence threads))
	    (%embedding ctx tokens prompt verbose add-initial-space add-beginning-of-sentence threads))
      (when (> verbose 1) (print-timings ctx)))))
      
;; ./embedding -p "testing" | head -c 28
;; 1.384214 -1.668122 0.817734

;; (subseq (llama:embedding "testing") 0 3)
;; #(1.384214 -1.6681216 0.81773395)
