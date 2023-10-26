(in-package :llama)

(defun normalize (x)
  (loop with sum = (loop for i in x sum i)
	for i in x collect (/ i sum)))

(defun softmax (logits)
  (normalize (loop with maxlogit = (loop for x in logits maximize x)
		   for x in logits collect (exp (* 1d0 (- x maxlogit))))))

(defun %perplexity (ctx tokens txt verbose add-beginning-of-sentence threads)
  (tokenize (model ctx) tokens txt :add-beginning-of-sentence add-beginning-of-sentence)
  (let ((nsegments (floor (n tokens) (n-ctx ctx)))) ;; do blocks of size n-ctx
    (when (plusp nsegments)
      (loop for segment below nsegments
	    for tok = (subset tokens (* (n-ctx ctx) segment) (n-ctx ctx) :change-first-to-bos ctx)
	    with count = 0
	    with nll = 0
	    do (when (> verbose 2) (print (list-tokens tok :limit 5)))
	       (evaluate ctx tok 0 threads) ;; n-past = 0
	       (let ((logits (get-logits ctx (n-ctx ctx)))
		     (ids (list-tokens tok :limit nil)))
		 (loop for i from (min 512 (/ (n-ctx ctx) 2)) below (1- (n-ctx ctx))
		       for token = (elt ids (1+ i))
		       for x = (loop for j below (n-vocab ctx) collect (aref logits i j))
		       for prob = (elt (softmax x) token)
		       do (incf nll (- (log prob)))
		       do (incf count)
		       finally (when (> verbose 0)
				 (format t (if (> verbose 2) "[~D]~1,6F " "[~D]~1,4F ")
					 (1+ segment) (exp (/ nll count))))))
	    finally (return (coerce (exp (/ nll count)) 'single-float))))))

(defun perplexity (text &key (model *model*) (n-ctx *n-ctx*) (verbose 0) (numa *numa*)
			  (add-beginning-of-sentence t) (threads *threads*) (metal *metal*))
  "Calculate perplexity as done in llama.cpp/examples/perplexity
Full blocks of length n-ctx are used (returns nil if there is not even one full block).
and the first half (or 512 tokens if larger than 1024) is discarded.
Cumulative values are printed after processing each block if <verbose> is at least 1.
If <text> is a pathname the contents of the file are used."
  #+sbcl (sb-ext::set-floating-point-modes :traps nil)
  (when (pathnamep text)
      (setf text (with-open-file (in text :external-format :utf-8 :element-type 'character)
		   (let ((contents (make-array (file-length in) :element-type 'character :adjustable t)))
		     (setf contents (adjust-array contents (list (read-sequence contents in))))
		     (if (equal (aref contents (1- (length contents))) #\Newline)
			 (adjust-array contents (list (1- (length contents))))
			 contents)))))
  (llama-backend-init numa)
  (let* ((mdl (make-instance 'mdl :file model
				  :params (model-parameters :n-gpu-layers (if metal 1 0))))
	 (ctx (make-instance 'ctx :model mdl
				  :params (context-parameters :f16-kv t :logits-all t :n-ctx n-ctx)))
	 (tokens (make-instance 'tokens :size (length text))))
    (prog1
	(%perplexity ctx tokens text verbose add-beginning-of-sentence threads)
      (when (> verbose 1) (print-timings ctx)))))

;; ./perplexity  -f ~/wikitext-2-raw/wiki.test.raw -ngl 1
;; [1]4.2343,[2]4.7119,[3]5.5786,[4]6.1821

;; (perplexity #P"~/wikitext-2-raw/wiki.test.raw" :verbose 1 :metal t)
;; [1]4.2343 [2]4.7119 [3]5.5786 [4]6.1821 

;; ./perplexity  -f ~/wikitext-2-raw/wiki.test.raw -ngl 0
;; [1]4.2344,[2]4.7123,[3]5.5789,[4]6.1824

;; (perplexity #P"~/wikitext-2-raw/wiki.test.raw" :verbose 1 :metal nil)
;; [1]4.2344 [2]4.7123 [3]5.5789 [4]6.1824 



