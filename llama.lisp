(in-package :llama)

(defun allocate-candidates (size)
  #+lispworks (fli:allocate-foreign-object :type '(:struct llama-token-data) :nelems size)
  #+allegro (ff:allocate-fobject `(:array llama-token-data ,size))
  #-(or lispworks allegro) (cffi::foreign-alloc '(:struct llama-token-data) :count size))

(defun allocate-candidates-p ()
  #+lispworks (fli:allocate-foreign-object :type '(:struct llama-token-data-array))
  #+allegro (ff:allocate-fobject 'llama-token-data-array :c)
  #-(or lispworks allegro) (cffi::foreign-alloc '(:struct llama-token-data-array)))

(defun init-candidates (candidates logits)
  #+lispworks
  (loop for id below (length logits)
	for ptr = (fli:dereference candidates :index id :copy-foreign-object nil)
	do (setf (fli:foreign-slot-value ptr 'id) id
		 (fli:foreign-slot-value ptr 'logit) (elt logits id)
		 (fli:foreign-slot-value ptr 'p) 0.0))
  #+allegro		    
  (loop for id below (length logits)
	for ptr = (ff:fslot-value candidates id)
	do (setf (ff:fslot-value-typed 'llama-token-data :c ptr 'id) id
		 (ff:fslot-value-typed 'llama-token-data :c ptr 'logit) (elt logits id)
		 (ff:fslot-value-typed 'llama-token-data :c ptr 'p) 0.0))
  #-(or lispworks allegro)
  (loop for id below (length logits)
	for ptr = (cffi:mem-aptr candidates '(:struct llama-token-data) id)
	do (setf (cffi:foreign-slot-value ptr '(:struct llama-token-data) 'id) id
		 (cffi:foreign-slot-value ptr '(:struct llama-token-data) 'logit) (elt logits id)
		 (cffi:foreign-slot-value ptr '(:struct llama-token-data) 'p) 0.0)))

(defun init-candidates-p (candidates-p candidates n-vocab)
  #+lispworks
  (setf (fli:foreign-slot-value candidates-p 'data) candidates
	(fli:foreign-slot-value candidates-p 'size) n-vocab
	(fli:foreign-slot-value candidates-p 'sorted) nil)
  #+allegro
  (setf (ff:fslot-value-typed 'llama-token-data-array :c candidates-p 'data) (ff:fslot-address candidates 0)
	(ff:fslot-value-typed 'llama-token-data-array :c candidates-p 'size) n-vocab
	(ff:fslot-value-typed 'llama-token-data-array :c candidates-p 'sorted) 0)
  #-(or lispworks allegro)
  (setf (cffi:foreign-slot-value candidates-p '(:struct llama-token-data-array) 'data) candidates
	(cffi:foreign-slot-value candidates-p '(:struct llama-token-data-array) 'size) n-vocab
	(cffi:foreign-slot-value candidates-p '(:struct llama-token-data-array) 'sorted) nil))

(defun get-id (tok idx)
  #+lispworks (fli:dereference (ptr tok) :index idx)
  #+allegro (ff:fslot-value (ptr tok) idx)
  #-(or lispworks allegro) (cffi:mem-aref (ptr tok) :int idx))

(defun push-id (tok id)
  #+lispworks (setf (fli:dereference (ptr tok) :index (n tok)) id)
  #+allegro (setf (ff:fslot-value (ptr tok) (n tok)) id)
  #-(or lispworks allegro) (setf (cffi:mem-aref (ptr tok) :int (n tok)) id)
  (incf (n tok)))

(defun llama (&key (prompt "A") (predict *predict*) (model *model*) (threads *threads*) (verbose 0) (numa *numa*)
		(stream t) (metal t) (seed (random (expt 2 30))) (n-ctx *n-ctx*) (n-batch *n-batch*) (n-keep *n-keep*)
		(top-k *top-k*) (tfs-z *tfs-z*) (top-p *top-p*) (typical-p *typical-p*) (temp *temp*)
		(mirostat *mirostat*) (mirostat-eta *mirostat-eta*) (mirostat-tau *mirostat-tau*)
		(repeat-last-n *repeat-last-n*) (repeat-penalty *repeat-penalty*)
		(presence-penalty *presence-penalty*) (frequency-penalty *frequency-penalty*)
		(penalize-newlines *penalize-newlines*) (add-initial-space t) (add-beginning-of-sentence t))
  (assert (<= n-ctx *max-ctx*))
  #+sbcl (sb-ext::set-floating-point-modes :traps nil)
  (llama-backend-init numa)
  (let ((ctx (make-instance 'context :model model :params (context-parameters :n-ctx n-ctx :seed seed
									      :n-gpu-layers (if metal 1 0))))
	(embd-inp (make-instance 'tokens :size n-ctx)))
    (when add-initial-space
      ;; // Add a space in front of the first character to match OG llama tokenizer behavior      
      (setf prompt (concatenate 'string " " prompt)))
    ;; // tokenize the prompt    
    (tokenize ctx embd-inp prompt :add-beginning-of-sentence add-beginning-of-sentence)
    (when (> (n embd-inp) (- n-ctx 4))
      (error "prompt too long (~D tokens, max ~D)" (n embd-inp) (- n-ctx 4)))
    ;; // number of tokens to keep when resetting context
    (when (or (null n-keep) (minusp n-keep) (> n-keep (n embd-inp))) ;; TODO: instruct
      (setf n-keep (n embd-inp)))
    (when (>= verbose 2)
      (loop for token in (list-tokens embd-inp)
	    do (format t "~& ~6D -> '~A'~&" token (get-token ctx token))))
    (when (>= verbose 1)
      (format t "~&sampling: repeat-last-n=~D repeat-penalty=~F presence-penalty=~F frequency-penalty=~F
top-k=~D tfs-z=~F top-p=~F typycal-p=~F temp=~F mirostat=~D mirostat-lr=~D mirostat-ent=~F"
	      repeat-last-n repeat-penalty presence-penalty frequency-penalty
	      top-k tfs-z top-p typical-p temp mirostat mirostat-eta mirostat-tau)
      (format t "~&generate: n-ctx=~D n-batch=~D predict=~D n-keep=~D" n-ctx n-batch predict n-keep))
    ;; // TODO: replace with ring-buffer
    ;; std::vector<llama_token> last_n_tokens(n_ctx);
    ;; std::fill(last_n_tokens.begin(), last_n_tokens.end(), 0);
    (format stream "~&~A" prompt)
    (loop with n-remain = predict
	  with n-past = 0
	  with n-consumed = 0
	  with last-tokens = (make-instance 'circular-buffer :size n-ctx)
	  with embd = (make-instance 'tokens :size *max-ctx*)
	  with id = 0
	  while (plusp n-remain)
	  ;; // predict	  
	  do (when (plusp (n embd))
	       ;; // infinite text generation via context swapping
	       ;; // if we run out of context:
	       ;; // - take the n_keep first tokens from the original prompt (via n_past)
	       ;; // - take half of the last (n_ctx - n_keep) tokens and recompute the logits in a batch
	       (when (> (+ n-past (n embd)) n-ctx)
		 (let ((n-left (- n-past n-keep)))
		   (setf n-past (max 1 n-keep)) ;; // always keep the first token - BOS
		   ;; // insert n_left/2 tokens at the start of embd from last_n_tokens
		   ;; embd.insert(embd.begin(), last_n_tokens.begin() + n_ctx - n_left/2 - embd.size(), last_n_tokens.end() - embd.size());
;;		   (format t "~& n-left: ~A  n-past: ~A  n-keep: ~A~&" n-left n-past n-keep)
		   (let (;;(current (loop for i below (n embd) collect (get-id embd i)))
			 (extended (loop for i from (- (+ (n embd) (floor (/ n-left 2)))) to -1
					 collect (elt (cb-content last-tokens) (+ n-ctx i)))))
;;		     (format t "~A => ~A" current extended)
		     (setf (n embd) 0)
		     (loop for id in extended do (push-id embd id)))))
;;	       (format t "~&embd: ~A~&" (list-tokens embd :limit nil))
               ;; // evaluate tokens in batches
               ;; // embd is typically prepared beforehand to fit within a batch, but not always
	       (loop with i = 0
		     while (< i (n embd))
		     for n-eval = (min (- (n embd) i) n-batch)
		     do (evaluate ctx (subset embd i n-eval) n-past threads)
			(incf n-past n-eval)
			(incf i n-eval))
	       (setf (n embd) 0))
	     (if (> (n embd-inp) n-consumed)
		 ;; // some user input remains from prompt or interaction, forward it to processing
		 (loop while (> (n embd-inp) n-consumed)
		       repeat n-batch 
		       do (let ((id (get-id embd-inp n-consumed)))
			    (push-id embd id)
			    (cb-push last-tokens id)
			    (incf n-consumed)))
		 ;; // out of user input, sample next token
		 (let ((logits (get-logits ctx))
		       (n-vocab (n-vocab ctx)))
		   (assert (= n-vocab (length logits)))
                   ;; // Apply params.logit_bias map
                   ;; for (auto it = params.logit_bias.begin(); it != params.logit_bias.end(); it++) {
                   ;;   logits[it->first] += it->second;
                   ;; }
		   (let ((candidates (allocate-candidates n-vocab))
			 (candidates-p (allocate-candidates-p)))
		     (init-candidates candidates logits)
		     (init-candidates-p candidates-p candidates n-vocab)
                     ;; // Apply penalties
		     (let ((newline-logit (elt logits (llama-token-nl)))
			   (last-n-tokens (subseq (cb-content last-tokens)
						  (max 0 (- (cb-length last-tokens) repeat-last-n)))))
                       (when (plusp (length last-n-tokens))
			 (sample-repetition-penalty ctx candidates-p last-n-tokens repeat-penalty)
			 (sample-frequency-and-presence-penalties ctx candidates-p last-n-tokens frequency-penalty presence-penalty)
			 (unless penalize-newlines (setf (elt logits (llama-token-nl)) newline-logit))))
		     (setf id (if (< temp 0) ;; // Greedy sampling
				  (sample-token-greedy ctx candidates-p)
				  (ecase mirostat
				    (1 (let ((mirostat-mu (* 2.0 mirostat-tau))
					     (mirostat-m 100))
					 (llama-sample-temperature ctx candidates-p temp)
					 (llama-sample-token-mirostat ctx candidates-p mirostat-tau mirostat-eta mirostat-m mirostat-mu)))
				    (2 (let ((mirostat-mu (* 2.0 mirostat-tau)))
					 (llama-sample-temperature ctx candidates-p temp)
					 (llama-sample-token-mirostat-v2 ctx candidates-p mirostat-tau mirostat-eta mirostat-mu)))
				    (0 ;; // Temperature sampling
				     (sample-top-k ctx candidates-p top-k)
				     (sample-tail-free ctx candidates-p tfs-z)
				     (sample-typical ctx candidates-p typical-p)
				     (sample-top-p ctx candidates-p top-p)
				     (sample-temperature ctx candidates-p temp)
				     (sample-token ctx candidates-p)))))
		     #+lispworks (progn (fli:free-foreign-object candidates-p) (fli:free-foreign-object candidates))
		     #+allegro (ff:free-fobject candidates-p)
		     #-(or lispworks allegro) (progn (cffi:foreign-free candidates-p) (cffi:foreign-free candidates))
		     (cb-push last-tokens id)
		     ;; // add it to the context
		     (push-id embd id)
		     ;; // decrement remaining sampling budget
		     (decf n-remain)
		     )))
;;	     (format t "~&last-tokens: ~A~&" (cb-content last-tokens))
	     ;; // display text
	     (when (plusp id) (format stream "~A" (get-token ctx id)))
	     ;; // end of text token
	     (when (equal id (llama-token-eos))
	       (format stream " [end of text]~&")
	       (setf n-remain 0)))
    (if (>= verbose 3) (print-timings ctx))))

;; ./main  --prompt "in the first part of" --n_predict 20 --seed 42
;; in the first part of the book we see the development of the main characters, we witness their fights and their love for

;; ./main  --prompt "in the first part of" --n_predict 20 --seed 42 -ngl 1
;; in the first part of the book we see the development of the main characters, we witness their fights and their love for

;; (llama::llama :prompt "in the first part of" :predict 20 :seed 42 :metal nil)
;; in the first part of the book we see the development of the main characters, we witness their fights and their love for

;; (llama::llama :prompt "in the first part of" :predict 20 :seed 42 :metal t)
;; in the first part of the book we see the development of the main characters, we witness their fights and their love for
