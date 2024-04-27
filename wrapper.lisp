(in-package :llama)

(defconstant +default-piece-size+ 8)
(defvar *lib* (asdf:system-relative-pathname "llama" "llama.cpp/libllama.so"))

(unless (probe-file *lib*)
  (error "to build the library execute 'make clean libllama.so' in the llama.cpp subdirectory"))

#-(or lispworks allegro)
(progn
  (cffi:load-foreign-library *lib*)
  (format t "~%~%   Foreign library ~A loaded~%~%~%" *lib*))

#-(or lispworks allegro) (load (asdf:system-relative-pathname "llama" "llama-cffi.lisp"))

#+lispworks
(progn
  (fli:register-module "libllama.so" :file-name *lib*)
  (format cl:t "~%~%   Foreign library ~A loaded~%~%~%" *lib*))

#+lispworks (assert (not (eq lw:*default-character-element-type* 'base-char)))

#|
(require "foreign-parser")

(defun pretty-name (name) (string-upcase (substitute #\- #\_ name)))

(foreign-parser:process-foreign-file (asdf:system-relative-pathname "llama" "llama.cpp/llama.h")
				     :dff (asdf:system-relative-pathname "llama" "llama-dff-original.lisp")
				     :package "LLAMA" :case-sensitive '(:user-routine pretty-name))

;; make a copy of llama-dff-original.lisp as llama-dff.lisp
;; keep only the definitions of size-t (derived from stddef.h)
;; and uint8-t (derived from _uint8_t.h) and the whole
;; last section derived from llama.h changing occurrences of
;;   (:pointer (:const :char))
;; in function arguments only (without modifying result types) to
;;   (:reference-pass :ef-mb-string)
;; except for argument text in llama-tokenize that should have the form
;;   (:reference-pass (:ef-mb-string :external-format :utf-8))
|#

#+lispworks (load (asdf:system-relative-pathname "llama" "llama-dff.lisp"))

#+allegro
(progn
  (load *lib* :foreign t) ;;"libllama.so" :file-name library)
  (format cl:t "~%~%   Foreign library ~A loaded~%~%~%" *lib*))

#+allegro (load (asdf:system-relative-pathname "llama" "llama-ff.lisp"))

(defclass model-params ()
  ((n-gpu-layers :initarg :n-gpu-layers)
   (split-mode :initarg :split-mode)   
   (main-gpu :initarg :main-gpu)
   (tensor-split :initarg :tensor-split)
   (progress-callback :initarg :progress-callback)
   (progress-callback-user-data :initarg :progress-callback-user-data)
   (vocab-only :initarg :vocab-only)
   (kv-overrides :initarg :kv-overrides)   
   (use-mmap :initarg :use-mmap)
   (use-mlock :initarg :use-mlock)
   #+(or lispworks allegro) foreign-struct))

(defun model-default-params ()
  #+lispworks (let ((ptr (fli:allocate-foreign-object :pointer-type '(:pointer (:struct llama-model-params)))))
		(llama-model-default-params :result-pointer ptr))
  #-lispworks (llama-model-default-params))

(defclass context-params ()
  ((seed :initarg :seed)
   (n-ctx :initarg :n-ctx)
   (n-batch :initarg :n-batch)
   (n-ubatch :initarg :n-ubatch)
   (n-seq-max :initarg :n-seq-max)
   (n-threads :initarg :n-threads)
   (n-threads-batch :initarg :n-threads-batch)

   (rope-scaling-type :initarg :rope-scaling-type)
   (pooling-type :initarg :pooling-type)

   (rope-freq-base :initarg :rope-freq-base)
   (rope-freq-scale :initarg :rope-freq-scale)
   (yarn-ext-factor :initarg :yarn-ext-factor)
   (yarn-attn-factor :initarg :yarn-attn-factor)
   (yarn-beta-fast :initarg :yarn-beta-fast)
   (yarn-beta-slow :initarg :yarn-beta-slow)
   (yarn-orig-ctx :initarg :yarn-orig-ctx)
   (defrag-thold :initarg :defrag-thold)
   
   (cb-eval :initarg :cb-eval)
   (cb-eval-user-data :initarg :cb-eval-user-data)

   (type-k :initarg :type-k)
   (type-v :initarg :type-v)

   (logits-all :initarg :logits-all)
   (embedding :initarg :embedding)
   (offload-kqv :initarg :offload-kqv)
   (abort-callback :initarg :abort-callback)
   (abort-callback-data :initarg :abort-callback-data)
   #+(or lispworks allegro) foreign-struct))

(defun context-default-params ()
  #+lispworks (let ((ptr (fli:allocate-foreign-object :pointer-type '(:pointer (:struct llama-context-params)))))
		(llama-context-default-params :result-pointer ptr))
  #-lispworks (llama-context-default-params))

(defun max-devices ()
  (llama-max-devices))

(defun mmap-supported ()
  (llama-supports-mmap))

(defun mlock-supported ()
  (llama-supports-mlock))

(defun model-from-file (filename &optional (params (model-default-params)))
  (let ((file (namestring (probe-file (namestring filename)))))
    (assert file)
    (llama-load-model-from-file file params)))

(defun context-from-model (model &optional (params (context-default-params)))
  (assert model)
  (llama-new-context-with-model model params))

#+lispworks
(defmethod initialize-instance :after ((obj model-params) &key)
  (let ((params (model-default-params)))
    (loop for foreign-slot in (fli:foreign-slot-names params)
	  for slot = (intern (string-upcase (substitute #\- #\_ (symbol-name foreign-slot))) "LLAMA")
	  if (slot-boundp obj slot)
	    do (setf (fli:foreign-slot-value params foreign-slot) (slot-value obj slot))
	  else
	    do (setf (slot-value obj slot) (fli:foreign-slot-value params foreign-slot)))
    (setf (slot-value obj 'foreign-struct) params)
    (tg:finalize obj (lambda () (fli:free params)))
    obj))

#+lispworks
(defmethod initialize-instance :after ((obj context-params) &key)
  (let ((params (context-default-params)))
    (loop for foreign-slot in (fli:foreign-slot-names params)
	  for slot = (intern (string-upcase (substitute #\- #\_ (symbol-name foreign-slot))) "LLAMA")
	  if (slot-boundp obj slot)
	    do (setf (fli:foreign-slot-value params foreign-slot) (slot-value obj slot))
	  else
	    do (setf (slot-value obj slot) (fli:foreign-slot-value params foreign-slot)))
    (setf (slot-value obj 'foreign-struct) params)
    (tg:finalize obj (lambda () (fli:free params)))
    obj))

(defun context-parameters (&rest args)
  #+lispworks (apply #'make-instance 'context-params args)
  #-lispworks (let ((params (context-default-params)))
		(loop for (key value) on args by #'cddr
		      do #+allegro (setf (ff:fslot-value params (intern (symbol-name key) "LLAMA"))
					  (if (numberp value) value (if value 1 0)))
		      #-allegro (setf (slot-value params (intern (symbol-name key) "LLAMA")) value))
		params))

(defclass mdl ()
  ((file :initarg :file :accessor file)
   (params :initarg :params :accessor params
	   :initform #+lispworks (make-instance 'model-params)
		     #-lispworks (model-default-params))
   (foreign-pointer :accessor ptr)))

(defmethod initialize-instance :after ((mdl mdl) &key)
  (let ((ptr (model-from-file (file mdl)
			      #+lispworks (slot-value (params mdl) 'foreign-struct)
			      #-lispworks (params mdl))))
    (setf (slot-value mdl 'foreign-pointer) ptr)
    (tg:finalize mdl (lambda () (llama-free-model ptr))))
  mdl)

(defun model-parameters (&rest args)
  #+lispworks (apply #'make-instance 'model-params args)
  #-lispworks (let ((params (model-default-params)))
		(loop for (key value) on args by #'cddr
		      do #+allegro (setf (ff:fslot-value params (intern (symbol-name key) "LLAMA"))
					  (if (numberp value) value (if value 1 0)))
		      #-allegro (setf (slot-value params (intern (symbol-name key) "LLAMA")) value))
		params))

(defmethod n-vocab ((mdl mdl))
  (llama-n-vocab (ptr mdl)))

(defmethod n-embd ((mdl mdl))
  (llama-n-embd (ptr mdl)))

(defmethod size ((mdl mdl))
  (llama-model-size (ptr mdl)))

(defmethod n-params ((mdl mdl))
  (llama-model-n-params (ptr mdl)))

(defmethod desc ((mdl mdl))
  #+lispworks
  (fli:with-dynamic-foreign-objects ()
    (let ((c-string (fli:allocate-dynamic-foreign-object :type :char :nelems 100)))
      (llama-model-desc (ptr mdl) c-string 100)
      (fli:convert-from-foreign-string c-string)))
  #+allegro
  (let ((c-string (ff:allocate-fobject :char :c 100)))
    (unwind-protect
	 (progn (llama-model-desc (ptr mdl) c-string 100)
		(excl:native-to-string c-string))
      (ff:free-fobject c-string)))
  #-(or lispworks allegro)
  (cffi:with-foreign-pointer-as-string ((buf buf-size) 100)
    (llama-model-desc (ptr mdl) buf buf-size)
    (cffi:foreign-string-to-lisp buf)))

(defmethod print-object ((obj mdl) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A" (desc obj) (file obj))))

(defclass batch ()
  ((n-tokens :initarg :n-tokens)
   (token :initarg :token)
   (embd :initarg :embd)
   (pos :initarg :pos)

   (n-seq-id :initarg :n-seq-id)
   (seq-id :initarg :seq-id)
   (logits :initarg :logits)

   (all-pos-0 :initarg :all-pos-0)
   (all-pos-1 :initarg :all-pos-1)
   (all-seq-id :initarg :all-seq-id)))

(defclass ctx ()
  ((model :initarg :model :accessor model)
   (params :initarg :params :accessor params
	   :initform #+lispworks (make-instance 'context-params)
		     #-lispworks (context-default-params))
   (foreign-pointer :accessor ptr)))

(defmethod initialize-instance :after ((ctx ctx) &key)
  (let ((ptr (context-from-model (ptr (model ctx))
				 #+lispworks (slot-value (params ctx) 'foreign-struct)
				 #-lispworks (params ctx))))
    (setf (ptr ctx) ptr)
    (tg:finalize ctx (lambda () (llama-free ptr))))
  ctx)

(defmethod n-vocab ((ctx ctx))
  (n-vocab (model ctx)))

(defmethod n-ctx ((ctx ctx))
  (llama-n-ctx (ptr ctx)))

(defmethod n-embd ((ctx ctx))
  (n-embd (model ctx)))

(defmethod vocab-type ((ctx ctx))
  (llama-vocab-type (ptr ctx)))

(defmethod print-object ((obj ctx) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A" (model obj) (params obj))))

(defun batch-add (batch id pos seq-ids logits)
  ;; FIXME: support non-CFFI
  (let ((n-tokens (slot-value batch 'n-tokens)))
    (setf (cffi:mem-aref (slot-value batch 'token) :int32 n-tokens) id)
    (setf (cffi:mem-aref (slot-value batch 'pos) :int32 n-tokens) pos)
    (setf (cffi:mem-aref (slot-value batch 'n-seq-id) :int32 n-tokens)
	  (length seq-ids))
    (loop for seq-id in seq-ids
	  for i from 0
	  do
	     (setf (cffi:mem-aref (cffi:mem-aref (slot-value batch 'seq-id) :pointer n-tokens)
				  :int32
				  i)
		   seq-id))
    (setf (cffi:mem-aref (slot-value batch 'logits) :int8 n-tokens) logits)
    (incf (slot-value batch 'n-tokens))))

(defun build-init-batch (tokens n-batch)
  ;; FIXME: support non-CFFI
  (loop with batch = (llama-batch-init n-batch 0 1)
	with token-ids = (list-tokens tokens)
	for id in token-ids
	for pos from 0
	do
	   (batch-add batch id pos '(0) 0)
	finally
	   (setf (cffi:mem-aref (slot-value batch 'logits)
				:int8
				(1- (slot-value batch 'n-tokens)))
		 1)
	   (return batch)))

(defmethod evaluate ((ctx ctx) tokens n-batch)
  (let ((batch (build-init-batch tokens n-batch)))
    (assert (= (llama-decode (ptr ctx) batch) 0))
    (llama-batch-free batch)))

(defmethod decode ((ctx ctx) batch)
  (assert (= (llama-decode (ptr ctx) batch) 0)))

(defclass tokens ()
  ((n :accessor n :initform 0)
   (size :initarg :size :initform (error "specify :size (buffer size)") :accessor size)
   (foreign-pointer :accessor ptr)))

(defmethod initialize-instance :after ((tok tokens) &key)
  (let ((ptr #+lispworks (fli:allocate-foreign-object :type :int :nelems (size tok))
	     #+allegro (ff:allocate-fobject (list :array :int (size tok)))
	     #-(or lispworks allegro) (cffi::foreign-alloc :int :count (size tok))))
    (setf (ptr tok) ptr)
    (tg:finalize tok (lambda () #+lispworks (fli:free ptr)
			     ;;#+allegro (ff:free-fobject ptr)
			     #-(or lispworks allegro) (cffi:foreign-free ptr))))
  tok)

(defmethod list-tokens ((tok tokens) &key (limit 10) context)
  (let ((ids (loop for i below (n tok)
		   repeat (or limit (n tok))
		   collect
		   #+lispworks (fli:dereference (ptr tok) :index i)
		   #+allegro (ff:fslot-value (ptr tok) i)
		   #-(or lispworks allegro) (cffi:mem-aref (ptr tok) :int i))))
    (if context
	(mapcar (lambda (id) (get-token context id)) ids)
	ids)))

(defmethod subset ((tok tokens) start length &key change-first-to-bos)
  (let ((out (make-instance 'tokens :size length)))
    (setf (n out) length)
    (loop for tgt below length
	  for src from start
	  do #+lispworks (setf (fli:dereference (ptr out) :index tgt)
			       (fli:dereference (ptr tok) :index src))
	  #+allegro (setf (ff:fslot-value (ptr out) tgt)
			  (ff:fslot-value (ptr tok) src))
	  #-(or lispworks allegro) (setf (cffi:mem-aref (ptr out) :int tgt)
					 (cffi:mem-aref (ptr tok) :int src)))
    (when change-first-to-bos (setf #+lispworks (fli:dereference (ptr out) :index 0)
				    #+allegro (ff:fslot-value (ptr out) 0)
				    #-(or lispworks allegro) (cffi:mem-aref (ptr out) :int 0)
				    (token-bos change-first-to-bos)))
    out))

(defmethod print-object ((obj tokens) stream)
  (print-unreadable-object (obj stream :type t)
    (let* ((limit 10)
	   (ids (list-tokens obj :limit limit))
	   (add (max 0 (- (n obj) limit))))
      (format stream "~A and ~D tokens more" ids add))))

(defmethod get-one-batch ((toks tokens) n-tokens pos-0 seq-id)
  ;; FIXME: other FFI
  (llama-batch-get-one (ptr toks) n-tokens pos-0 seq-id))

(defmethod tokenize ((mdl mdl) (tok fixnum) text &key add-beginning-of-sentence special)
  (tokenize mdl (make-instance 'tokens :size tok)
	    text :add-beginning-of-sentence add-beginning-of-sentence :special special))

(defmethod tokenize ((mdl mdl) (tok tokens) text &key add-beginning-of-sentence special)
  (let ((res (llama-tokenize (ptr mdl) text (length text)
			     (ptr tok) (size tok) add-beginning-of-sentence special)))
    (when (minusp res) (error "returned ~D, more than buffer size ~D" (- res) (size tok)))
    (setf (n tok) res)
    tok))

(defmethod get-embeddings ((ctx ctx))
  (let ((ptr (llama-get-embeddings (ptr ctx))))
    (if #+lispworks (fli:null-pointer-p ptr) #+allegro (zerop ptr) #-(or allegro lispworks) (cffi:null-pointer-p ptr)
	(values nil t)
	(let ((out (make-array (list (n-embd (model ctx))) :initial-element 0.0 :element-type 'float)))
	  (loop for i below (length out)
		do (setf (aref out i)
			 #+lispworks (fli:dereference ptr :index i)
			 #+allegro (ff:fslot-value-typed '(:array :float) :c ptr i)
			 #-(or lispworks allegro) (cffi:mem-aref ptr :float i)))
	  out))))

(defmethod get-logits ((ctx ctx) &optional (n 1))
  (let ((ptr (llama-get-logits (ptr ctx))))
    (unless #+lispworks (fli:null-pointer-p ptr) #+allegro (zerop ptr) #-(or lispworks allegro) (cffi:null-pointer-p ptr)
	    (let ((out (make-array (if (= n 1) (list (n-vocab ctx)) (list n (n-vocab ctx)))
				   :initial-element 0.0 :element-type 'single-float)))
	      (loop for i below (array-total-size out)
		    do (setf (row-major-aref out i)
			     #+lispworks (fli:dereference ptr :index i)
			     #+allegro (ff:fslot-value-typed '(:array :float) :c ptr i)
			     #-(or lispworks allegro) (cffi:mem-aref ptr :float i)))
	      out))))

(defmethod get-token ((ctx ctx) id)
  (get-token (model ctx) id))

(defmethod get-token ((mdl mdl) id)
  (ignore-errors 
   #+lispworks (fli:convert-from-foreign-string (llama-token-get-text (ptr mdl) id)
						:external-format :utf-8)
   #-lispworks (llama-token-get-text (ptr mdl) id)))

(defmethod to-piece ((ctx ctx) id)
  (to-piece (model ctx) id))

(defmethod to-piece ((mdl mdl) id)
  ;; FIXME: support lispworks
  (let* ((buf-size +default-piece-size+)
	 (buf (cffi:foreign-alloc :char :count buf-size))
	 (n-tokens (llama-token-to-piece (ptr mdl) id buf buf-size)))
    (when (< n-tokens 0)
      (cffi:foreign-free buf)
      (setq buf-size (- n-tokens))
      (setq buf (cffi:foreign-alloc :char :count buf-size))
      (let ((check (llama-token-to-piece (ptr mdl) id buf buf-size)))
	(assert (= check (- n-tokens)))
	(setq n-tokens check)))
    (prog1 (cffi:foreign-string-to-lisp buf :count n-tokens)
      (cffi:foreign-free buf))))

(defmethod print-prompt ((ctx ctx) tokens stream)
  (print-prompt (model ctx) tokens stream))

(defmethod print-prompt ((mdl mdl) tokens stream)
  (dolist (token-id (list-tokens tokens))
    (princ (to-piece mdl token-id) stream))
  (terpri stream)
  (force-output stream))

(defmethod get-vocab ((ctx ctx))
  (loop for id below (n-vocab (model ctx)) collect (get-token ctx id)))

(defmethod token-bos ((ctx ctx))
  (token-bos (model ctx)))

(defmethod token-eos ((ctx ctx))
  (token-eos (model ctx)))

(defmethod token-nl ((ctx ctx))
  (token-nl (model ctx)))

(defmethod token-bos ((mdl mdl))
  (llama-token-bos (ptr mdl)))

(defmethod token-eos ((mdl mdl))
  (llama-token-eos (ptr mdl)))

(defmethod token-nl ((mdl mdl))
  (llama-token-nl (ptr mdl)))

(defmethod sample-repetition-penalties ((ctx ctx) candidates last-tokens repeat-penalty frequency-penalty presence-penalty)
  (let ((ptr #+lispworks (fli:allocate-foreign-object :type :int :nelems (length last-tokens))
	     #+allegro (ff:allocate-fobject (list :array :int  (length last-tokens)))
	     #-(or lispworks allegro) (cffi::foreign-alloc :int :count (length last-tokens))))
    (unwind-protect
	 (progn
	   (loop for i below (length last-tokens)
		 do (setf #+lispworks (fli:dereference ptr :index i)
			  #+allegro (ff:fslot-value ptr i)
			  #-(or lispworks allegro) (cffi:mem-aref ptr :int i)
			  (elt last-tokens i)))
	   (llama-sample-repetition-penalties (ptr ctx) candidates ptr (length last-tokens)
					      (coerce repeat-penalty 'single-float)
					      (coerce frequency-penalty 'single-float)
					      (coerce presence-penalty 'single-float)))
      #+lispworks (fli:free ptr)
      ;;#+allegro (ff:free-fobject ptr)
      #-(or lispworks allegro) (cffi:foreign-free ptr))))

(defmethod sample-softmax ((ctx ctx) candidates)
  (llama-sample-softmax (ptr ctx) candidates))

(defmethod sample-top-k ((ctx ctx) candidates top-k &optional (min-keep 1))
  (llama-sample-top-k (ptr ctx) candidates top-k min-keep))

(defmethod sample-tail-free ((ctx ctx) candidates tfs-z &optional (min-keep 1))
  (llama-sample-tail-free (ptr ctx) candidates tfs-z min-keep))

(defmethod sample-typical ((ctx ctx) candidates typical-p &optional (min-keep 1))
  (llama-sample-typical (ptr ctx) candidates (coerce typical-p 'single-float) min-keep))

(defmethod sample-top-p ((ctx ctx) candidates top-p &optional (min-keep 1))
  (llama-sample-top-p (ptr ctx) candidates (coerce top-p 'single-float) min-keep))

(defmethod sample-temperature ((ctx ctx) candidates temp)
  (llama-sample-temp (ptr ctx) candidates (coerce temp 'single-float)))

(defmethod sample-token ((ctx ctx) candidates)
  (llama-sample-token (ptr ctx) candidates))

(defmethod sample-token-greedy ((ctx ctx) candidates)
  (llama-sample-token-greedy (ptr ctx) candidates))

(defmethod print-timings ((ctx ctx))
  (llama-print-timings(ptr ctx)))

(defmethod reset-timings ((ctx ctx))
  (llama-reset-timings (ptr ctx)))

(defun system-info ()
  #+lispworks (fli:convert-from-foreign-string (llama-print-system-info))
  #-lispworks (llama-print-system-info))
