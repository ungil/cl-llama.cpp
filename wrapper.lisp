;;;; -*- encoding:utf-8 -*-

(in-package :llama)

(defclass model-params ()
  ((devices :initarg :devices)
   (tensor-buft-overrides :initarg :tensor-buft-overrides)
   (n-gpu-layers :initarg :n-gpu-layers)
   (split-mode :initarg :split-mode)
   (main-gpu :initarg :main-gpu)
   (tensor-split :initarg :tensor-split)
   (progress-callback :initarg :progress-callback)
   (progress-callback-user-data :initarg :progress-callback-user-data)
   (kv-overrides :initarg :kv-overrides)
   (vocab-only :initarg :vocab-only)
   (use-mmap :initarg :use-mmap)
   (use-mlock :initarg :use-mlock)
   (check-tensors :initarg :check-tensors)
   (use-extra-bufts :initarg :use-extra-bufts)
   (no-host :initarg :no-host)
   #+(or lispworks allegro) foreign-struct))

(defun model-default-params ()
  #+lispworks (let ((ptr (fli:allocate-foreign-object :pointer-type '(:pointer (:struct llama-model-params)))))
		(llama-model-default-params :result-pointer ptr))
  #-lispworks (llama-model-default-params))

(defclass context-params ()
  ((n-ctx :initarg :n-ctx)
   (n-batch :initarg :n-batch)
   (n-ubatch :initarg :n-ubatch)
   (n-seq-max :initarg :n-seq-max)
   (n-threads :initarg :n-threads)
   (n-threads-batch :initarg :n-threads-batch)
   (rope-scaling-type :initarg :rope-scaling-type)
   (pooling-type :initarg :pooling-type)
   (attention-type :initarg :attention-type)
   (flash-attn-type :initarg :flash-attn-type)   
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
   (abort-callback :initarg :abort-callback)
   (abort-callback-data :initarg :abort-callback-data)
   (embeddings :initarg :embeddings)
   (offload-kqv :initarg :offload-kqv)
   (no-perf :initarg :no-perf)
   (op-offload :initarg :op-offload)
   (swa-full :initarg :swa-full)
   (kv-unified :initarg :kv-unified)
   #+(or lispworks allegro) foreign-struct))

(defun context-default-params ()
  #+lispworks (let ((ptr (fli:allocate-foreign-object :pointer-type '(:pointer (:struct llama-context-params)))))
		(llama-context-default-params :result-pointer ptr))
  #-lispworks (llama-context-default-params))

(defclass model-quantize-params ()
  ((nthread :initarg :nthread)
   (ftype :initarg :ftype)
   (output-tensor-type :initarg :output-tensor-type)
   (token-embedding-type :initarg :token-embedding-type)
   (allow-requantize :initarg :allow-requantize)
   (quantize-output-tensor :initarg :quantize-output-tensor)
   (only-copy :initarg :only-copy)
   (pure :initarg :pure)
   (keep-split :initarg :keep-split)
   (imatrix :initarg :imatrix)
   (kv-overrides :initarg :kv-overrides)
   #+(or lispworks allegro) foreign-struct))

(defun model-quantize-default-params ()
  #+lispworks (let ((ptr (fli:allocate-foreign-object :pointer-type '(:pointer (:struct llama-model-quantize-params)))))
		(llama-model-quantize-default-params :result-pointer ptr))
  #-lispworks (llama-model-quantize-default-params))

(defclass sampler-chain-params ()
  ((no-perf :initarg :no-perf)
   #+(or lispworks allegro) foreign-struct))

(defun sampler-chain-default-params ()
  #+lispworks (let ((ptr (fli:allocate-foreign-object :pointer-type '(:pointer (:struct llama-sampler-chain-params)))))
		(llama-sampler-chain-default-params :result-pointer ptr))
  #-lispworks (llama-sampler-chain-default-params))

(defun max-devices ()
  (llama-max-devices))

(defun mmap-supported ()
  (llama-supports-mmap))

(defun mlock-supported ()
  (llama-supports-mlock))

(defun gpu-offload-supported ()
  (llama-supports-gpu-offload))

(defun model-from-file (filename &optional (params (model-default-params)))
  (let ((file (namestring (probe-file (namestring filename)))))
    (assert file)
    (llama-model-load-from-file file params)))

(defun context-from-model (model &optional (params (context-default-params)))  
  (assert model)
  (llama-init-from-model model params))

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
    (tg:finalize obj (lambda () (fli:free-foreign-object params)))
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
    (tg:finalize obj (lambda () (fli:free-foreign-object params)))
    obj))

#+lispworks
(defmethod initialize-instance :after ((obj model-quantize-params) &key)
  (let ((params (model-quantize-default-params)))
    (loop for foreign-slot in (fli:foreign-slot-names params)
	  for slot = (intern (string-upcase (substitute #\- #\_ (symbol-name foreign-slot))) "LLAMA")
	  if (slot-boundp obj slot)
	    do (setf (fli:foreign-slot-value params foreign-slot) (slot-value obj slot))
	  else
	    do (setf (slot-value obj slot) (fli:foreign-slot-value params foreign-slot)))
    (setf (slot-value obj 'foreign-struct) params)
    (tg:finalize obj (lambda () (fli:free-foreign-object params)))
    obj))

#+lispworks
(defmethod initialize-instance :after ((obj sampler-chain-params) &key)
  (let ((params (sampler-chain-default-params)))
    (loop for foreign-slot in (fli:foreign-slot-names params)
	  for slot = (intern (string-upcase (substitute #\- #\_ (symbol-name foreign-slot))) "LLAMA")
	  if (slot-boundp obj slot)
	    do (setf (fli:foreign-slot-value params foreign-slot) (slot-value obj slot))
	  else
	    do (setf (slot-value obj slot) (fli:foreign-slot-value params foreign-slot)))
    (setf (slot-value obj 'foreign-struct) params)
    (tg:finalize obj (lambda () (fli:free-foreign-object params)))
    obj))

(defun context-parameters (&rest args)
  #+lispworks (apply #'make-instance 'context-params args)
  #-lispworks (let ((params (context-default-params)))
		(loop for (key value) on args by #'cddr
		      do #+allegro (setf (ff:fslot-value params (intern (symbol-name key) "LLAMA"))
					 (if (numberp value) value (if value 1 0)))
		      #-allegro (setf (slot-value params (intern (symbol-name key) "LLAMA")) value))
		params))

(defclass voc ()
  ((model :initarg :model :accessor model)
   (foreign-pointer :initarg :ptr :accessor ptr)))

(defclass mdl ()
  ((file :initarg :file :accessor file)
   (params :initarg :params :accessor params
	   :initform #+lispworks (make-instance 'model-params)
		     #-lispworks (model-default-params))
   (vocab :accessor vocab)
   (foreign-pointer :accessor ptr)))

(defmethod initialize-instance :after ((mdl mdl) &key)
  (let ((ptr (model-from-file (file mdl)
			      #+lispworks (slot-value (params mdl) 'foreign-struct)
			      #-lispworks (params mdl))))
    (assert (not #+lispworks (fli:null-pointer-p ptr)
		 #+allegro (zerop ptr)
		 #-(or lispworks allegro) (cffi:null-pointer-p ptr)))
    (setf (slot-value mdl 'foreign-pointer) ptr)
    (tg:finalize mdl (lambda () (llama-model-free ptr)))
    (setf (vocab mdl) (make-instance 'voc :ptr (llama-model-get-vocab ptr) :model mdl)))
  mdl)

(defun model-parameters (&rest args)
  #+lispworks (apply #'make-instance 'model-params args)
  #-lispworks (let ((params (model-default-params)))
		(loop for (key value) on args by #'cddr
		      do #+allegro (setf (ff:fslot-value params (intern (symbol-name key) "LLAMA"))
					 (if (numberp value) value (if value 1 0)))
		      #-allegro (setf (slot-value params (intern (symbol-name key) "LLAMA")) value))
		params))

(defmethod vocab-type ((voc voc))
  (ecase (llama-vocab-type (ptr voc))
    (0 :none)
    (1 :spm)   ;; LLaMA tokenizer based on byte-level BPE with byte fallback
    (2 :bpe)   ;; GPT-2 tokenizer based on byte-level BPE
    (3 :wpm))) ;; BERT tokenizer based on WordPiece

(defmethod vocab-type ((mdl mdl))
  (vocab-type (vocab mdl)))
  
(defmethod rope-type ((mdl mdl))
  (ecase (llama-model-rope-type (ptr mdl))
    (-1 :none)
    (0 :norm)
    (2 :neox)
    (4 :glm)))

(defmethod n-vocab ((voc voc))
  (llama-vocab-n-tokens (ptr voc)))

(defmethod n-vocab ((mdl mdl))
  (n-vocab (vocab mdl)))

(defmethod n-ctx-train ((mdl mdl))
  (llama-model-n-ctx-train (ptr mdl)))

(defmethod n-embd ((mdl mdl))
  (llama-model-n-embd (ptr mdl)))

(defmethod n-layer ((mdl mdl))
  (llama-model-n-layer (ptr mdl)))

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
    (assert (not #+lispworks (fli:null-pointer-p ptr)
		 #+allegro (zerop ptr)
		 #-(or lispworks allegro) (cffi:null-pointer-p ptr)))
    (setf (ptr ctx) ptr)
    (tg:finalize ctx (lambda () (llama-free ptr))))
  ctx)

(defmethod n-vocab ((ctx ctx))
  (n-vocab (model ctx)))

(defmethod vocab-type ((ctx ctx))
  (vocab-type (model ctx)))

(defmethod rope-type ((ctx ctx))
  (rope-type (model ctx)))

(defmethod n-ctx-train ((ctx ctx))
  (n-ctx-train (model ctx)))

(defmethod n-embd ((ctx ctx))
  (n-embd (model ctx)))

(defmethod n-layer ((ctx ctx))
  (n-embd (model ctx)))

(defmethod n-ctx ((ctx ctx))
  (llama-n-ctx (ptr ctx)))

(defmethod n-batch ((ctx ctx))
  (llama-n-batch (ptr ctx)))

(defmethod n-ubatch ((ctx ctx))
  (llama-n-ubatch (ptr ctx)))

(defmethod n-seq-max ((ctx ctx))
  (llama-n-seq-max (ptr ctx)))

(defmethod pooling-type ((ctx ctx))
  (ecase (llama-pooling-type (ptr ctx))
    (-1 :unspecified)
    (0 :none)
    (1 :mean)
    (2 :cls)
    (3 :last)))

(defmethod print-object ((obj ctx) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A" (model obj) (params obj))))

(defclass chain ()
  ((params :initarg :params :accessor params
	   :initform #+lispworks (make-instance 'sampler-chain-params)
		     #-lispworks (sampler-chain-default-params))
   (foreign-pointer :accessor ptr)))

(defmethod initialize-instance :after ((chain chain) &key)
  (let ((ptr (llama-sampler-chain-init #+lispworks (slot-value (params chain) 'foreign-struct)
				       #-lispworks (params chain))))
    (assert (not #+lispworks (fli:null-pointer-p ptr)
		 #+allegro (zerop ptr)
		 #-(or lispworks allegro) (cffi:null-pointer-p ptr)))
    (setf (ptr chain) ptr)
    (tg:finalize chain (lambda () (llama-sampler-free ptr))))
  chain)

(defmethod print-object ((obj chain) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~D" (llama-sampler-chain-n (ptr obj)))))

(defclass batch ()
  ((n-tokens-max :initarg :n-tokens-max :initform (error "n-tokens-max should be specified") :accessor n-tokens-max)
   (embd :initarg :embd :initform 0 :accessor embd)
   (n-seq-max :initarg :n-seq-max :initform 1 :accessor n-seq-max)
   (foreign-object :accessor #+(or lispworks allegro) ptr #-(or lispworks allegro) obj)))

(defmethod initialize-instance :after ((batch batch) &key)
  #+lispworks (let ((ptr (fli:allocate-foreign-object :pointer-type '(:pointer (:struct llama-batch)))))
		(llama-batch-init (n-tokens-max batch) (embd batch) (n-seq-max batch) :result-pointer ptr)
		(setf (ptr batch) ptr)
		(tg:finalize batch (lambda () (llama-batch-free ptr))))
  #+allegro (let ((ptr (llama-batch-init (n-tokens-max batch) (embd batch) (n-seq-max batch))))
	      (setf (ptr batch) ptr)
	      (tg:finalize batch (lambda () (llama-batch-free ptr))))
  #-(or lispworks allegro) (let ((obj (llama-batch-init (n-tokens-max batch) (embd batch) (n-seq-max batch))))
			     (setf (obj batch) obj)
			     (tg:finalize batch (lambda () (llama-batch-free obj))))
  batch)

(defclass tokens ()
  ((n :accessor n :initform 0)
   (size :initarg :size :initform (error "specify :size (buffer size)") :accessor size)
   (foreign-pointer :accessor ptr)))

(defmethod initialize-instance :after ((tok tokens) &key)
  (let ((ptr #+lispworks (fli:allocate-foreign-object :type :int :nelems (size tok))
	     #+allegro (ff:allocate-fobject (list :array :int (size tok)))
	     #-(or lispworks allegro) (cffi::foreign-alloc :int :count (size tok))))
    (setf (ptr tok) ptr)
    (tg:finalize tok (lambda () #+lispworks (fli:free-foreign-object ptr)
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
	(mapcar (lambda (id) (when (plusp id) (get-token context id))) ids)
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
				    (token change-first-to-bos :bos)))
    out))

(defmethod print-object ((obj tokens) stream)
  (print-unreadable-object (obj stream :type t)
    (let* ((limit 10)
	   (ids (list-tokens obj :limit limit))
	   (add (max 0 (- (n obj) limit))))
      (format stream "~A and ~D tokens more" ids add))))

(defmethod tokenize ((mdl mdl) tok text &key add-special parse-special)
  (tokenize (vocab mdl) tok text :add-special add-special :parse-special parse-special))
	    
(defmethod tokenize ((voc voc) (tok (eql t)) text &key add-special parse-special)
  ;; TODO: automatically grow the output vector as needed
  (tokenize voc (length text) text :add-special add-special :parse-special parse-special))

(defmethod tokenize ((voc voc) (tok fixnum) text &key add-special parse-special)
  (tokenize voc (make-instance 'tokens :size tok)
	    text :add-special add-special :parse-special parse-special))

(defmethod tokenize ((voc voc) (tok tokens) text &key add-special parse-special)
  (let ((res (llama-tokenize (ptr voc) text
			     #+lispworks (fli:with-foreign-string
					     (ptr elements bytes :external-format :utf-8 :null-terminated-p nil)
					   text bytes)
			     #+allegro (multiple-value-bind (ptr bytes)
					   (excl:string-to-native text :external-format :utf8 :null-terminate nil)
					 (declare (ignore ptr))
					 bytes)
			     #-(or lispworks allegro) (cffi:with-foreign-string
							  ((text len) text :null-terminated-p nil) len)
			     (ptr tok) (size tok) add-special parse-special)))
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
			 #+lispworks (fli:dereference ptr :type :float :index i)
			 #+allegro (ff:fslot-value-typed '(:array :float) :c ptr i)
			 #-(or lispworks allegro) (cffi:mem-aref ptr :float i)))
	  out))))

(defmethod get-embeddings-seq ((ctx ctx) seq)
  (let ((ptr (llama-get-embeddings-seq (ptr ctx) seq)))
    (if #+lispworks (fli:null-pointer-p ptr) #+allegro (zerop ptr) #-(or allegro lispworks) (cffi:null-pointer-p ptr)
	(values nil t)
	(let ((out (make-array (list (n-embd (model ctx))) :initial-element 0.0 :element-type 'float)))
	  (loop for i below (length out)
		do (setf (aref out i)
			 #+lispworks (fli:dereference ptr :type :float :index i)
			 #+allegro (ff:fslot-value-typed '(:array :float) :c ptr i)
			 #-(or lispworks allegro) (cffi:mem-aref ptr :float i)))
	  out))))

(defmethod get-embeddings-ith ((ctx ctx) i)
  (let ((ptr (llama-get-embeddings-ith (ptr ctx) i)))
    (if #+lispworks (fli:null-pointer-p ptr) #+allegro (zerop ptr) #-(or allegro lispworks) (cffi:null-pointer-p ptr)
	(values nil t)
	(let ((out (make-array (list (n-embd (model ctx))) :initial-element 0.0 :element-type 'float)))
	  (loop for i below (length out)
		do (setf (aref out i)
			 #+lispworks (fli:dereference ptr :type :float :index i)
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
			     #+lispworks (fli:dereference ptr :type :float :index i)
			     #+allegro (ff:fslot-value-typed '(:array :float) :c ptr i)
			     #-(or lispworks allegro) (cffi:mem-aref ptr :float i)))
	      out))))

(defmethod get-logits-ith ((ctx ctx) i)
  (let ((ptr (llama-get-logits-ith (ptr ctx) i)))
    (unless #+lispworks (fli:null-pointer-p ptr) #+allegro (zerop ptr) #-(or lispworks allegro) (cffi:null-pointer-p ptr)
	    (let ((out (make-array (list (n-vocab ctx))
				   :initial-element 0.0 :element-type 'single-float)))
	      (loop for i below (array-total-size out)
		    do (setf (row-major-aref out i)
			     #+lispworks (fli:dereference ptr :type :float :index i)
			     #+allegro (ff:fslot-value-typed '(:array :float) :c ptr i)
			     #-(or lispworks allegro) (cffi:mem-aref ptr :float i)))
	      out))))

(defmethod get-token ((ctx ctx) id)
  (get-token (model ctx) id))

(defmethod get-token ((mdl mdl) id)
  (get-token (vocab mdl) id))

(defmethod get-token ((voc voc) id)
  (substitute #\Space #\Ġ	      
	      #+lispworks (fli:convert-from-foreign-string (llama-vocab-get-text (ptr voc) id) :external-format :utf-8)
	      #-lispworks (llama-vocab-get-text (ptr voc) id)))

(defmethod get-vocab ((ctx ctx))
  (loop with model = (model ctx) for id below (n-vocab model) collect (get-token model id)))

(defmethod get-vocab ((ctx ctx))
  (loop with model = (model ctx) for id below (n-vocab model) collect (get-token model id)))

(defmethod token ((ctx ctx) token)
  (assert (member token '(:bos :eos :nl :cls :sep :prefix :middle :suffix :eot)))
  (token (model ctx) token))

(defmethod token ((voc voc) token)
  (ecase token
    (:bos (llama-vocab-bos (ptr voc)))
    (:eos (llama-vocab-eos (ptr voc)))
    (:nl (llama-vocab-nl (ptr voc)))
    (:sep (llama-vocab-sep (ptr voc)))
    (:prefix (llama-vocab-fim-pre (ptr voc)))
    (:middle (llama-vocab-fim-mid (ptr voc)))
    (:suffix (llama-vocab-fim-suf (ptr voc)))
    (:eot (llama-vocab-eot (ptr voc)))))

(defmethod token ((mdl mdl) token)
  (token (vocab mdl) token))

(defmethod token-is-eog ((voc voc) id)
  (llama-vocab-is-eog (ptr voc) id))

(defmethod token-is-eog ((mdl mdl) id)
  (token-is-eog (vocab mdl) id))

(defmethod token-is-control ((voc voc) id)
  (llama-vocab-is-control (ptr voc) id))

(defmethod token-is-control ((mdl mdl) id)
  (token-is-control (vocab mdl) id))

(defmethod print-timings ((ctx ctx))
  (llama-perf-context-print (ptr ctx)))

(defmethod reset-timings ((ctx ctx))
  (llama-perf-context-reset (ptr ctx)))

(defun system-info ()
  #+lispworks (fli:convert-from-foreign-string (llama-print-system-info))
  #-lispworks (llama-print-system-info))

(defmethod n-tokens ((batch batch))
  #+lispworks (fli:foreign-slot-value (ptr batch) 'n-tokens)
  #+allegro (ff:fslot-value (ptr batch) 'n-tokens)
  #-(or lispworks allegro) (elt (obj batch) 0))

(defmethod desc ((batch batch))
  #+lispworks (let ((ptr (ptr batch)))
		(loop for i below (fli:foreign-slot-value ptr 'n-tokens)
		      collect (list (fli:dereference (fli:foreign-slot-value ptr 'token) :index i)
				    (fli:dereference (fli:foreign-slot-value ptr 'pos) :index i)
				    (fli:dereference (fli:dereference (fli:foreign-slot-value ptr 'seq-id) :index i) :index 0)
				    (fli:dereference (fli:foreign-slot-value ptr 'logits) :index i))))
  #+allegro (let ((ptr (ptr batch)))
	      (loop for i below (ff:fslot-value ptr 'n-tokens)
                    collect (list (ff:fslot-value-typed '(:array :int) nil (ff:fslot-value ptr 'token) i)
		                  (ff:fslot-value-typed '(:array :int) nil (ff:fslot-value ptr 'pos) i)
				  #+NIL (ff:fslot-value (ff:fslot-value (ff:fslot-value ptr 'seq-id) i) 0)
		                  (ff:fslot-value-typed '(:array :int) nil (ff:fslot-value ptr 'logits) i))))
  #-(or lispworks allegro) (obj batch))

(defmethod add ((batch batch) token pos &optional logits (seq-ids 0))
  (if (numberp seq-ids) (setf seq-ids (list seq-ids)))
  #+lispworks (let* ((ptr (ptr batch))
		     (i (fli:foreign-slot-value ptr 'n-tokens)))
		(setf (fli:dereference (fli:foreign-slot-value ptr 'token) :index i) token)
		(setf (fli:dereference (fli:foreign-slot-value ptr 'pos) :index i) pos)
		(setf (fli:dereference (fli:foreign-slot-value ptr 'n-seq-id) :index i) (length seq-ids))
		(loop for seq-id in seq-ids for idx from 0
		      do (setf (fli:dereference (fli:dereference (fli:foreign-slot-value ptr 'seq-id) :index i) :index idx) seq-id))
		(setf (fli:dereference (fli:foreign-slot-value ptr 'logits) :index i) (if logits 1 0))
		(incf (fli:foreign-slot-value ptr 'n-tokens)))
  #+allegro (let* ((ptr (ptr batch))
		   (i (ff:fslot-value ptr 'n-tokens)))
	      (setf (ff:fslot-value-typed '(:array :int) nil (ff:fslot-value ptr 'token) i) token)
	      (setf (ff:fslot-value-typed '(:array :int) nil (ff:fslot-value ptr 'pos) i) pos)
  	      (setf (ff:fslot-value-typed '(:array :int) nil (ff:fslot-value ptr 'n-seq-id) i) (length seq-ids))
	      (loop for seq-id in seq-ids for idx from 0
		    do (setf (ff:fslot-value-typed '(:array (* :int)) nil (ff:fslot-value ptr 'seq-id) i idx) seq-id))
	      (setf (ff:fslot-value-typed '(:array :char) nil (ff:fslot-value ptr 'logits) i) (if logits 1 0))
	      (incf (ff:fslot-value ptr 'n-tokens)))
  #-(or lispworks allegro) (let* ((obj (obj batch))
				  (i (elt obj 0)))
			     (setf (cffi:mem-aref (elt obj 1) :int i) token)
			     (setf (cffi:mem-aref (elt obj 3) :int i) pos)
			     (setf (cffi:mem-aref (elt obj 4) :int i) (length seq-ids))
			     (loop for seq-id in seq-ids for idx from 0
				   do (setf (cffi:mem-aref (cffi:mem-aref (elt obj 5) '(:pointer :int) i) :int idx) seq-id))
			     (setf (cffi:mem-aref (elt obj 6) :uint8 i) (if logits 1 0))
			     (incf (elt obj 0)))
  batch)

(defmethod clear ((batch batch))
  (setf #+lispworks (fli:foreign-slot-value (ptr batch) 'n-tokens)
	#+allegro (ff:fslot-value (ptr batch) 'n-tokens)
	#-(or lispworks allegro) (elt (obj batch) 0)
	0))

(defmethod decode ((ctx ctx) (batch batch) &key (clear nil))
  (when clear (memory-clear ctx))
  (let ((res (llama-decode (ptr ctx) #+(or lispworks allegro) (ptr batch) #-(or lispworks allegro) (obj batch))))
    (case res
      (0 t)
      (1 (error "could not find a KV slot for the batch
(try reducing the size of the batch or increase the context)"))
      (otherwise (error "code error: ~A" res)))))

(defmethod add-bos-token ((voc voc))
  (llama-vocab-get-add-bos (ptr voc)))

(defmethod add-bos-token ((mdl mdl))
  (add-bos-token (vocab mdl)))

(defmethod add-bos-token ((ctx ctx))
  (add-bos-token (model ctx)))

(defmethod add-eos-token ((voc voc))
  (llama-vocab-get-add-eos (ptr voc)))

(defmethod add-eos-token ((mdl mdl))
  (add-eos-token (vocab mdl)))

(defmethod add-eos-token ((ctx ctx))
  (add-eos-token (model ctx)))

(defmethod should-add-bos-token ((mdl mdl))
  (let ((add-bos (add-bos-token mdl)))
    (if (= add-bos -1)
	(equal (vocab-type mdl) :spm)
	(plusp add-bos))))

(defmethod should-add-bos-token ((ctx ctx))
  (should-add-bos-token (model ctx)))

(defmethod memory-clear ((ctx ctx) &optional (data t))
  (llama-memory-clear (llama-get-memory (ptr ctx)) data))
