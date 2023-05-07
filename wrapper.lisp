(in-package :llama)

(unless (probe-file (asdf:system-relative-pathname "llama" "llama.cpp/libllama.so"))
  (error "to build the library execute 'make libllama.so' in the llama.cpp subdirectory"))

#-(or lispworks allegro)
(let ((library (asdf:system-relative-pathname "llama" "llama.cpp/libllama.so")))
  (cffi:load-foreign-library library)
  (format t "~%~%   Foreign library ~A loaded~%~%~%" library))

#-(or lispworks allegro) (load (asdf:system-relative-pathname "llama" "llama-cffi.lisp"))

#+lispworks
(let ((library (asdf:system-relative-pathname "llama" "llama.cpp/libllama.so")))
  (fli:register-module "libllama.so" :file-name library)
  (format cl:t "~%~%   Foreign library ~A loaded~%~%~%" library))

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
(let ((library (asdf:system-relative-pathname "llama" "llama.cpp/libllama.so")))
  (load library :foreign t) ;;"libllama.so" :file-name library)
  (format cl:t "~%~%   Foreign library ~A loaded~%~%~%" library))

#+allegro (load (asdf:system-relative-pathname "llama" "llama-ff.lisp"))

(defclass context-params ()
  ((n-ctx :initarg :n-ctx)
   (n-parts :initarg :n-parts)
   (seed :initarg :seed)
   (f16-kv :initarg :f16-kv)
   (logits-all :initarg :logits-all)
   (vocab-only :initarg :vocab-only)
   (use-mmap :initarg :use-mmap)
   (use-mlock :initarg :use-mlock)
   (embedding :initarg :embedding)
   (progress-callback :initarg :progress-callback)
   (progress-callback-user-data :initarg :progress-callback-user-data)
   #+(or lispworks allegro) foreign-struct))

(defun context-default-params ()
  #+lispworks (let ((ptr (fli:allocate-foreign-object :pointer-type '(:pointer (:struct llama-context-params)))))
		(llama-context-default-params :result-pointer ptr))
  #-lispworks (llama-context-default-params))

(defun mmap-supported ()
  (llama-mmap-supported))

(defun mlock-supported ()
  (llama-mlock-supported))

(defun init-from-file (model params)
  (llama-init-from-file (namestring model) params))

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

(defclass context ()
  ((model :initarg :model :accessor model)
   (params :initarg :params :accessor params
	   :initform #+lispworks (make-instance 'context-params)
		     #-lispworks (context-default-params))
   foreign-pointer))

(defmethod initialize-instance :after ((ctx context) &key)
  (let ((ptr (init-from-file (model ctx) #+lispworks (slot-value (params ctx) 'foreign-struct)
					 #-lispworks (params ctx))))
    (setf (slot-value ctx 'foreign-pointer) ptr)
    (tg:finalize ctx (lambda () (llama-free ptr))))
  ctx)

(defmethod print-object ((obj context) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A" (model obj) (params obj))))

(defmethod evaluate ((ctx context) tokens n-past &optional (threads 4))
  (llama-eval (slot-value ctx 'foreign-pointer) (ptr tokens) (n tokens) n-past threads))

(defclass tokens ()
  ((n :accessor n :initform 0)
   (size :initarg :size :initform (error "specify :size (buffer size)") :accessor size)
   (foreign-pointer :accessor ptr)))

(defmethod initialize-instance :after ((tok tokens) &key)
  (let ((ptr #+lispworks (fli:allocate-foreign-object :type :int :nelems (size tok))
	     #+allegro (ff:allocate-fobject (list :array :int (size tok)))
	     #-(or lispworks allegro) (cffi::foreign-alloc :int :count (size tok))))
    (setf (slot-value tok 'foreign-pointer) ptr)
    (tg:finalize tok (lambda () #+lispworks (fli:free ptr)
			     ;;#+allegro (ff:free-fobject ptr)
			     #-(or lispworks allegro) (cffi:foreign-free ptr))))
  tok)

(defmethod list-tokens ((tok tokens) &key (limit 10) context)
  (let ((ids (loop for i below (n tok)
		   repeat (or limit (n tok))
		   collect
		   #+lispworks (fli:dereference (slot-value tok 'foreign-pointer) :index i)
		   #+allegro (ff:fslot-value (ptr tok) i)
		   #-(or lispworks allegro) (cffi:mem-aref (slot-value tok 'foreign-pointer) :int i))))
    (if context
	(mapcar (lambda (id) (get-token context id)) ids)
	ids)))

(defmethod subset ((tok tokens) start length)
  (let ((out (make-instance 'tokens :size length)))
    (setf (n out) length)
    (loop for tgt below length
	  for src from start
	  do #+lispworks (setf (fli:dereference (slot-value out 'foreign-pointer) :index tgt)
			       (fli:dereference (slot-value tok 'foreign-pointer) :index src))
	  #+allegro (setf (ff:fslot-value (ptr out) tgt)
			  (ff:fslot-value (ptr tok) src))
	  #-(or lispworks allegro) (setf (cffi:mem-aref (slot-value out 'foreign-pointer) :int tgt)
					 (cffi:mem-aref (slot-value tok 'foreign-pointer) :int src)))
    out))

(defmethod print-object ((obj tokens) stream)
  (print-unreadable-object (obj stream :type t)
    (let* ((limit 10)
	   (ids (list-tokens obj :limit limit))
	   (add (max 0 (- (n obj) limit))))
      (format stream "~A and ~D tokens more" ids add))))

(defmethod tokenize ((ctx context) (tok fixnum) text &key add-beginning-of-sentence)
  (tokenize ctx (make-instance 'tokens :size tok)
	    text :add-beginning-of-sentence add-beginning-of-sentence))

(defmethod tokenize ((ctx context) (tok tokens) text &key add-beginning-of-sentence)
  (let ((res (llama-tokenize (slot-value ctx 'foreign-pointer) text
			     (ptr tok) (size tok) add-beginning-of-sentence)))
    (when (minusp res) (error "returned ~D, more than buffer size ~D" (- res) (size tok)))
    (setf (n tok) res)
    tok))

(defmethod n-vocab ((ctx context))
  (llama-n-vocab (slot-value ctx 'foreign-pointer)))

(defmethod n-ctx ((ctx context))
  (llama-n-ctx (slot-value ctx 'foreign-pointer)))

(defmethod n-embd ((ctx context))
  (llama-n-embd (slot-value ctx 'foreign-pointer)))

(defmethod get-embeddings ((ctx context))
  (let ((ptr (llama-get-embeddings (slot-value ctx 'foreign-pointer))))
    (if #+lispworks (fli:null-pointer-p ptr) #+allegro (zerop ptr) #-(or allegro lispworks) (cffi:null-pointer-p ptr)	
	(values nil t)
	(let ((out (make-array (list (n-embd ctx)) :initial-element 0.0 :element-type 'float)))
	  (loop for i below (length out)
		do (setf (aref out i)
			 #+lispworks (fli:dereference ptr :index i)
			 #+allegro (ff:fslot-value-typed '(:array :float) :c ptr i)
			 #-(or lispworks allegro) (cffi:mem-aref ptr :float i)))
	  out))))

(defmethod get-logits ((ctx context) &optional (n 1))
  (let ((ptr (llama-get-logits (slot-value ctx 'foreign-pointer))))
    (unless #+lispworks (fli:null-pointer-p ptr) #+allegro (zerop ptr) #-(or lispworks allegro) (cffi:null-pointer-p ptr)
	    (let ((out (make-array (if (= n 1) (list (n-vocab ctx)) (list n (n-vocab ctx)))
				   :initial-element 0.0 :element-type 'single-float)))
	      (loop for i below (array-total-size out)
		    do (setf (row-major-aref out i)
			     #+lispworks (fli:dereference ptr :index i)
			     #+allegro (ff:fslot-value-typed '(:array :float) :c ptr i)
			     #-(or lispworks allegro) (cffi:mem-aref ptr :float i)))
	      out))))

(defmethod get-token ((ctx context) id)
  (ignore-errors 
   #+lispworks (fli:convert-from-foreign-string (llama-token-to-str (slot-value ctx 'foreign-pointer) id)
						:external-format :utf-8 )
   #-lispworks (llama-token-to-str (slot-value ctx 'foreign-pointer) id)))

(defmethod get-vocab ((ctx context))
  (loop for id below (n-vocab ctx) collect (get-token ctx id)))

;; ;; direct access:
;; llama_token_bos
;; llama_token_eos

(defmethod sample-repetition-penalty ((ctx context) candidates last-tokens penalty)
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
	   (llama-sample-repetition-penalty (slot-value ctx 'foreign-pointer) candidates ptr (length last-tokens)
					    (coerce penalty 'single-float)))
      #+lispworks (fli:free ptr)
      ;;#+allegro (ff:free-fobject ptr)
      #-(or lispworks allegro) (cffi:foreign-free ptr))))

(defmethod sample-frequency-and-presence-penalties ((ctx context) candidates last-tokens alpha-frequency alpha-presence)
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
	   (llama-sample-frequency-and-presence-penalties (slot-value ctx 'foreign-pointer) candidates ptr (length last-tokens)
							  (coerce alpha-frequency 'single-float) (coerce alpha-presence 'single-float)))
      #+lispworks (fli:free ptr)
      ;;#+allegro (ff:free-fobject ptr)
      #-(or lispworks allegro) (cffi:foreign-free ptr))))
  
(defmethod sample-softmax ((ctx context) candidates)
  (llama-sample-softmax (slot-value ctx 'foreign-pointer) candidates))

(defmethod sample-top-k ((ctx context) candidates top-k &optional (min-keep 1))
  (llama-sample-top-k (slot-value ctx 'foreign-pointer) candidates top-k min-keep))

(defmethod sample-tail-free ((ctx context) candidates tfs-z &optional (min-keep 1))
  (llama-sample-tail-free (slot-value ctx 'foreign-pointer) candidates tfs-z min-keep))

(defmethod sample-typical ((ctx context) candidates typical-p &optional (min-keep 1))
  (llama-sample-typical (slot-value ctx 'foreign-pointer) candidates (coerce typical-p 'single-float) min-keep))

(defmethod sample-top-p ((ctx context) candidates top-p &optional (min-keep 1))
  (llama-sample-top-p (slot-value ctx 'foreign-pointer) candidates (coerce top-p 'single-float) min-keep))

(defmethod sample-temperature ((ctx context) candidates temp)
  (llama-sample-temperature (slot-value ctx 'foreign-pointer) candidates (coerce temp 'single-float)))

(defmethod sample-token ((ctx context) candidates)
  (llama-sample-token (slot-value ctx 'foreign-pointer) candidates))

(defmethod sample-token-greedy ((ctx context) candidates)
  (llama-sample-token-greedy (slot-value ctx 'foreign-pointer) candidates))

(defmethod print-timings ((ctx context))
  (llama-print-timings(slot-value ctx 'foreign-pointer)))

(defmethod reset-timings ((ctx context))
  (llama-reset-timings (slot-value ctx 'foreign-pointer)))

(defun system-info ()
  #+lispworks (fli:convert-from-foreign-string (llama-print-system-info))
  #-lispworks (llama-print-system-info))
