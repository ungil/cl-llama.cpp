(in-package :llama)

(cffi:defcstruct llama-context)

(cffi:defctype llama-progress-callback :pointer)

(cffi:defctype llama-token :int)

(cffi:defcstruct llama-token-data
  (id llama-token)
  (logit :float)
  (p :float))

(cffi:defcstruct llama-token-data-array
  (data (:pointer (:struct llama-token-data)))
  (size :unsigned-long)
  (sorted :bool))

(cffi:defcstruct (llama-context-params :class c-params)
  (n-ctx :int)
  (n-parts :int)
  (seed :int)
  (f16-kv :bool)
  (logits-all :bool)
  (vocab-only :bool)
  (use-mmap :bool)
  (use-mlock :bool)
  (embedding :bool)
  (progress-callback :pointer)
  (progress-callback-user-data :pointer))

(defmethod cffi:translate-from-foreign (ptr (type c-params))
  (cffi:with-foreign-slots ((n-ctx n-parts seed f16-kv logits-all vocab-only use-mmap use-mlock embedding
				   progress-callback progress-callback-user-data)
			    ptr (:struct llama-context-params))
    (make-instance 'context-params :n-ctx n-ctx :n-parts n-parts :seed seed :f16-kv f16-kv :logits-all logits-all
				   :vocab-only vocab-only :use-mmap use-mmap :use-mlock use-mlock :embedding embedding
				   :progress-callback progress-callback :progress-callback-user-data progress-callback-user-data)))

(defmethod cffi:translate-into-foreign-memory (value (type c-params) ptr)
  (cffi:with-foreign-slots ((n-ctx n-parts seed f16-kv logits-all vocab-only use-mmap use-mlock embedding
				   progress-callback progress-callback-user-data)
			    ptr (:struct llama-context-params))
    (setf n-ctx (slot-value value 'n-ctx)
	  n-parts (slot-value value 'n-parts)
	  seed (slot-value value 'seed)
	  f16-kv (slot-value value 'f16-kv)
	  logits-all (slot-value value 'logits-all)
	  vocab-only (slot-value value 'vocab-only)
	  use-mmap (slot-value value 'use-mmap)
	  use-mlock (slot-value value 'use-mlock)
	  embedding (slot-value value 'embedding)
	  progress-callback (slot-value value 'progress-callback)
	  progress-callback-user-data (slot-value value 'progress-callback-user-data))))

(defmethod cffi:free-translated-object (ptr (type c-params) param)
  (cffi:foreign-free ptr))

(cffi:defcfun llama-context-default-params (:struct llama-context-params))

(cffi:defcfun llama-mmap-supported :boolean)

(cffi:defcfun llama-mlock-supported :boolean)

(cffi:defcfun llama-init-from-file (:pointer (:struct llama-context))
  (path-model :string)
  (params (:struct llama-context-params)))

(cffi:defcfun llama-free :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-apply-lora-from-file :int
  (ctx (:pointer (:struct llama-context)))
  (path-lora :string)
  (path-base-model :string)
  (n-threads :int))

(cffi:defcfun llama-set-rng-seed :void
  (ctx (:pointer (:struct llama-context)))
  (seed :int))

(cffi:defcfun llama-eval :int
  (ctx (:pointer (:struct llama-context)))
  (tokens (:pointer llama-token))
  (n-tokens :int)
  (n-past :int)
  (n-threads :int))

(cffi:defcfun llama-tokenize :int
  (ctx (:pointer (:struct llama-context)))
  (text :string)
  (tokens (:pointer llama-token))
  (n-max-tokens :int)
  (add-bos :bool))

(cffi:defcfun llama-n-vocab :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-ctx :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-embd :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-get-logits (:pointer :float)
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-get-embeddings (:pointer :float)
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-token-to-str :string
  (ctx (:pointer (:struct llama-context)))
  (token llama-token))

(cffi:defcfun llama-token-bos llama-token)

(cffi:defcfun llama-token-eos llama-token)

(cffi:defcfun llama-token-nl llama-token)

(cffi:defcfun llama-sample-repetition-penalty :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (last_tokens (:pointer llama-token))
  (last-tokens-size :unsigned-long)
  (penalty :float))

(cffi:defcfun llama-sample-frequency-and-presence-penalties :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (last_tokens (:pointer llama-token))
  (last-tokens-size :unsigned-long)
  (alpha-frequency :float)
  (alpha-presence :float))

(cffi:defcfun llama-sample-softmax :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array))))

(cffi:defcfun llama-sample-top-k :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (k :int)
  (min-keep :unsigned-long))

(cffi:defcfun llama-sample-top-p :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (p :float)
  (min-keep :unsigned-long))

(cffi:defcfun llama-sample-tail-free :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (z :float)
  (min-keep :unsigned-long))

(cffi:defcfun llama-sample-typical :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (p :float)
  (min-keep :unsigned-long))

(cffi:defcfun llama-sample-temperature :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (temp :float))

(cffi:defcfun llama-sample-token-mirostat llama-token
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (tau :float)
  (eta :float)
  (m :int)
  (mu (:pointer :float)))

(cffi:defcfun llama-sample-token-mirostat-v2 llama-token
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (tau :float)
  (eta :float)
  (mu (:pointer :float)))

(cffi:defcfun llama-sample-token-greedy llama-token
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array))))

(cffi:defcfun llama-sample-token llama-token
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array))))

(cffi:defcfun llama-print-timings :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-reset-timings :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-print-system-info :string)

(cffi:defcfun llama-describe-candidates :void
    (candidates (:pointer (:struct llama-token-data-array))))
