(in-package :llama)

(cffi:defcstruct llama-model)

(cffi:defcstruct llama-context)

(cffi:defctype llama-token :int)

;; llama_log_level

;; llama_vocab_type

;; llama_token_type

;; llama_ftype

(cffi:defcstruct llama-token-data
  (id llama-token)
  (logit :float)
  (p :float))

(cffi:defcstruct llama-token-data-array
  (data (:pointer (:struct llama-token-data)))
  (size :unsigned-long)
  (sorted :bool))

(cffi:defctype llama-progress-callback :pointer)

(cffi:defcstruct (llama-context-params :class c-params)
  (seed :int)
  (n-ctx :int)
  (n-batch :int)
  (n-gpu-layers :int)
  (main-gpu :int)
  (tensor-split :pointer)
  (rope-freq-base :float)
  (rope-freq-scale :float)
  (progress-callback :pointer)
  (progress-callback-user-data :pointer)
  (low-vram :bool)
  (mul-mat :bool)
  (f16-kv :bool)
  (logits-all :bool)
  (vocab-only :bool)
  (use-mmap :bool)
  (use-mlock :bool)
  (embedding :bool))

;; llama_log_callback

;; llama_model_quantize_params

(cffi:defcstruct llama-grammar)

;; llama_gretype

(cffi:defcstruct llama-grammar-element
  (type :int)
  (value :unsigned-long))

(cffi:defcstruct (llama-timings :class c-timings)
  (t-start-ms :double)
  (t-end-ms :double)
  (t-load-ms :double)
  (t-sample-ms :double)
  (t-p-eval-ms :double)
  (t-eval-ms :double)
  (n-sample :int)
  (n-p-eval :int)
  (n-eval :int))

(defmethod cffi:translate-from-foreign (ptr (type c-params))
  (cffi:with-foreign-slots ((seed n-ctx n-batch n-gpu-layers main-gpu tensor-split
				  rope-freq-base rope-freq-scale
				  progress-callback progress-callback-user-data
				  low-vram mul-mat f16-kv logits-all vocab-only use-mmap use-mlock embedding)
			    ptr (:struct llama-context-params))
    (make-instance 'context-params :seed seed :n-ctx n-ctx :n-batch n-batch
				   :n-gpu-layers n-gpu-layers :main-gpu main-gpu :tensor-split tensor-split
				   :rope-freq-base rope-freq-base :rope-freq-scale rope-freq-scale
				   :progress-callback progress-callback :progress-callback-user-data progress-callback-user-data
				   :low-vram low-vram :mul-mat mul-mat :f16-kv f16-kv :logits-all logits-all :vocab-only vocab-only
				   :use-mmap use-mmap :use-mlock use-mlock :embedding embedding)))

(defmethod cffi:translate-into-foreign-memory (value (type c-params) ptr)
  (cffi:with-foreign-slots ((seed n-ctx n-batch n-gpu-layers main-gpu tensor-split
				  rope-freq-base rope-freq-scale
				  progress-callback progress-callback-user-data
				  low-vram mul-mat f16-kv logits-all vocab-only use-mmap use-mlock embedding)
			    ptr (:struct llama-context-params))
    (setf seed (slot-value value 'seed)
	  n-ctx (slot-value value 'n-ctx)
	  n-batch (slot-value value 'n-batch)
	  n-gpu-layers (slot-value value 'n-gpu-layers)
	  main-gpu (slot-value value 'main-gpu)
	  tensor-split (slot-value value 'tensor-split)
	  rope-freq-base (slot-value value 'rope-freq-base)
	  rope-freq-scale (slot-value value 'rope-freq-scale)
	  progress-callback (slot-value value 'progress-callback)
	  progress-callback-user-data (slot-value value 'progress-callback-user-data)
	  low-vram (slot-value value 'low-vram)
	  mul-mat (slot-value value 'mul-mat)
	  f16-kv (slot-value value 'f16-kv)
	  logits-all (slot-value value 'logits-all)
	  vocab-only (slot-value value 'vocab-only)
	  use-mmap (slot-value value 'use-mmap)
	  use-mlock (slot-value value 'use-mlock)
	  embedding (slot-value value 'embedding))))

(defmethod cffi:free-translated-object (ptr (type c-params) param)
  (cffi:foreign-free ptr))

(cffi:defcfun llama-context-default-params (:struct llama-context-params))

;; llama_model_quantize_default_params

(cffi:defcfun llama-backend-init :void
  (numa :boolean))

(cffi:defcfun llama-backend-free :void)

(cffi:defcfun llama-load-model-from-file (:pointer (:struct llama-model))
  (path-model :string)
  (params (:struct llama-context-params)))

(cffi:defcfun llama-free-model :void
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-new-context-with-model (:pointer (:struct llama-context))
  (model (:pointer (:struct llama-model)))
  (params (:struct llama-context-params)))

;; ;; DEPRECATED
;; (cffi:defcfun llama-init-from-file (:pointer (:struct llama-context))
;;   (path-model :string)
;;   (params (:struct llama-context-params)))

(cffi:defcfun llama-free :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-time-us :int64)

(cffi:defcfun llama-max-devices :int)

(cffi:defcfun llama-mmap-supported :boolean)

(cffi:defcfun llama-mlock-supported :boolean)

(cffi:defcfun llama-n-vocab :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-ctx :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-ctx-train :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-embd :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-vocab-type :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-model-n-vocab :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-ctx :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-ctx-train :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-embd :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-desc :int
  (model (:pointer (:struct llama-model)))
  (buf :string)
  (buf-size :unsigned-long))

(cffi:defcfun llama-model-size :unsigned-long
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-params :unsigned-long
  (model (:pointer (:struct llama-model))))

;; llama_model_quantize

;; ;; DEPRECATED
;; (cffi:defcfun llama-apply-lora-from-file :int
;;   (ctx (:pointer (:struct llama-context)))
;;   (path-lora :string)
;;   (path-base-model :string)
;;   (n-threads :int))

(cffi:defcfun llama-model-apply-lora-from-file :int
  (model (:pointer (:struct llama-model)))
  (path-lora :string)
  (path-base-model :string)
  (n-threads :int))

(cffi:defcfun llama-get-kv-cache-token-count :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-set-rng-seed :void
  (ctx (:pointer (:struct llama-context)))
  (seed :int))

(cffi:defcfun llama-get-state-size :unsigned-long
  (ctx (:pointer (:struct llama-context))))

;; llama_copy_state_data

;; llama_set_state_data

;; llama_load_session_file

;; llama_save_session_file

(cffi:defcfun llama-eval :int
  (ctx (:pointer (:struct llama-context)))
  (tokens (:pointer llama-token))
  (n-tokens :int)
  (n-past :int)
  (n-threads :int))

(cffi:defcfun llama-eval-embd :int
  (ctx (:pointer (:struct llama-context)))
  (embd (:pointer :float))
  (n-tokens :int)
  (n-past :int)
  (n-threads :int))

;; llama_eval_export

(cffi:defcfun llama-get-logits (:pointer :float)
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-get-embeddings (:pointer :float)
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-token-get-text :string
  (ctx (:pointer (:struct llama-context)))
  (token llama-token))

(cffi:defcfun llama-token-get-score :float
  (ctx (:pointer (:struct llama-context)))
  (token llama-token))

(cffi:defcfun llama-token-get-type :int
  (ctx (:pointer (:struct llama-context)))
  (token llama-token))

(cffi:defcfun llama-token-bos llama-token
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-token-eos llama-token
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-token-nl llama-token
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-tokenize :int
  (ctx (:pointer (:struct llama-context)))
  (text :string)
  (text-len :int)  
  (tokens (:pointer llama-token))
  (n-max-tokens :int)
  (add-bos :bool))

(cffi:defcfun llama-tokenize-with-model :int
  (model (:pointer (:struct llama-model)))
  (text :string)
  (text-len :int)  
  (tokens (:pointer llama-token))
  (n-max-tokens :int)
  (add-bos :bool))

(cffi:defcfun llama-token-to-str :int
  (ctx (:pointer (:struct llama-context)))
  (token llama-token)
  (buf :string)
  (length :int))

(cffi:defcfun llama-token-to-str-with-model :int
  (model (:pointer (:struct llama-model)))
  (token llama-token)
  (buf :string)
  (length :int))

(cffi:defcfun llama-grammar-init (:pointer (:struct llama-grammar))
  (model (:pointer (:struct llama-model)))
  (n-rules :unsigned-long)
  (start-rule-index :unsigned-long))

(cffi:defcfun llama-grammar-free :void
  (grammar (:pointer (:struct llama-grammar))))

(cffi:defcfun llama-grammar-copy (:pointer (:struct llama-grammar))
  (grammar (:pointer (:struct llama-grammar))))

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

(cffi:defcfun llama-sample-classifier-free-guidance :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (guidance-ctx (:pointer (:struct llama-context)))
  (scale :float))

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

(cffi:defcfun llama-sample-grammar :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (grammar (:pointer (:struct llama-grammar))))

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

(cffi:defcfun llama-grammar-accept-token :void
  (ctx (:pointer (:struct llama-context)))
  (grammar (:pointer (:struct llama-grammar)))
  (token llama-token))

;; llama_beam_view

;; llama_beam_state

;; llama_beam_search_callback_fn_t

;; llama_beam_search

(cffi:defcfun llama-get-timings (:struct llama-timings)
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-print-timings :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-reset-timings :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-print-system-info :string)

;; llama_log_set

;; llama_dump_timing_info_yaml
