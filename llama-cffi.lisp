

(cffi:defcstruct llama-model)

(cffi:defcstruct llama-context)

(cffi:defctype llama-pos :int)

(cffi:defctype llama-token :int)

(cffi:defctype llama-seq-id :int)

;; llama_log_level

;; llama_vocab_type

;; llama_token_type
(cffi:defcenum llama-token-type
  (:LLAMA-TOKEN-TYPE-UNDEFINED 0)
  (:LLAMA-TOKEN-TYPE-NORMAL 1)
  (:LLAMA-TOKEN-TYPE-UNKNOWN 2)
  (:LLAMA-TOKEN-TYPE-CONTROL 3)
  (:LLAMA-TOKEN-TYPE-USER-DEFINED 4)
  (:LLAMA-TOKEN-TYPE-UNUSED 5)
  (:LLAMA-TOKEN-TYPE-BYTE 6))

;; llama_ftype

;; llama_rope_scaling_type

;; llama_split_mode

(cffi:defcstruct llama-token-data
  (id llama-token)
  (logit :float)
  (p :float))

(cffi:defcstruct llama-token-data-array
  (data (:pointer (:struct llama-token-data)))
  (size :unsigned-long)
  (sorted :bool))

(cffi:defctype llama-progress-callback :pointer)

(cffi:defcstruct (llama-batch :class c-batch)
  (n-tokens :int32)

  (token (:pointer llama-token))
  (embd (:pointer :float))
  (pos (:pointer llama-pos))

  (n-seq-id (:pointer :int32))
  (seq-id (:pointer (:pointer llama-seq-id)))
  (logits (:pointer :int8))

  ;; helper - can be deprecated in the future
  (all-pos-0 llama-pos)
  (all-pos-1 llama-pos)
  (all-seq-id llama-seq-id))

;; llama_model_kv_override_type

(cffi:defcstruct llama-model-kv-override)

(cffi:defcstruct (llama-model-params :class c-model-params)
  (n-gpu-layers :int)
  (split-mode :int)
  (main-gpu :int)
  (tensor-split :pointer)
  (progress-callback :pointer)
  (progress-callback-user-data :pointer)
  (kv-overrides :pointer)
  (vocab-only :bool)
  (use-mmap :bool)
  (use-mlock :bool))

(cffi:defcstruct (llama-context-params :class c-context-params)
  (seed :int)
  (n-ctx :int)
  (n-batch :int)
  (n-ubatch :int)
  (n-seq-max :int)
  (n-threads :int)
  (n-threads-batch :int)

  (rope-scaling-type :int)
  (pooling-type :int)

  (rope-freq-base :float)
  (rope-freq-scale :float)
  (yarn-ext-factor :float)
  (yarn-attn-factor :float)
  (yarn-beta-fast :float)
  (yarn-beta-slow :float)
  (yarn-orig-ctx :int)
  (defrag-thold :float)

  (cb-eval :pointer)
  (cb-eval-user-data :pointer)

  (type-k :int)
  (type-v :int)

  (logits-all :bool)
  (embedding :bool)
  (offload-kqv :bool)

  (abort-callback :pointer)
  (abort-callback-data :pointer))

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

(defmethod cffi:translate-from-foreign (ptr (type c-model-params))
  (cffi:with-foreign-slots ((n-gpu-layers split-mode main-gpu tensor-split progress-callback progress-callback-user-data
					  vocab-only kv-overrides use-mmap use-mlock)
			    ptr (:struct llama-model-params))
    (make-instance 'model-params :n-gpu-layers n-gpu-layers :split-mode split-mode :main-gpu main-gpu :tensor-split tensor-split
				 :progress-callback progress-callback :progress-callback-user-data progress-callback-user-data
				 :vocab-only vocab-only :kv-overrides kv-overrides :use-mmap use-mmap :use-mlock use-mlock)))

(defmethod cffi:translate-into-foreign-memory (value (type c-model-params) ptr)
  (cffi:with-foreign-slots ((n-gpu-layers split-mode main-gpu tensor-split progress-callback progress-callback-user-data
					  vocab-only kv-overrides use-mmap use-mlock)
			    ptr (:struct llama-model-params))
    (setf n-gpu-layers (slot-value value 'n-gpu-layers)
	  split-mode (slot-value value 'split-mode)
	  main-gpu (slot-value value 'main-gpu)
	  tensor-split (slot-value value 'tensor-split)
	  progress-callback (slot-value value 'progress-callback)
	  progress-callback-user-data (slot-value value 'progress-callback-user-data)
	  vocab-only (slot-value value 'vocab-only)
	  kv-overrides (slot-value value 'kv-overrides)
	  use-mmap (slot-value value 'use-mmap)
	  use-mlock (slot-value value 'use-mlock))))

(defmethod cffi:free-translated-object (ptr (type c-model-params) param)
  (cffi:foreign-free ptr))

(defmethod cffi:translate-from-foreign (ptr (type c-context-params))
  (cffi:with-foreign-slots ((seed n-ctx n-batch n-ubatch n-seq-max n-threads n-threads-batch
				  rope-scaling-type pooling-type
				  rope-freq-base rope-freq-scale
				  yarn-ext-factor yarn-attn-factor yarn-beta-fast yarn-beta-slow yarn-orig-ctx
				  defrag-thold
				  cb-eval cb-eval-user-data
				  type-k type-v
				  logits-all embedding offload-kqv
				  abort-callback abort-callback-data)
			    ptr (:struct llama-context-params))
    (make-instance 'context-params :seed seed :n-ctx n-ctx :n-batch n-batch :n-ubatch n-ubatch :n-seq-max n-seq-max
				   :n-threads n-threads :n-threads-batch n-threads-batch
				   :rope-scaling-type rope-scaling-type :pooling-type pooling-type
				   :rope-freq-base rope-freq-base :rope-freq-scale rope-freq-scale
				   :yarn-ext-factor yarn-ext-factor :yarn-attn-factor yarn-attn-factor
				   :yarn-beta-fast yarn-beta-fast :yarn-beta-slow yarn-beta-slow :yarn-orig-ctx yarn-orig-ctx
				   :defrag-thold defrag-thold
				   :cb-eval cb-eval :cb-eval-user-data cb-eval-user-data
				   :type-k type-k :type-v type-v
				   :logits-all logits-all :embedding embedding :offload-kqv offload-kqv
				   :abort-callback abort-callback :abort-callback-data abort-callback-data)))

(defmethod cffi:translate-into-foreign-memory (value (type c-context-params) ptr)
  (cffi:with-foreign-slots ((seed n-ctx n-batch n-ubatch n-seq-max n-threads n-threads-batch
				  rope-scaling-type pooling-type
				  rope-freq-base rope-freq-scale
				  yarn-ext-factor yarn-attn-factor yarn-beta-fast
				  yarn-beta-slow yarn-orig-ctx defrag-thold
				  cb-eval cb-eval-user-data
				  type-k type-v
				  logits-all embedding offload-kqv
				  abort-callback abort-callback-data)
			    ptr (:struct llama-context-params))
    (setf seed (slot-value value 'seed)
	  n-ctx (slot-value value 'n-ctx)
	  n-batch (slot-value value 'n-batch)
	  n-ubatch (slot-value value 'n-ubatch)
	  n-seq-max (slot-value value 'n-seq-max)
	  n-threads (slot-value value 'n-threads)
	  n-threads-batch (slot-value value 'n-threads-batch)

	  rope-scaling-type (slot-value value 'rope-scaling-type)
	  pooling-type (slot-value value 'pooling-type)

	  rope-freq-base (slot-value value 'rope-freq-base)
	  rope-freq-scale (slot-value value 'rope-freq-scale)
	  yarn-ext-factor (slot-value value 'yarn-ext-factor)
	  yarn-attn-factor (slot-value value 'yarn-attn-factor)
	  yarn-beta-fast (slot-value value 'yarn-beta-fast)
	  yarn-beta-slow (slot-value value 'yarn-beta-slow)
	  yarn-orig-ctx (slot-value value 'yarn-orig-ctx)
	  defrag-thold (slot-value value 'defrag-thold)

	  cb-eval (slot-value value 'cb-eval)
	  cb-eval-user-data (slot-value value 'cb-eval-user-data)

	  type-k (slot-value value 'type-k)
	  type-v (slot-value value 'type-v)

	  logits-all (slot-value value 'logits-all)
	  embedding (slot-value value 'embedding)
	  offload-kqv (slot-value value 'offload-kqv)

	  abort-callback (slot-value value 'abort-callback)
	  abort-callback-data (slot-value value 'abort-callback-data))))

(defmethod cffi:free-translated-object (ptr (type c-context-params) param)
  (cffi:foreign-free ptr))

;; c-batch
(defmethod cffi:translate-from-foreign (ptr (type c-batch))
  (cffi:with-foreign-slots ((n-tokens token pos embd n-seq-id seq-id logits all-pos-0 all-pos-1
				      all-seq-id)
			    ptr (:struct llama-batch))
    (make-instance 'batch :n-tokens n-tokens :token token :pos pos :embd embd :n-seq-id n-seq-id
			  :seq-id seq-id :logits logits :all-pos-0 all-pos-0 :all-pos-1 all-pos-1
			  :all-seq-id all-seq-id)))

(defmethod cffi:translate-into-foreign-memory (value (type c-batch) ptr)
  (cffi:with-foreign-slots ((n-tokens token pos embd n-seq-id seq-id logits all-pos-0 all-pos-1
				      all-seq-id)
			    ptr (:struct llama-batch))
    (setf n-tokens (slot-value value 'n-tokens)
	  token (slot-value value 'token)
	  pos (slot-value value 'pos)
	  embd (slot-value value 'embd)
	  n-seq-id (slot-value value 'n-seq-id)
	  seq-id (slot-value value 'seq-id)
	  logits (slot-value value 'logits)
	  all-pos-0 (slot-value value 'all-pos-0)
	  all-pos-1 (slot-value value 'all-pos-1)
	  all-seq-id (slot-value value 'all-seq-id))))

(defmethod cffi:free-translated-object (ptr (type c-batch) param)
  (cffi:foreign-free ptr))

(cffi:defcfun llama-model-default-params (:struct llama-model-params))

(cffi:defcfun llama-context-default-params (:struct llama-context-params))

;; llama_model_quantize_default_params

(cffi:defcfun llama-backend-init :void)

(cffi:defcenum ggml-numa-strategy
  (:GGML-NUMA-STRATEGY-DISABLED    0)
  (:GGML-NUMA-STRATEGY-DISTRIBUTE  1)
  (:GGML-NUMA-STRATEGY-ISOLATE     2)
  (:GGML-NUMA-STRATEGY-NUMACTL     3)
  (:GGML-NUMA-STRATEGY-MIRROR      4)
  (:GGML-NUMA-STRATEGY-COUNT       5))

(cffi:defcfun llama-numa-init :void
  (numa ggml-numa-strategy))

(cffi:defcfun llama-backend-free :void)

(cffi:defcfun llama-load-model-from-file (:pointer (:struct llama-model))
  (path-model :string)
  (params (:struct llama-model-params)))

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

(cffi:defcfun llama-supports-mmap :boolean)

(cffi:defcfun llama-supports-mlock :boolean)

(cffi:defcfun llama-supports-gpu-offload :boolean)

;; ;; deprecated
;; (cffi:defcfun llama-mmap-supported :boolean)

;; ;; deprecated
;; (cffi:defcfun llama-mlock-supported :boolean)

(cffi:defcfun llama-get-model (:pointer (:struct llama-model))
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-ctx :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-batch :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-vocab-type :int
  (ctx (:pointer (:struct llama-model))))

(cffi:defcfun llama-n-vocab :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-ctx-train :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-n-embd :int
  (model (:pointer (:struct llama-model))))

;; llama_rope_freq_scale_train

;; llama_model_meta_val_str

;; llama_model_meta_count

;; llama_model_meta_key_by_index

;; llama_model_meta_val_str_by_index

(cffi:defcfun llama-model-desc :int
  (model (:pointer (:struct llama-model)))
  (buf :string)
  (buf-size :unsigned-long))

(cffi:defcfun llama-model-size :unsigned-long
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-params :unsigned-long
  (model (:pointer (:struct llama-model))))

;; llama_get_model_tensor

;; llama_model_quantize

;; ;; DEPRECATED
;; (cffi:defcfun llama-apply-lora-from-file :int
;;   (ctx (:pointer (:struct llama-context)))
;;   (path-lora :string)
;;   (scale :float)
;;   (path-base-model :string)
;;   (n-threads :int))

(cffi:defcfun llama-model-apply-lora-from-file :int
  (model (:pointer (:struct llama-model)))
  (path-lora :string)
  (path-base-model :string)
  (n-threads :int))

;; llama_kv_cache_view_cell
;; llama_kv_cache_view
;; llama_kv_cache_view_init
;; llama_kv_cache_view_free
;; llama_kv_cache_view_update
;; llama_get_kv_cache_token_count
;; llama_get_kv_cache_used_cells
;; llama_kv_cache_clear
;; llama_kv_cache_seq_rm
;; llama_kv_cache_seq_cp
;; llama_kv_cache_seq_keep
;; llama_kv_cache_seq_shift
;; llama_kv_cache_seq_div

(cffi:defcfun llama-get-state-size :unsigned-long
  (ctx (:pointer (:struct llama-context))))

;; llama_copy_state_data

;; llama_set_state_data

;; llama_load_session_file

;; llama_save_session_file

;; DEPRECATED - use llama_decode
(cffi:defcfun llama-eval :int
  (ctx (:pointer (:struct llama-context)))
  (tokens (:pointer llama-token))
  (n-tokens :int)
  (n-past :int)
  (n-threads :int))

;; DEPRECATED - use llama_decode
(cffi:defcfun llama-eval-embd :int
  (ctx (:pointer (:struct llama-context)))
  (embd (:pointer :float))
  (n-tokens :int)
  (n-past :int)
  (n-threads :int))

(cffi:defcfun llama-batch-get-one (:struct llama-batch)
  (tokens (:pointer llama-token))
  (n-tokens :int32)
  (pos-0 llama-pos)
  (seq-id llama-seq-id))

(cffi:defcfun llama-batch-init (:struct llama-batch)
  (n-tokens :int32)
  (embd :int32)
  (n-seq-max :int32))

(cffi:defcfun llama-batch-free :void
  (batch (:struct llama-batch)))

(cffi:defcfun llama-decode :int32
  (ctx (:pointer (:struct llama-context)))
  (batch (:struct llama-batch)))

;; llama_set_n_threads

(cffi:defcfun llama-get-logits (:pointer :float)
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-get-logits-ith (:pointer :float)
  (ctx (:pointer (:struct llama-context)))
  (i :int32))

(cffi:defcfun llama-get-embeddings (:pointer :float)
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-get-embeddings-ith (:pointer :float)
  (ctx (:pointer (:struct llama-context)))
  (i :int))

(cffi:defcfun llama-token-get-text :string
  (model (:pointer (:struct llama-model)))
  (token llama-token))

(cffi:defcfun llama-token-get-score :float
  (model (:pointer (:struct llama-model)))
  (token llama-token))

(cffi:defcfun llama-token-get-type :int
  (model (:pointer (:struct llama-model)))
  (token llama-token))

(cffi:defcfun llama-token-is-eog :bool
  (model (:pointer (:struct llama-model)))
  (token llama-token))

(cffi:defcfun llama-token-bos llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-token-eos llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-token-nl llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-add-bos-token :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-add-eos-token :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-token-prefix llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-token-middle llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-token-suffix llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-tokenize :int
  (model (:pointer (:struct llama-model)))
  (text :string)
  (text-len :int)
  (tokens (:pointer llama-token))
  (n-max-tokens :int)
  (add-bos :bool)
  (special :bool))

(cffi:defcfun llama-token-to-piece :int
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

(cffi:defcfun llama-set-rng-seed :void
  (ctx (:pointer (:struct llama-context)))
  (seed :int))

(cffi:defcfun llama-sample-repetition-penalties :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (last_tokens (:pointer llama-token))
  (penalty-last-n :unsigned-long)
  (penalty-repeat :float)
  (penalty-freq :float)
  (penalty-present :float))

(cffi:defcfun llama-sample-frequency-and-presence-penalties :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (last_tokens (:pointer llama-token))
  (last-tokens-size :unsigned-long)
  (alpha-frequency :float)
  (alpha-presence :float))

(cffi:defcfun llama-sample-sample-apply-guidance :void
  (ctx (:pointer (:struct llama-context)))
  (logits (:pointer :float))
  (logits-guidance-ctx (:pointer :float))
  (scale :float))

;; deprecated - use llama-sample-apply-guidance
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

(cffi:defcfun llama-sample-min-p :void
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

(cffi:defcfun llama-sample-entropy :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (min-temp :float)
  (max-temp :float)
  (exponent-val :float))

(cffi:defcfun llama-sample-temp :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (temp :float))

(cffi:defcfun llama-sample-temperature :void
  (ctx (:pointer (:struct llama-context)))
  (candidates (:pointer (:struct llama-token-data-array)))
  (temp :float))

;; ;; DEPRECATED
;; (cffi:defcfun llama-sample-grammar :void
;;   (ctx (:pointer (:struct llama-context)))
;;   (candidates (:pointer (:struct llama-token-data-array)))
;;   (grammar (:pointer (:struct llama-grammar))))

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
(cffi:defcfun llama-log-set :void
  (log-callback :pointer)
  (user-data :pointer))

;; llama_dump_timing_info_yaml
