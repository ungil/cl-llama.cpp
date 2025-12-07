(in-package :llama)

(cffi:defcstruct llama-vocab)

(cffi:defcstruct llama-model)

(cffi:defcstruct llama-context)

(cffi:defcstruct llama-sampler)

(cffi:defcstruct llama-memory)

(cffi:defctype llama-pos :int)

(cffi:defctype llama-token :int)

(cffi:defctype llama-seq-id :int)

;; llama_vocab_type
;; llama_vocab_pre_type
;; llama_rope_type
;; llama_token_type
;; llama_token_attr
;; llama_ftype
;; llama_rope_scaling_type
;; llama_pooling_type
;; llama_attention_type
;; llama_flash_attn_type
;; llama_flash_attn_type_name
;; llama_split_mode

(cffi:defcstruct llama-token-data
  (id llama-token)
  (logit :float)
  (p :float))

(cffi:defcstruct llama-token-data-array
  (data (:pointer (:struct llama-token-data)))
  (size :unsigned-long)
  (selected :int64)
  (sorted :bool))

(cffi:defctype llama-progress-callback :pointer)

(cffi:defcstruct (llama-batch :class c-batch)
  (n-tokens :int)
  (token (:pointer llama-token))
  (embd (:pointer :float))
  (pos (:pointer llama-pos))
  (n-seq-id (:pointer :int))
  (seq-id (:pointer (:pointer llama-seq-id)))
  (logits (:pointer :int8)))

;; llama_model_kv_override_type
;; (cffi:defcstruct llama-model-kv-override)

;; llama_model_tensor_buft_override

(cffi:defcstruct (llama-model-params :class c-model-params)
  (devices :pointer)
  (tensor-buft-overrides :pointer)
  (n-gpu-layers :int)
  (split-mode :int)
  (main-gpu :int)
  (tensor-split (:pointer :float))
  (progress-callback :pointer)
  (progress-callback-user-data :pointer)
  (kv-overrides :pointer)
  (vocab-only :bool)
  (use-mmap :bool)
  (use-mlock :bool)
  (check-tensors :bool)
  (use-extra-bufts :bool)
  (no-host :bool))

(cffi:defcstruct (llama-context-params :class c-context-params)
  (n-ctx :unsigned-int)
  (n-batch :unsigned-int)
  (n-ubatch :unsigned-int)
  (n-seq-max :unsigned-int)
  (n-threads :int)
  (n-threads-batch :int)
  (rope-scaling-type :int)
  (pooling-type :int)
  (attention-type :int)
  (flash-attn-type :int)
  (rope-freq-base :float)
  (rope-freq-scale :float)
  (yarn-ext-factor :float)
  (yarn-attn-factor :float)
  (yarn-beta-fast :float)
  (yarn-beta-slow :float)
  (yarn-orig-ctx :unsigned-int)
  (defrag-thold :float)
  (cb-eval :pointer)
  (cb-eval-user-data :pointer)
  (type-k :int)
  (type-v :int)
  (abort-callback :pointer)
  (abort-callback-data :pointer)
  (embeddings :bool)
  (offload-kqv :bool)
  (no-perf :bool)
  (op-offload :bool)
  (swa-full :bool)
  (kv-unified :bool))

(cffi:defcstruct (llama-model-quantize-params :class c-model-quantize-params)
  (nthread :int)
  (ftype :int)
  (output-tensor-type :int)
  (token-embedding-type :int)
  (allow-requantize :bool)
  (quantize-output-tensor :bool)
  (only-copy :bool)
  (pure :bool)
  (keep-split :bool)
  (imatrix :pointer)
  (kv-overrides :pointer)
  (tensor-types :pointer)
  (prune-layers :pointer))

(cffi:defcstruct (llama-logit-bias)
  (token llama-token)
  (bias :float))

(cffi:defcstruct (llama-sampler-chain-params)
  (no-perf :bool))

(cffi:defcstruct (llama-chat-message)
  (role :string)
  (content :string))

(defmethod cffi:translate-from-foreign (ptr (type c-model-params))
  (cffi:with-foreign-slots ((devices tensor-buft-overrides
			     n-gpu-layers split-mode main-gpu tensor-split
			     progress-callback progress-callback-user-data
			     kv-overrides vocab-only use-mmap use-mlock check-tensors)
			    ptr (:struct llama-model-params))
    (make-instance 'model-params :devices devices :tensor-buft-overrides tensor-buft-overrides
				 :n-gpu-layers n-gpu-layers :split-mode split-mode :main-gpu main-gpu
				 :tensor-split tensor-split :progress-callback progress-callback
				 :progress-callback-user-data progress-callback-user-data
				 :kv-overrides kv-overrides :vocab-only vocab-only
				 :use-mmap use-mmap :use-mlock use-mlock :check-tensors check-tensors)))

(defmethod cffi:translate-into-foreign-memory (value (type c-model-params) ptr)
  (cffi:with-foreign-slots ((devices tensor-buft-overrides
			     n-gpu-layers split-mode main-gpu tensor-split
			     progress-callback progress-callback-user-data
			     kv-overrides vocab-only use-mmap use-mlock check-tensors)
			    ptr (:struct llama-model-params))
    (setf devices (slot-value value 'devices)
	  tensor-buft-overrides (slot-value value 'tensor-buft-overrides)
	  n-gpu-layers (slot-value value 'n-gpu-layers)
	  split-mode (slot-value value 'split-mode)
	  main-gpu (slot-value value 'main-gpu)
	  tensor-split (slot-value value 'tensor-split)
	  progress-callback (slot-value value 'progress-callback)
	  progress-callback-user-data (slot-value value 'progress-callback-user-data)
	  kv-overrides (slot-value value 'kv-overrides)
	  vocab-only (slot-value value 'vocab-only)
	  use-mmap (slot-value value 'use-mmap)
	  use-mlock (slot-value value 'use-mlock)
	  check-tensors (slot-value value 'check-tensors))))

(defmethod cffi:free-translated-object (ptr (type c-model-params) param)
  (cffi:foreign-free ptr))

(defmethod cffi:translate-from-foreign (ptr (type c-context-params))
  (cffi:with-foreign-slots ((n-ctx n-batch n-ubatch n-seq-max n-threads n-threads-batch
			     rope-scaling-type pooling-type attention-type flash-attn-type rope-freq-base rope-freq-scale
			     yarn-ext-factor yarn-attn-factor yarn-beta-fast yarn-beta-slow yarn-orig-ctx
			     defrag-thold cb-eval cb-eval-user-data
			     type-k type-v embeddings offload-kqv no-perf op-offload swa-full kv-unified)
			    ptr (:struct llama-context-params))
    (make-instance 'context-params :n-ctx n-ctx :n-batch n-batch :n-ubatch n-ubatch :n-seq-max n-seq-max
				   :n-threads n-threads :n-threads-batch n-threads-batch
				   :rope-scaling-type rope-scaling-type :pooling-type pooling-type
				   :attention-type attention-type :flash-attn-type flash-attn-type
				   :rope-freq-base rope-freq-base :rope-freq-scale rope-freq-scale
				   :yarn-ext-factor yarn-ext-factor :yarn-attn-factor yarn-attn-factor
				   :yarn-beta-fast yarn-beta-fast :yarn-beta-slow yarn-beta-slow :yarn-orig-ctx yarn-orig-ctx
				   :defrag-thold defrag-thold :cb-eval cb-eval :cb-eval-user-data cb-eval-user-data
				   :type-k type-k :type-v type-v
				   :embeddings embeddings
				   :offload-kqv offload-kqv :no-perf no-perf
				   :op-offload op-offload :swa-full swa-full :kv-unified kv-unified)))

(defmethod cffi:translate-into-foreign-memory (value (type c-context-params) ptr)
  (cffi:with-foreign-slots ((n-ctx n-batch n-ubatch n-seq-max n-threads n-threads-batch
			     rope-scaling-type pooling-type attention-type flash-attn-type rope-freq-base rope-freq-scale
			     yarn-ext-factor yarn-attn-factor yarn-beta-fast yarn-beta-slow yarn-orig-ctx
			     defrag-thold cb-eval cb-eval-user-data
			     type-k type-v embeddings offload-kqv no-perf op-offload swa-full kv-unified)
			    ptr (:struct llama-context-params))
    (setf n-ctx (slot-value value 'n-ctx)
	  n-batch (slot-value value 'n-batch)
	  n-ubatch (slot-value value 'n-ubatch)
	  n-seq-max (slot-value value 'n-seq-max)
	  n-threads (slot-value value 'n-threads)
	  n-threads-batch (slot-value value 'n-threads-batch)
	  rope-scaling-type (slot-value value 'rope-scaling-type)
	  pooling-type (slot-value value 'pooling-type)
	  attention-type (slot-value value 'attention-type)
	  flash-attn-type (slot-value value 'flash-attn-type)
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
	  embeddings (slot-value value 'embeddings)
	  offload-kqv (slot-value value 'offload-kqv)
	  no-perf (slot-value value 'no-perf)
	  op-offload (slot-value value 'op-offload)
	  swa-full (slot-value value 'swa-full)
	  kv-unified (slot-value value 'kv-unified))))

(defmethod cffi:free-translated-object (ptr (type c-context-params) param)
  (cffi:foreign-free ptr))

(defmethod cffi:translate-from-foreign (ptr (type c-model-quantize-params))
  (cffi:with-foreign-slots ((nthread ftype output-tensor-type token-embedding-type allow-requantize
			     quantize-output-tensor only-copy pure keep-split imatrix kv-overrides)
			    ptr (:struct llama-model-quantize-params))
    (make-instance 'model-quantize-params :nthread nthread :ftype ftype :output-tensor-type output-tensor-type
					  :token-embedding-type token-embedding-type :allow-requantize allow-requantize
					  :quantize-output-tensor quantize-output-tensor :only-copy only-copy :pure pure
					  :keep-split keep-split :imatrix imatrix :kv-overrides kv-overrides)))

(defmethod cffi:translate-into-foreign-memory (value (type c-model-quantize-params) ptr)
  (cffi:with-foreign-slots ((nthread ftype output-tensor-type token-embedding-type allow-requantize
			     quantize-output-tensor only-copy pure keep-split imatrix kv-overrides)
			    ptr (:struct llama-model-quantize-params))
    (setf nthread (slot-value value 'nthread)
	  ftype (slot-value value 'ftype)
	  output-tensor-type (slot-value value 'output-tensor-type)
	  token-embedding-type (slot-value value 'token-embedding-type)
	  allow-requantize (slot-value value 'allow-requantize)
	  quantize-output-tensor (slot-value value 'quantize-output-tensor)
	  only-copy (slot-value value 'only-copy)
	  pure (slot-value value 'pure)
	  keep-split (slot-value value 'keep-split)
	  imatrix (slot-value value 'imatrix)
	  kv-overrides (slot-value value 'kv-overrides))))

(defmethod cffi:free-translated-object (ptr (type c-model-quantize-params) param)
  (cffi:foreign-free ptr))

(defmethod cffi:translate-from-foreign (ptr (type c-batch))
  (cffi:with-foreign-slots ((n-tokens token embd pos n-seq-id seq-id logits)
			    ptr (:struct llama-batch))
    (list n-tokens token embd pos n-seq-id seq-id logits)))

(defmethod cffi:translate-into-foreign-memory (value (type c-batch) ptr)
  (cffi:with-foreign-slots ((n-tokens token embd pos n-seq-id seq-id logits)
			    ptr (:struct llama-batch))
    (setf n-tokens (elt value 0)
	  token (elt value 1)
	  embd (elt value 2)
	  pos (elt value 3)
	  n-seq-id (elt value 4)
	  seq-id (elt value 5)
	  logits (elt value 6))))

(defmethod cffi:free-translated-object (ptr (type c-batch) param)
  (cffi:foreign-free ptr))

(cffi:defcstruct llama-adapter-lora)

(cffi:defcfun llama-model-default-params (:struct llama-model-params))

(cffi:defcfun llama-context-default-params (:struct llama-context-params))

(cffi:defcfun llama-sampler-chain-default-params (:struct llama-sampler-chain-params))

(cffi:defcfun llama-model-quantize-default-params (:struct llama-model-quantize-params))

(cffi:defcfun llama-backend-init :void)

(cffi:defcfun llama-backend-free :void)

(cffi:defcfun llama-numa-init :void
  (numa :int))

(cffi:defcfun llama-attach-threadpool :void
  (ctx (:pointer (:struct llama-context)))
  (threadpool (:pointer :void))
  (threadpool-batch (:pointer :void)))

(cffi:defcfun llama-detach-threadpool :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-model-load-from-file (:pointer (:struct llama-model))
  (path-model :string)
  (params (:struct llama-model-params)))

(cffi:defcfun llama-model-load-from-splits (:pointer (:struct llama-model))
  (paths (:pointer :string))
  (n-paths :unsigned-long)
  (params (:struct llama-model-params)))

;; llama_model_save_to_file

(cffi:defcfun llama-model-free :void
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-init-from-model (:pointer (:struct llama-context))
  (model (:pointer (:struct llama-model)))
  (params (:struct llama-context-params)))

(cffi:defcfun llama-free :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-time-us :int64)

(cffi:defcfun llama-max-devices :int)

(cffi:defcfun llama-max-parallel-sequences :int)

(cffi:defcfun llama-supports-mmap :boolean)

(cffi:defcfun llama-supports-mlock :boolean)

(cffi:defcfun llama-supports-gpu-offload :boolean)

(cffi:defcfun llama-supports-rpc :boolean)

(cffi:defcfun llama-n-ctx :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-ctx-seq :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-batch :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-ubatch :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-seq-max :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-get-model (:pointer (:struct llama-model))
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-get-memory (:pointer (:struct llama-memory))
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-pooling-type :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-model-get-vocab (:pointer (:struct llama-vocab))
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-rope-type :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-ctx-train :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-embd :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-embd-inp :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-layer :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-head :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-head-kv :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-swa :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-rope-freq-scale-train :float
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-n-cls-out :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-cls-label :string
  (model (:pointer (:struct llama-model)))
  (i :int))

(cffi:defcfun llama-vocab-type :int
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-n-tokens :int
  (vocab (:pointer (:struct llama-vocab))))

;; llama_model_meta_val_str
;; llama_model_meta_count
;; llama_model_meta_key_str
;; llama_model_meta_key_by_index
;; llama_model_meta_val_str_by_index

(cffi:defcfun llama-model-desc :int
  (model (:pointer (:struct llama-model)))
  (buf :string)
  (buf-size :unsigned-long))

(cffi:defcfun llama-model-size :unsigned-long
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-chat-template :string
  (model (:pointer (:struct llama-model)))
  (name :string))

(cffi:defcfun llama-model-n-params :unsigned-long
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-has-encoder :bool
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-has-decoder :bool
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-decoder-start-token llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-is-recurrent :bool
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-is-hybrid :bool
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-is-difussion :bool
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-quantize :int
  (fname-inp :string)
  (fname-out :string)
  (params (:pointer (:struct llama-model-quantize-params))))

(cffi:defcfun llama-adapter-lora-init (:struct llama-adapter-lora)
  (model (:pointer (:struct llama-model)))
  (path-lora :string))

;; llama-adapter-meta-val-str
;; llama-adapter-meta-count
;; llama-adapter-meta-key-by
;; llama-adapter-meta-val-str-by-index

(cffi:defcfun llama-adapter-lora-free :void
  (adapter (:pointer (:struct llama-adapter-lora))))

;; llama-adapter-get-alora-n-invocation-tokens
;; llama-adapter-get-alora-invocation-tokens

(cffi:defcfun llama-set-adapter-lora :int
  (ctx (:pointer (:struct llama-context)))
  (adapter (:pointer (:struct llama-adapter-lora)))
  (scale :float))

(cffi:defcfun llama-rm-adapter-lora :int
  (ctx (:pointer (:struct llama-context)))
  (adapter (:pointer (:struct llama-adapter-lora))))

(cffi:defcfun llama-clear-adapter-lora :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-apply-adapter-cvec :int
  (ctx (:pointer (:struct llama-context)))
  (data (:pointer :float))
  (len :int)
  (n-embd :int)
  (il-start :int)
  (il-end :int))

;; Memory

(cffi:defcfun llama-memory-clear :void
  (mem (:pointer (:struct llama-memory)))
  (data :bool))

;; llama_memory_seq_rm
;; llama_memory_seq_cp
;; llama_memory_seq_keep
;; llama_memory_seq_add
;; llama_memory_seq_div
;; llama_memory_seq_pos_min
;; llama_memory_seq_pos_max
;; llama_memory_can_shift

;; State / sessions

(cffi:defcfun llama-state-get-size :unsigned-long
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-state-get-data :unsigned-long
  (ctx (:pointer (:struct llama-context)))
  (dest (:pointer :uint8)))

(cffi:defcfun llama-state-set-data :unsigned-long
  (ctx (:pointer (:struct llama-context)))
  (src (:pointer :uint8)))

;; llama_state_load_file
;; llama_state_save_file

(cffi:defcfun llama-state-seq-get-size :unsigned-long
  (ctx (:pointer (:struct llama-context)))
  (seq-id llama-seq-id))

(cffi:defcfun llama-state-seq-get-data :unsigned-long
  (ctx (:pointer (:struct llama-context)))
  (dest (:pointer :uint8))
  (seq-id llama-seq-id))

(cffi:defcfun llama-state-seq-set-data :unsigned-long
  (ctx (:pointer (:struct llama-context)))
  (src (:pointer :uint8))
  (seq-id llama-seq-id))

;; llama-state-seq-save-file
;; llama-state-seq-load-file
;; llama-state-seq-get-size-ext
;; llama-state-seq-get-data-ext
;; llama-state-seq-set-data-ext

;; Decoding

(cffi:defcfun llama-batch-get-one (:struct llama-batch)
  (tokens (:pointer llama-token))
  (n-tokens :int32))

(cffi:defcfun llama-batch-init (:struct llama-batch)
  (n-tokens :int32)
  (embd :int32)
  (n-seq-max :int32))

(cffi:defcfun llama-batch-free :void
  (batch (:struct llama-batch)))

(cffi:defcfun llama-encode :int
  (ctx (:pointer (:struct llama-context)))
  (batch (:struct llama-batch)))

(cffi:defcfun llama-decode :int
  (ctx (:pointer (:struct llama-context)))
  (batch (:struct llama-batch)))

(cffi:defcfun llama-set-n-threads :void
  (ctx (:pointer (:struct llama-context)))
  (n-threads :int)
  (n-threads-batch :int))

(cffi:defcfun llama-n-threads :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-threads-batch :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-set-embeddings :void
  (ctx (:pointer (:struct llama-context)))
  (embeddings :bool))

(cffi:defcfun llama-set-causal-attn :void
  (ctx (:pointer (:struct llama-context)))
  (causal-attn :bool))

(cffi:defcfun llama-set-warmup :void
  (ctx (:pointer (:struct llama-context)))
  (warmup :bool))

(cffi:defcfun llama-set-abort-callback :void
  (ctx (:pointer (:struct llama-context)))
  (abort-callback :pointer)
  (abort-callback-data :pointer))

(cffi:defcfun llama-synchronize :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-get-logits (:pointer :float)
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-get-logits-ith (:pointer :float)
  (ctx (:pointer (:struct llama-context)))
  (i :int))

(cffi:defcfun llama-get-embeddings (:pointer :float)
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-get-embeddings-ith (:pointer :float)
  (ctx (:pointer (:struct llama-context)))
  (i :int))

(cffi:defcfun llama-get-embeddings-seq (:pointer :float)
  (ctx (:pointer (:struct llama-context)))
  (seq-id llama-seq-id))

;; Vocab

(cffi:defcfun llama-vocab-get-text :string
  (vocab (:pointer (:struct llama-vocab)))
  (token llama-token))

(cffi:defcfun llama-vocab-get-score :float
  (vocab (:pointer (:struct llama-vocab)))
  (token llama-token))

(cffi:defcfun llama-vocab-get-attr :int
  (vocab (:pointer (:struct llama-vocab)))
  (token llama-token))

(cffi:defcfun llama-vocab-is-eog :bool
  (vocab (:pointer (:struct llama-vocab)))
  (token llama-token))

(cffi:defcfun llama-vocab-is-control :bool
  (vocab (:pointer (:struct llama-vocab)))
  (token llama-token))

(cffi:defcfun llama-vocab-bos llama-token
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-eos llama-token
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-eot llama-token
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-sep llama-token
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-nl llama-token
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-pad llama-token
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-mask llama-token
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-get-add-bos :int
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-get-add-eos :int
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-get-add-sep :int
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-fim-pre llama-token
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-fim-suf llama-token
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-fim-mid llama-token
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-fim-pad llama-token
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-fim-rep llama-token
  (vocab (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-vocab-fim-sep llama-token
  (vocab (:pointer (:struct llama-vocab))))

;; Tokenization

(cffi:defcfun llama-tokenize :int
  (vocab (:pointer (:struct llama-vocab)))
  (text :string)
  (text-len :int)
  (tokens (:pointer llama-token))
  (n-tokens-max :int)
  (add-special :bool)
  (parse-special :bool))

(cffi:defcfun llama-token-to-piece :int
  (vocab (:pointer (:struct llama-vocab)))
  (token llama-token)
  (buf :string)
  (length :int)
  (lstrip :int)
  (special :bool))

(cffi:defcfun llama-detokenize :int
  (vocab (:pointer (:struct llama-vocab)))
  (tokens (:pointer llama-token))
  (n-tokens :int)
  (text :string)
  (text-len-max :int)
  (remove-special :bool)
  (unparse-special :bool))

;; Chat templates

(cffi:defcfun llama-chat-apply-template :int
  (tmpl :string)
  (chat (:pointer (:struct llama-chat-message)))
  (n-msg :int)
  (bad-ass :bool)
  (buf :string)
  (length :int))

(cffi:defcfun llama-chat-builtin-templates :int
  (output (:pointer :string))
  (len :int))

;; Sampling API

;; llama_sampler_context_t
;; llama_sampler_i
;; llama_sampler
;; llama_sampler_init

(cffi:defcfun llama-sampler-name :string
  (smpl (:pointer (:struct llama-sampler))))

(cffi:defcfun llama-sampler-accept :void
  (smpl (:pointer (:struct llama-sampler)))
  (token llama-token))

(cffi:defcfun llama-sampler-apply :void
  (smpl (:pointer (:struct llama-sampler)))
  (cur-p (:pointer (:struct llama-token-data-array))))

(cffi:defcfun llama-sampler-reset :void
  (smpl (:pointer (:struct llama-sampler))))

(cffi:defcfun llama-sampler-clone (:pointer (:struct llama-sampler))
  (smpl (:pointer (:struct llama-sampler))))

(cffi:defcfun llama-sampler-free :void
  (smpl (:pointer (:struct llama-sampler))))

(cffi:defcfun llama-sampler-chain-init (:pointer (:struct llama-sampler))
  (params (:struct llama-sampler-chain-params)))

(cffi:defcfun llama-sampler-chain-add :void
  (chain (:pointer (:struct llama-sampler)))
  (smpl (:pointer (:struct llama-sampler))))

(cffi:defcfun llama-sampler-chain-get (:pointer (:struct llama-sampler))
  (chain (:pointer (:struct llama-sampler)))
  (i :int))

(cffi:defcfun llama-sampler-chain-n :int
  (chain (:pointer (:struct llama-sampler))))

(cffi:defcfun llama-sampler-chain-remove (:pointer (:struct llama-sampler))
  (chain (:pointer (:struct llama-sampler)))
  (i :int))

(cffi:defcfun llama-sampler-init-greedy (:pointer (:struct llama-sampler)))

(cffi:defcfun llama-sampler-init-dist (:pointer (:struct llama-sampler))
  (seed :unsigned-int))

(cffi:defcfun llama-sampler-init-top-k (:pointer (:struct llama-sampler))
  (k :int))

(cffi:defcfun llama-sampler-init-top-p (:pointer (:struct llama-sampler))
  (p :float)
  (min-keep :unsigned-long))

(cffi:defcfun llama-sampler-init-min-p (:pointer (:struct llama-sampler))
  (p :float)
  (min-keep :unsigned-long))

(cffi:defcfun llama-sampler-init-typical (:pointer (:struct llama-sampler))
  (p :float)
  (min-keep :unsigned-long))

(cffi:defcfun llama-sampler-init-temp (:pointer (:struct llama-sampler))
  (temp :float))

(cffi:defcfun llama-sampler-init-temp-ext (:pointer (:struct llama-sampler))
  (temp :float)
  (delta :float)
  (exponent :float))

(cffi:defcfun llama-sampler-init-xtc (:pointer (:struct llama-sampler))
  (p :float)
  (temp :float)
  (min-keep :unsigned-long)
  (seed :unsigned-int))

(cffi:defcfun llama-sampler-init-top-n-sigma (:pointer (:struct llama-sampler))
  (n :float))

(cffi:defcfun llama-sampler-init-mirostat (:pointer (:struct llama-sampler))
  (n-vocab :int)
  (seed :unsigned-int)
  (tau :float)
  (eta :float)
  (m :int))

(cffi:defcfun llama-sampler-init-mirostat-v2 (:pointer (:struct llama-sampler))
  (seed :unsigned-int)
  (tau :float)
  (eta :float))

(cffi:defcfun llama-sampler-init-grammar (:pointer (:struct llama-sampler))
  (vocab (:pointer (:struct llama-vocab)))
  (grammar-str :string)
  (grammar-root :string))

;; llama_sampler_init_grammar_lazy_patterns

(cffi:defcfun llama-sampler-init-penalties (:pointer (:struct llama-sampler))
  (penalty-last-n :int)
  (penalty-repeat :float)
  (penalty-freq :float)
  (penalty-present :float))

;; (cffi:defcfun llama-sampler-init-dry

(cffi:defcfun llama-sampler-init-logit-bias (:pointer (:struct llama-sampler))
  (n-vocab :int)
  (n-logit-bias :int)
  (logit-bias (:pointer (:struct llama-logit-bias))))

(cffi:defcfun llama-sampler-init-infill (:pointer (:struct llama-sampler))
  (model (:pointer (:struct llama-vocab))))

(cffi:defcfun llama-sampler-get-seed :unsigned-int
  (smpl (:pointer (:struct llama-sampler))))

(cffi:defcfun llama-sampler-sample llama-token
  (smpl (:pointer (:struct llama-sampler)))
  (ctx (:pointer (:struct llama-context)))
  (idx :int))

;; Model split

;; llama_split_path
;; llama_split_prefix

(cffi:defcfun llama-print-system-info :string)

(cffi:defcfun llama-log-set :void
  (log-call-back (:pointer :void))
  (user-data (:pointer :void)))

;; Performance utils

;; llama_perf_context_data
;; llama_perf_sample_data
;; llama_perf_context

(cffi:defcfun llama-perf-context-print :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-perf-context-reset :void
  (ctx (:pointer (:struct llama-context))))

;; llama_perf_context_print
;; llama_perf_context_reset
;; llama_perf_sample

(cffi:defcfun llama-perf-sampler-print :void
  (smpl (:pointer (:struct llama-sampler))))

(cffi:defcfun llama-perf-sampler-reset :void
  (smpl (:pointer (:struct llama-sampler))))

(cffi:defcfun llama-memory-breakdown-print :void
  (ctx (:pointer (:struct llama-context))))

;; llama_opt_param_filter_all
;; llama_opt_params
;; llama_opt_init
;; llama_opt_epoch
