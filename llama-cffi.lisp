(in-package :llama)

(cffi:defcstruct llama-model)

(cffi:defcstruct llama-context)

(cffi:defcstruct llama-sampler)

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
  (n-tokens :int)
  (token (:pointer llama-token))
  (embd (:pointer :float))
  (pos (:pointer llama-pos))
  (n-seq-id (:pointer :int))
  (seq-id (:pointer (:pointer llama-seq-id)))
  (logits (:pointer :int8))
  (all-pos-0 llama-pos)
  (all-pos-1 llama-pos)
  (all-seq-id llama-seq-id))

;; llama_model_kv_override_type
;; (cffi:defcstruct llama-model-kv-override)

(cffi:defcstruct (llama-model-params :class c-model-params)
  (n-gpu-layers :int)
  (split-mode :int)
  (main-gpu :int)
  (tensor-split (:pointer :float))
  (rpc-servers (:pointer :char))
  (progress-callback :pointer)
  (progress-callback-user-data :pointer)
  (kv-overrides :pointer)
  (vocab-only :bool)
  (use-mmap :bool)
  (use-mlock :bool)
  (check-tensors :bool))

(cffi:defcstruct (llama-context-params :class c-context-params)
  (n-ctx :int)
  (n-batch :int)
  (n-ubatch :int)  
  (n-seq-max :int)  
  (n-threads :int)
  (n-threads-batch :int)
  (rope-scaling-type :int)
  (pooling-type :int)
  (attention-type :int)  
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
  (embeddings :bool)
  (offload-kqv :bool)
  (flash-attn :bool)
  (no-perf :bool)  
  (abort-callback :pointer)
  (abort-callback-data :pointer))

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
  (kv-overrides :pointer))

(cffi:defcstruct (llama-logit-bias)
  (token llama-token)
  (bias :float))

(cffi:defcstruct (llama-sampler-chain-params)
  (no-perf :bool))

(cffi:defcstruct (llama-chat-message)
  (role :string)
  (content :string))

(defmethod cffi:translate-from-foreign (ptr (type c-model-params))
  (cffi:with-foreign-slots ((n-gpu-layers split-mode main-gpu tensor-split rpc-servers
			     progress-callback progress-callback-user-data
			     kv-overrides vocab-only use-mmap use-mlock check-tensors)
			    ptr (:struct llama-model-params))
    (make-instance 'model-params :n-gpu-layers n-gpu-layers :split-mode split-mode :main-gpu main-gpu
				 :tensor-split tensor-split :rpc-servers rpc-servers
				 :progress-callback progress-callback :progress-callback-user-data progress-callback-user-data
				 :kv-overrides kv-overrides :vocab-only vocab-only
				 :use-mmap use-mmap :use-mlock use-mlock :check-tensors check-tensors)))

(defmethod cffi:translate-into-foreign-memory (value (type c-model-params) ptr)
  (cffi:with-foreign-slots ((n-gpu-layers split-mode main-gpu tensor-split rpc-servers
			     progress-callback progress-callback-user-data
			     kv-overrides vocab-only use-mmap use-mlock check-tensors)
			    ptr (:struct llama-model-params))
    (setf n-gpu-layers (slot-value value 'n-gpu-layers)
	  split-mode (slot-value value 'split-mode)
	  main-gpu (slot-value value 'main-gpu)
	  tensor-split (slot-value value 'tensor-split)
	  rpc-servers (slot-value value 'rpc-servers)	  
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
			     rope-scaling-type pooling-type attention-type rope-freq-base rope-freq-scale
			     yarn-ext-factor yarn-attn-factor yarn-beta-fast yarn-beta-slow yarn-orig-ctx
			     defrag-thold cb-eval cb-eval-user-data
			     type-k type-v logits-all embeddings offload-kqv flash-attn no-perf)
			    ptr (:struct llama-context-params))
    (make-instance 'context-params :n-ctx n-ctx :n-batch n-batch :n-ubatch n-ubatch :n-seq-max n-seq-max
				   :n-threads n-threads :n-threads-batch n-threads-batch
				   :rope-scaling-type rope-scaling-type :pooling-type pooling-type :attention-type attention-type
				   :rope-freq-base rope-freq-base :rope-freq-scale rope-freq-scale
				   :yarn-ext-factor yarn-ext-factor :yarn-attn-factor yarn-attn-factor
				   :yarn-beta-fast yarn-beta-fast :yarn-beta-slow yarn-beta-slow :yarn-orig-ctx yarn-orig-ctx
				   :defrag-thold defrag-thold :cb-eval cb-eval :cb-eval-user-data cb-eval-user-data
				   :type-k type-k :type-v type-v
				   :logits-all logits-all :embeddings embeddings
				   :offload-kqv offload-kqv :flash-attn flash-attn :no-perf no-perf)))

(defmethod cffi:translate-into-foreign-memory (value (type c-context-params) ptr)
  (cffi:with-foreign-slots ((n-ctx n-batch n-ubatch n-seq-max n-threads n-threads-batch
			     rope-scaling-type pooling-type attention-type rope-freq-base rope-freq-scale
			     yarn-ext-factor yarn-attn-factor yarn-beta-fast yarn-beta-slow yarn-orig-ctx
			     defrag-thold cb-eval cb-eval-user-data
			     type-k type-v logits-all embeddings offload-kqv flash-attn no-perf)
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
	  embeddings (slot-value value 'embeddings)
	  offload-kqv (slot-value value 'offload-kqv)	  
	  flash-attn (slot-value value 'flash-attn)
	  no-perf (slot-value value 'no-perf))))

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
  (cffi:with-foreign-slots ((n-tokens token embd pos n-seq-id seq-id logits all-pos-0 all-pos-1 all-seq-id)
			    ptr (:struct llama-batch))
    (list n-tokens token embd pos n-seq-id seq-id logits all-pos-0 all-pos-1 all-seq-id)))

(defmethod cffi:translate-into-foreign-memory (value (type c-batch) ptr)
  (cffi:with-foreign-slots ((n-tokens token embd pos n-seq-id seq-id logits all-pos-0 all-pos-1 all-seq-id)
			    ptr (:struct llama-batch))
    (setf n-tokens (elt value 0)
	  token (elt value 1)
	  embd (elt value 2)
	  pos (elt value 3)
	  n-seq-id (elt value 4)
	  seq-id (elt value 5)	  
	  logits (elt value 6)
	  all-pos-0 (elt value 7)
	  all-pos-1 (elt value 8)
	  all-seq-id (elt value 9))))

(defmethod cffi:free-translated-object (ptr (type c-batch) param)
  (cffi:foreign-free ptr))

(cffi:defcstruct llama-lora-adapter)

(cffi:defcfun llama-model-default-params (:struct llama-model-params))

(cffi:defcfun llama-context-default-params (:struct llama-context-params))

(cffi:defcfun llama-sampler-chain-default-params (:struct llama-sampler-chain-params))

(cffi:defcfun llama-model-quantize-default-params (:struct llama-model-quantize-params))

(cffi:defcfun llama-backend-init :void)

(cffi:defcfun llama-numa-init :void
  (numa :int))

(cffi:defcfun llama-attach-threadpool :void
  (ctx (:pointer (:struct llama-context)))
  (threadpool (:pointer :void))
  (threadpool-batch (:pointer :void)))

(cffi:defcfun llama-detach-threadpool :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-backend-free :void)

(cffi:defcfun llama-load-model-from-file (:pointer (:struct llama-model))
  (path-model :string)
  (params (:struct llama-model-params)))

(cffi:defcfun llama-free-model :void
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-new-context-with-model (:pointer (:struct llama-context))
  (model (:pointer (:struct llama-model)))
  (params (:struct llama-context-params)))

(cffi:defcfun llama-free :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-time-us :int64)

(cffi:defcfun llama-max-devices :int)

(cffi:defcfun llama-supports-mmap :boolean)

(cffi:defcfun llama-supports-mlock :boolean)

(cffi:defcfun llama-supports-gpu-offload :boolean)

(cffi:defcfun llama-n-ctx :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-batch :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-ubatch :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-seq-max :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-n-vocab :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-n-ctx-train :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-n-embd :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-n-layer :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-n-head :int
  (model (:pointer (:struct llama-model))))
  
(cffi:defcfun llama-get-model (:pointer (:struct llama-model))
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-pooling-type :int
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-vocab-type :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-rope-type :int
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-rope-freq-scale-train :float
  (model (:pointer (:struct llama-model))))

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

(cffi:defcfun llama-model-has-encoder :bool
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-has-decoder :bool
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-decoder-start-token llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-is-recurrent :bool
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-model-quantize :int
  (fname-inp :string)
  (fname-out :string)
  (params (:pointer (:struct llama-model-quantize-params))))

(cffi:defcfun llama-lora-adapter-init (:struct llama-lora-adapter)
  (model (:pointer (:struct llama-model)))
  (path-lora :string))

(cffi:defcfun llama-lora-adapter-set :int
  (ctx (:pointer (:struct llama-context)))
  (adapter (:pointer (:struct llama-lora-adapter)))
  (scale :float))

(cffi:defcfun llama-lora-adapter-remove :int
  (ctx (:pointer (:struct llama-context)))
  (adapter (:pointer (:struct llama-lora-adapter))))

(cffi:defcfun llama-lora-adapter-clear :void
  (ctx (:pointer (:struct llama-context))))

(cffi:defcfun llama-lora-adapter-free :void
  (adapter (:pointer (:struct llama-lora-adapter))))

(cffi:defcfun llama-control-vector-apply :int
  (lctx (:pointer (:struct llama-context)))
  (data (:pointer :float))
  (len :int)
  (n-embd :int)
  (il-start :int)
  (il-end :int))

;; KV cache

;; llama_kv_cache_view_cell
;; llama_kv_cache_view
;; llama_kv_cache_view_init
;; llama_kv_cache_view_free
;; llama_kv_cache_view_update
;; llama_get_kv_cache_token_count
;; llama_get_kv_cache_used_cells

(cffi:defcfun llama-kv-cache-clear :void
  (ctx (:pointer (:struct llama-context))))

;; llama_kv_cache_seq_rm
;; llama_kv_cache_seq_cp
;; llama_kv_cache_seq_keep
;; llama_kv_cache_seq_add
;; llama_kv_cache_seq_div
;; llama_kv_cache_seq_pos_max
;; llama_kv_cache_defrag
;; llama_kv_cache_update

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

;; llama-state-seq-load-file
;; llama-state-seq-save-file

;; Decoding

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

(cffi:defcfun llama-token-get-text :string
  (model (:pointer (:struct llama-model)))
  (token llama-token))

(cffi:defcfun llama-token-get-score :float
  (model (:pointer (:struct llama-model)))
  (token llama-token))

(cffi:defcfun llama-token-get-attr :int
  (model (:pointer (:struct llama-model)))
  (token llama-token))

(cffi:defcfun llama-token-is-eog :bool
  (model (:pointer (:struct llama-model)))
  (token llama-token))

(cffi:defcfun llama-token-is-control :bool
  (model (:pointer (:struct llama-model)))
  (token llama-token))

(cffi:defcfun llama-token-bos llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-token-eos llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-token-cls llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-token-sep llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-token-nl llama-token
  (model (:pointer (:struct llama-model))))

(cffi:defcfun llama-token-pad llama-token
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

(cffi:defcfun llama-token-eot llama-token
  (model (:pointer (:struct llama-model))))

;; Tokenization

(cffi:defcfun llama-tokenize :int
  (model (:pointer (:struct llama-model)))
  (text :string)
  (text-len :int)
  (tokens (:pointer llama-token))
  (n-tokens-max :int)
  (add-special :bool)
  (parse-special :bool))

(cffi:defcfun llama-token-to-piece :int
  (model (:pointer (:struct llama-model)))
  (token llama-token)
  (buf :string)
  (length :int)
  (lstrip :int)  
  (special :bool))

(cffi:defcfun llama-detokenize :int
  (model (:pointer (:struct llama-model)))
  (tokens (:pointer llama-token))
  (n-tokens :int)
  (text :string)
  (text-len-max :int)
  (remove-special :bool)
  (unparse-special :bool))

;; Chat templates

(cffi:defcfun llama-chat-apply-template :int
  (model (:pointer (:struct llama-model)))
  (tmpl :string)
  (chat (:pointer (:struct llama-chat-message)))
  (n-msg :int)
  (bad-ass :bool)
  (buf :string)
  (length :int))

;; Sampling API

;; llama_sampler_context_t
;; llama_sampler_i
;; llama_sampler

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

(cffi:defcfun llama-sampler-init-softmax (:pointer (:struct llama-sampler)))

(cffi:defcfun llama-sampler-init-top-k (:pointer (:struct llama-sampler))
  (k :int))

(cffi:defcfun llama-sampler-init-top-p (:pointer (:struct llama-sampler))
  (p :float)
  (min-keep :unsigned-long))

(cffi:defcfun llama-sampler-init-min-p (:pointer (:struct llama-sampler))
  (p :float)
  (min-keep :unsigned-long))

(cffi:defcfun llama-sampler-init-tail-free (:pointer (:struct llama-sampler))
  (z :float)
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
  (model (:pointer (:struct llama-model)))
  (grammar-str :string)
  (grammar-root :string))

(cffi:defcfun llama-sampler-init-penalties (:pointer (:struct llama-sampler))
  (n-vocab :int)
  (special-eos-id llama-token)
  (linefeed-id llama-token)
  (penalty-last-n :int)
  (penalty-repeat :float)
  (penalty-freq :float)
  (penalty-present :float)
  (penalize-nl :bool)
  (ignore-eos :bool))

(cffi:defcfun llama-sampler-init-logit-bias (:pointer (:struct llama-sampler))
  (n-vocab :int)
  (n-logit-bias :int)
  (logit-bias (:pointer (:struct llama-logit-bias))))

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

;; llama_perf_dump_yaml
