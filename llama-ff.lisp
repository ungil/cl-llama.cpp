(in-package :llama)

(ff:def-foreign-type llama-vocab (* :void))

(ff:def-foreign-type llama-model (* :void))

(ff:def-foreign-type llama-context (* :void))

(ff:def-foreign-type llama-sampler (* :void))

(ff:def-foreign-type llama-memory (* :void))

(ff:def-foreign-type llama-pos :int)

(ff:def-foreign-type llama-token :int)

(ff:def-foreign-type llama-seq-id :int)

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

(ff:def-foreign-type llama-token-data
    (:struct (id llama-token)
	     (logit :float)
	     (p :float)))

(ff:def-foreign-type llama-token-data-array
    (:struct (data (* llama-token-data))
	     (size :unsigned-long)
             (selected :long)
	     (sorted :char boolean)))

(ff:def-foreign-type llama-progress-callback (* :void))

(ff:def-foreign-type llama-batch
    (:struct (n-tokens :int)
	     (token (* llama-token))
	     (embd (* :float))
	     (pos (* llama-pos))
	     (n-seq-id (* :int))
	     (seq-id (* (* llama-seq-id)))
	     (logits (* :int8))))

;; llama_model_kv_override_type
;; (ff:def-foreign-type llama-model-kv-override)

;; llama_model_tensor_buft_override

(ff:def-foreign-type llama-model-params
    (:struct (devices (* :void))
	     (tensor-buft-overrides (* :void))
             (n-gpu-layers :int)
	     (split-mode :int)
	     (main-gpu :int)
	     (tensor-split (* :float))
	     (progress-callback (* :void))
	     (progress-callback-user-data (* :void))
	     (kv-overrides (* :void))
	     (vocab-only :char boolean)
	     (use-mmap :char boolean)
	     (use-mlock :char boolean)
	     (check-tensors :char boolean)
	     (use-extra-bufts :char boolean)
	     (no-host :char boolean)))

(ff:def-foreign-type llama-context-params
    (:struct (n-ctx :unsigned-int)
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
	     (cb-eval (* :void))
	     (cb-eval-user-data (* :void))
	     (type-k :int)
	     (type-v :int)
	     (abort-callback (* :void))
	     (abort-callback-data (* :void))
	     (embeddings :char boolean)
	     (offload-kqv :char boolean)
	     (no-perf :char boolean)
	     (op-offload :char boolean)
	     (swa-full :char boolean)
	     (kv-unified :char boolean)))

(ff:def-foreign-type llama-model-quantize-params
    (:struct (nthread :int)
	     (ftype :int)
	     (output-tensor-type :int)
	     (token-embedding-type :int)
	     (allow-requantize :char boolean)
	     (quantize-output-tensor :char boolean)
	     (only-copy :char boolean)
	     (pure :char boolean)
	     (keep-split :char boolean)
	     (imatrix (* :void))
	     (kv-overrides (* :void))
	     (tensor-types (* :void))
	     (prune-layers (* :void))))

(ff:def-foreign-type llama-logit-bias
    (:struct (token llama-token)
	     (bias :float)))

(ff:def-foreign-type llama-sampler-chain-params
    (:struct (no-perf :char boolean)))

(ff:def-foreign-type llama-chat-message
    (:struct (role (* :char))
	     (content (* :char))))

(ff:def-foreign-type llama-adapter-lora (* :void))

(ff:def-foreign-call (llama-model-default-params "llama_model_default_params")
    (:void)
  :returning llama-model-params
  :pass-structs-by-value t)

(ff:def-foreign-call (llama-context-default-params "llama_context_default_params")
    (:void)
  :returning llama-context-params
  :pass-structs-by-value t)

;;; apparently the following doesn't work because of a bug in ACL
#+NIL
(ff:def-foreign-call (llama-sampler-chain-default-params "llama_sampler_chain_default_params")
    (:void)
  :returning llama-sampler-chain-params
  :pass-structs-by-value t)

(ff:def-foreign-call (llama-sampler-chain-default-params "llama_sampler_chain_default_params")
    (:void)
  :returning llama-sampler-chain-params)

(ff:def-foreign-call (llama-model-quantize-default-params "llama_model_quantize_default_params")
    (:void)
  :returning llama-model-quantize-params
  :pass-structs-by-value t)

(ff:def-foreign-call (llama-backend-init "llama_backend_init")
    (:void)
  :returning :void)

(ff:def-foreign-call (llama-backend-free "llama_backend_free")
    (:void)
  :returning :void)

(ff:def-foreign-call (llama-numa-init "llama_numa_init")
    ((numa :int))
  :returning :void)

(ff:def-foreign-call (llama-attach-threadpool "llama_attach_threadpool")
    ((ctx llama-context)
     (threadpool (* :void))
     (threadpool-batch (* :void)))
  :returning :void)

(ff:def-foreign-call (llama-detach-threadpool "llama_detach_threadpool")
    ((ctx llama-context))
  :returning :void)

(ff:def-foreign-call (llama-model-load-from-file "llama_model_load_from_file")
    ((path-model (* :char))
     (params (* llama-model-params)))
  :returning llama-model)

(ff:def-foreign-call (llama-model-load-from-splits "llama_model_load_from_splits")
    ((paths (* (* :char)))
     (n-paths :unsigned-long)
     (params (* llama-model-params)))
  :returning llama-model)

;; llama_model_save_to_file

(ff:def-foreign-call (llama-model-free "llama_model_free")
    ((model llama-model))
  :returning :void)

(ff:def-foreign-call (llama-init-from-model "llama_init_from_model")
    ((model llama-model)
     (params (* llama-context-params)))
  :returning llama-context)

(ff:def-foreign-call (llama-free "llama_free")
    ((ctx llama-context))
  :returning :void)

(ff:def-foreign-call (llama-time-us "llama_time_us")
    (:void)
  :returning :unsigned-long)

(ff:def-foreign-call (llama-max-devices "llama_max_devices")
    (:void)
  :returning :int)

(ff:def-foreign-call (llama-max-parallel-sequences "llama_max_parallel_sequences")
    (:void)
  :returning :int)

(ff:def-foreign-call (llama-supports-mmap "llama_supports_mmap")
    (:void)
  :returning :boolean)

(ff:def-foreign-call (llama-supports-mlock "llama_supports_mlock")
    (:void)
  :returning :boolean)

(ff:def-foreign-call (llama-supports-gpu-offload "llama_supports_gpu_offload")
    (:void)
  :returning :boolean)

(ff:def-foreign-call (llama-supports-rpc "llama_supports_rpc")
    (:void)
  :returning :boolean)

(ff:def-foreign-call (llama-n-ctx "llama_n_ctx")
    ((ctx llama-context))
  :returning :int)

(ff:def-foreign-call (llama-n-ctx-seq "llama_n_ctx_seq")
    ((ctx llama-context))
  :returning :int)

(ff:def-foreign-call (llama-n-batch "llama_n_batch")
    ((ctx llama-context))
  :returning :int)

(ff:def-foreign-call (llama-n-ubatch "llama_n_ubatch")
    ((ctx llama-context))
  :returning :int)

(ff:def-foreign-call (llama-n-seq-max "llama_n_seq_max")
    ((ctx llama-context))
  :returning :int)

(ff:def-foreign-call (llama-get-model "llama_get_model")
    ((ctx llama-context))
  :returning llama-model)

(ff:def-foreign-call (llama-get-memory "llama_get_memory")
    ((ctx llama-context))
  :returning llama-memory)

(ff:def-foreign-call (llama-pooling-type "llama_pooling_type")
    ((ctx llama-context))
  :returning :int)

(ff:def-foreign-call (llama-model-get-vocab "llama_model_get_vocab")
    ((model llama-model))
  :returning llama-vocab)

(ff:def-foreign-call (llama-model-rope-type "llama_model_rope_type")
    ((model llama-model))
  :returning :int)

(ff:def-foreign-call (llama-model-n-ctx-train "llama_model_n_ctx_train")
    ((model llama-model))
  :returning :int)

(ff:def-foreign-call (llama-model-n-embd "llama_model_n_embd")
    ((model llama-model))
  :returning :int)

(ff:def-foreign-call (llama-model-n-embd-inp "llama_model_n_embd_inp")
    ((model llama-model))
  :returning :int)

(ff:def-foreign-call (llama-model-n-layer "llama_model_n_layer")
    ((model llama-model))
  :returning :int)

(ff:def-foreign-call (llama-model-n-head "llama_model_n_head")
    ((model llama-model))
  :returning :int)

(ff:def-foreign-call (llama-model-n-head-kv "llama_model_n_head_kv")
    ((model llama-model))
  :returning :int)

(ff:def-foreign-call (llama-model-n-swa "llama_model_n_swa")
    ((model llama-model))
  :returning :int)

(ff:def-foreign-call (llama-model-rope-freq-scale-train "llama_model_rope_freq_scale_train")
    ((model llama-model))
  :returning :float)

(ff:def-foreign-call (llama-model-n-cls-out "llama_model_n_cls_out")
    ((model llama-model))
  :returning :int)

(ff:def-foreign-call (llama-model-cls-label "llama_model_cls_label")
    ((model llama-model)
     (i :int))
  :returning ((* :char)))

(ff:def-foreign-call (llama-vocab-type "llama_vocab_type")
    ((vocab llama-vocab))
  :returning :int)

(ff:def-foreign-call (llama-vocab-n-tokens "llama_vocab_n_tokens")
    ((vocab llama-vocab))
  :returning :int)

;; llama_model_meta_val_str
;; llama_model_meta_count
;; llama_model_meta_key_str
;; llama_model_meta_key_by_index
;; llama_model_meta_val_str_by_index

(ff:def-foreign-call (llama-model-desc "llama_model_desc")
    ((model llama-model)
     (buf (* :char))
     (buf-size :unsigned-long))
  :returning :int)

(ff:def-foreign-call (llama-model-size "llama_model_size")
    ((model llama-model))
  :returning :unsigned-long)

(ff:def-foreign-call (llama-model-chat-template "llama_model_chat_template")
    ((model llama-model)
     (name (* :char)))
  :returning ((* :char)))

(ff:def-foreign-call (llama-model-n-params "llama_model_n_params")
    ((model llama-model))
  :returning :unsigned-long)

(ff:def-foreign-call (llama-model-has-encoder "llama_model_has_encoder")
    ((model llama-model))
  :returning :boolean)

(ff:def-foreign-call (llama-model-has-decoder "llama_model_has_decoder")
    ((model llama-model))
  :returning :boolean)

(ff:def-foreign-call (llama-model-decoder-start-token "llama_model_decoder_start_token")
    ((model llama-model))
  :returning llama-token)

(ff:def-foreign-call (llama-model-is-recurrent "llama_model_is_recurrent")
    ((model llama-model))
  :returning :boolean)

(ff:def-foreign-call (llama-model-is-difussion "llama_model_is_difussion")
    ((model llama-model))
  :returning :boolean)

(ff:def-foreign-call (llama-model-quantize "llama_model_quantize")
    ((fname-inp (* :char))
     (fname-out (* :char))
     (params (* llama-model-quantize-params)))
  :returning :unsigned-long)

(ff:def-foreign-call (llama-adapter-lora-init "llama_adapter_lora_init")
    ((params llama-model)
     (path-model (* :char)))
  :returning llama-adapter-lora
  :pass-structs-by-value t)

(ff:def-foreign-call (llama-adapter-lora-free "llama_adapter_lora_free")
    ((adapter (* llama-adapter-lora)))
  :returning :int)

(ff:def-foreign-call (llama-set-adapter-lora "llama_set_adapter_lora")
    ((ctx llama-context)
     (adapter (* llama-adapter-lora))
     (scale :float))
  :returning :int)

(ff:def-foreign-call (llama-rm-adapter-lora "llama_rm_adapter_lora")
    ((ctx llama-context)
     (adapter (* llama-adapter-lora)))
  :returning :int)

(ff:def-foreign-call (llama-clear-adapter-lora "llama_clear_adapter_lora")
    ((ctx llama-context))
  :returning :int)

(ff:def-foreign-call (llama-apply-adapter-cvec "llama_apply_adapter_cvec")
    ((ctx llama-context)
     (data (* :float))
     (len :int)
     (il-start :int)
     (il-end :int))
  :returning :int)

;; Memory

(ff:def-foreign-call (llama-memory-clear "llama_memory_clear")
    ((mem llama-memory)
     (data :char boolean))
  :returning :void)

;; llama_memory_seq_rm
;; llama_memory_seq_cp
;; llama_memory_seq_keep
;; llama_memory_seq_add
;; llama_memory_seq_div
;; llama_memory_seq_pos_min
;; llama_memory_seq_pos_max
;; llama_memory_can_shift

;; State / session

(ff:def-foreign-call (llama-state-get-size "llama_state_get_size")
    ((ctx llama-context))
  :returning :unsigned-long)

(ff:def-foreign-call (llama-state-get-data "llama_state_get_data")
    ((ctx llama-context)
     (dest (* :unsigned-char)))
  :returning :unsigned-long)

(ff:def-foreign-call (llama-state-set-data "llama_state_set_data")
    ((ctx llama-context)
     (src (* :unsigned-char)))
  :returning :unsigned-long)

;; llama_state_load_file
;; llama_state_save_file

;; llama_state_seq_get_size
;; llama_state_seq_get_data
;; llama_state_seq_set_data
;; llama_state_seq_save_file
;; llama_state_seq_load_file
;; llama-state-seq-get-size-ext
;; llama-state-seq-get-data-ext
;; llama-state-seq-set-data-ext

;; Decoding

(ff:def-foreign-call (llama-batch-get-one "llama_batch_get_one")
    ((tokens (* llama-token))
     (n-tokens :int))
  :returning llama-batch
  :pass-structs-by-value t)

(ff:def-foreign-call (llama-batch-init "llama_batch_init")
    ((n-tokens :int)
     (embd :int)
     (n-seq-max :int))
  :returning llama-batch
  :pass-structs-by-value t)

(ff:def-foreign-call (llama-batch-free "llama_batch_free")
    ((batch llama-batch))
  :returning :void
  :pass-structs-by-value t)

(ff:def-foreign-call (llama-encode "llama_encode")
    ((ctx llama-context)
     (batch llama-batch))
  :returning :int
  :pass-structs-by-value t)

(ff:def-foreign-call (llama-decode "llama_decode")
    ((ctx llama-context)
     (batch llama-batch))
  :returning :int
  :pass-structs-by-value t)

(ff:def-foreign-call (llama-set-n-threads "llama_set_n_threads")
    ((ctx llama-context)
     (n-threads :int)
     (n-threads-batch :int))
  :returning :void)

(ff:def-foreign-call (llama-n-threads "llama_n_threads")
    ((ctx llama-context))
  :returning :int)

(ff:def-foreign-call (llama-n-threads-batch "llama_n_threads_batch")
    ((ctx llama-context))
  :returning :int)

(ff:def-foreign-call (llama-set-embeddings "llama_set_embeddings")
    ((ctx llama-context)
     (embeddings :char boolean))
  :returning :void)

(ff:def-foreign-call (llama-set-causal-attn "llama_set_causal_attn")
    ((ctx llama-context)
     (causal-attn :char boolean))
  :returning :void)

(ff:def-foreign-call (llama-set-warmup "llama_set_warmup")
    ((ctx llama-context)
     (warmup :char boolean))
  :returning :void)

(ff:def-foreign-call (llama-set-abort-callback "llama_set_abort_callback")
    ((ctx llama-context)
     (abort-callback (* :void))
     (abort-callback-data (* :void)))
  :returning :void)

(ff:def-foreign-call (llama-synchronize "llama_synchronize")
    ((ctx llama-context))
  :returning :void)

(ff:def-foreign-call (llama-get-logits "llama_get_logits")
    ((ctx llama-context))
  :returning ((* :float)))

(ff:def-foreign-call (llama-get-logits-ith "llama_get_logits_ith")
    ((ctx llama-context)
     (i :int))
  :returning ((* :float)))

(ff:def-foreign-call (llama-get-embeddings "llama_get_embeddings")
    ((ctx llama-context))
  :returning ((* :float)))

(ff:def-foreign-call (llama-get-embeddings-ith "llama_get_embeddings_ith")
    ((ctx llama-context)
     (i :int))
  :returning ((* :float)))

(ff:def-foreign-call (llama-get-embeddings-seq "llama_get_embeddings_seq")
    ((ctx llama-context)
     (seq-id llama-seq-id))
  :returning ((* :float)))

;; Vocab

(ff:def-foreign-call (llama-vocab-get-text "llama_vocab_get_text")
    ((model llama-model)
     (token llama-token))
  :returning ((* :char)))

(ff:def-foreign-call (llama-vocab-get-score "llama_vocab_get_score")
    ((model llama-model)
     (token llama-token))
  :returning :float)

(ff:def-foreign-call (llama-vocab-get-attr "llama_vocab_get_attr")
    ((model llama-model)
     (token llama-token))
  :returning :int)

(ff:def-foreign-call (llama-vocab-is-eog "llama_vocab_is_eog")
    ((model llama-model)
     (token llama-token))
  :returning :boolean)

(ff:def-foreign-call (llama-vocab-is-control "llama_vocab_is_control")
    ((model llama-model)
     (token llama-token))
  :returning :boolean)

(ff:def-foreign-call (llama-vocab-bos "llama_vocab_bos")
    ((vocab llama-vocab))
  :returning llama-token)

(ff:def-foreign-call (llama-vocab-eos "llama_vocab_eos")
    ((vocab llama-vocab))
  :returning llama-token)

(ff:def-foreign-call (llama-vocab-eot "llama_vocab_eot")
    ((vocab llama-vocab))
  :returning llama-token)

(ff:def-foreign-call (llama-vocab-sep "llama_vocab_sep")
    ((vocab llama-vocab))
  :returning llama-token)

(ff:def-foreign-call (llama-vocab-nl "llama_vocab_nl")
    ((vocab llama-vocab))
  :returning llama-token)

(ff:def-foreign-call (llama-vocab-pad "llama_vocab_pad")
    ((vocab llama-vocab))
  :returning llama-token)

(ff:def-foreign-call (llama-vocab-mask "llama_vocab_mask")
    ((vocab llama-vocab))
  :returning llama-token)

(ff:def-foreign-call (llama-vocab-get-add-bos "llama_vocab_get_add_bos")
    ((vocab llama-vocab))
  :returning :int)

(ff:def-foreign-call (llama-vocab-get-add-eos "llama_vocab_get_add_eos")
    ((vocab llama-vocab))
  :returning :int)

(ff:def-foreign-call (llama-vocab-get-add-sep "llama_vocab_get_add_sep")
    ((vocab llama-vocab))
  :returning :int)

(ff:def-foreign-call (llama-vocab-fim-pre "llama_vocab_fim_pre")
    ((vocab llama-vocab))
  :returning llama-token)

(ff:def-foreign-call (llama-vocab-fim-suf "llama_vocab_fim_suf")
    ((vocab llama-vocab))
  :returning llama-token)

(ff:def-foreign-call (llama-vocab-fim-mid "llama_vocab_fim_mid")
    ((vocab llama-vocab))
  :returning llama-token)

(ff:def-foreign-call (llama-vocab-fim-pad "llama_vocab_fim_pad")
    ((vocab llama-vocab))
  :returning llama-token)

(ff:def-foreign-call (llama-vocab-fim-rep "llama_vocab_fim_rep")
    ((vocab llama-vocab))
  :returning llama-token)

(ff:def-foreign-call (llama-vocab-fim-sep "llama_vocab_fim_sep")
    ((vocab llama-vocab))
  :returning llama-token)

;; Tokenization

(ff:def-foreign-call (llama-tokenize "llama_tokenize")
    ((vocab llama-vocab)
     (text (* :char))
     (text-len :int)
     (tokens (* llama-token))
     (n-max-tokens :int)
     (add-special :char boolean)
     (parse-special :char boolean))
  :returning :int)

(ff:def-foreign-call (llama-token-to-piece "llama_token_to_piece")
    ((vocab llama-vocab)
     (token llama-token)
     (buf (* :char))
     (length :int)
     (lstrip :int)
     (special :char boolean))
  :returning :int)

(ff:def-foreign-call (llama-detokenize "llama_detokenize")
    ((vocab llama-vocab)
     (tokens (* llama-token))
     (n-tokens :int)
     (text (* :char))
     (text-len-max :int)
     (remove-special :char boolean)
     (unparse-special :char boolean))
  :returning :int)

;; Chat templates

(ff:def-foreign-call (llama-chat-apply-template "llama_chat_apply_template")
    ((tmpl (* :char))
     (chat (* llama-chat-message))
     (n-msg :int)
     (bad-ass :char boolean)
     (buf (* :char))
     (length :int))
  :returning :int)

(ff:def-foreign-call (llama-chat-builtin-templates "llama_chat_builtin_templates")
    ((output (* (* :char)))
     (len :int))
  :returning :int)

;; Sampling API

;; llama_sampler_context_t
;; llama_sampler_i
;; llama_sampler
;; llama_sampler_init

(ff:def-foreign-call (llama-sampler-name "llama_sampler_name")
    ((smpl llama-sampler))
  :returning ((* :char)))

(ff:def-foreign-call (llama-sampler-accept "llama_sampler_accept")
    ((smpl llama-sampler)
     (token llama-token))
  :returning :void)

(ff:def-foreign-call (llama-sampler-apply "llama_sampler_apply")
    ((smpl llama-sampler)
     (cur-p (* llama-token-data-array)))
  :returning :void)

(ff:def-foreign-call (llama-sampler-reset "llama_sampler_reset")
    ((smpl llama-sampler))
  :returning :void)

(ff:def-foreign-call (llama-sampler-clone "llama_sampler_clone")
    ((smpl llama-sampler))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-free "llama_sampler_free")
    ((smpl llama-sampler))
  :returning :void)

(ff:def-foreign-call (llama-sampler-chain-init "llama_sampler_chain_init")
    ((params llama-sampler-chain-params))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-chain-add "llama_sampler_chain_add")
    ((chain llama-sampler)
     (smpl llama-sampler))
  :returning :void)

(ff:def-foreign-call (llama-sampler-chain-get "llama_sampler_chain_get")
    ((chain llama-sampler)
     (i :int))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-chain-n "llama_sampler_chain_n")
    ((chain llama-sampler))
  :returning :int)

(ff:def-foreign-call (llama-sampler-chain-remove "llama_sampler_chain_remove")
    ((chain llama-sampler)
     (i :int))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-greedy "llama_sampler_init_greedy")
    ()
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-dist "llama_sampler_init_dist")
    ((seed :unsigned-int))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-softmax "llama_sampler_init_softmax")
    ()
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-top-k "llama_sampler_init_top_k")
    ((k :int))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-top-p "llama_sampler_init_top_p")
    ((p :float)
     (min-keep :unsigned-long))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-min-p "llama_sampler_init_min_p")
    ((p :float)
     (min-keep :unsigned-long))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-typical "llama_sampler_init_typical")
    ((p :float)
     (min-keep :unsigned-long))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-temp "llama_sampler_init_temp")
    ((temp :float))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-temp-ext "llama_sampler_init_temp_ext")
    ((temp :float)
     (delta :float)
     (exponent :float))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-xtc "llama_sampler_init_xtc")
    ((p :float)
     (temp :float)
     (min-keep :unsigned-long)
     (seed :unsigned-int))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-top-n-sigma "llama_sampler_init_top_n_sigma")
    ((n :float))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-mirostat "llama_sampler_init_mirostat")
    ((n-vocab :int)
     (seed :unsigned-int)
     (tau :float)
     (eta :float)
     (m :int))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-mirostat-v2 "llama_sampler_init_mirostat_v2")
    ((seed :unsigned-int)
     (tau :float)
     (eta :float))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-grammar "llama_sampler_init_grammar")
    ((vocab llama-vocab)
     (grammar-str (* :char))
     (grammar-root (* :char)))
  :returning llama-sampler)

;; llama_sampler_init_grammar_lazy_patterns

(ff:def-foreign-call (llama-sampler-init-penalties "llama_sampler_init_penalties")
    ((penalty-last-n :int)
     (penalty-repeat :float)
     (penalty-freq :float)
     (penalty-present :float))
  :returning llama-sampler)

;; (ff:def-foreign-call llama-sampler-init-dry

(ff:def-foreign-call (llama-sampler-init-logit-bias "llama_sampler_init_logit_bias")
    ((n-vocab :int)
     (n-logit-bias :int)
     (logit-bias (* llama-logit-bias)))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-init-infill "llama_sampler_init_infill")
    ((model llama-vocab))
  :returning llama-sampler)

(ff:def-foreign-call (llama-sampler-get-seed "llama_sampler_get_seed")
    ((smpl llama-sampler))
  :returning :unsigned-int)

(ff:def-foreign-call (llama-sampler-sample "llama_sampler_sample")
    ((smpl llama-sampler)
     (ctx llama-context)
     (idx :int))
  :returning llama-token)

;; Model split

;; llama_split_path
;; llama_split_prefix

(ff:def-foreign-call (llama-print-system-info "llama_print_system_info")
    (:void)
  :returning ((* :char)))

(ff:def-foreign-call (llama-log-set "llama_log_set")
    ((log-callback (* :void))
     (user-data (* :void)))
  :returning :void)

;; Performance utils

;; llama_perf_context_data
;; llama_perf_sample_data
;; llama_perf_context

(ff:def-foreign-call (llama-perf-context-print "llama_perf_context_print")
    ((ctx llama-context))
  :returning :void)

(ff:def-foreign-call (llama-perf-context-reset "llama_perf_context_reset")
    ((ctx llama-context))
  :returning :void)

;; llama_perf_sampler

(ff:def-foreign-call (llama-perf-sampler-print "llama_perf_sampler_print")
    ((chain llama-sampler))
  :returning :void)

(ff:def-foreign-call (llama-perf-sampler-reset "llama_perf_sampler_reset")
    ((chain llama-sampler))
  :returning :void)

(ff:def-foreign-call (llama-memory-breakdown-print "llama_memory_breakdown_print")
    ((ctx llama-context))
  :returning :void)

;; llama_perf_sampler_print
;; llama_perf_sampler_reset

;; llama_opt_param_filter_all
;; llama_opt_params
;; llama_opt_init
;; llama_opt_epoch
