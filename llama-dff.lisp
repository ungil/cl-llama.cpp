(in-package :llama)

(fli:define-c-struct (llama-model (:foreign-name "llama_model")))

(fli:define-c-struct (llama-context (:foreign-name "llama_context")))

(fli:define-c-struct (llama-sampler (:foreign-name "llama_sampler")))

(fli:define-c-typedef (llama-pos (:foreign-name "llama_pos")) :int)

(fli:define-c-typedef (llama-token (:foreign-name "llama_token")) :int)

(fli:define-c-typedef (llama-seq-id (:foreign-name "llama_seq_id")) :int)

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

(fli:define-c-struct (llama-token-data (:foreign-name "llama_token_data"))
    (id llama-token)
  (logit :float)
  (p :float))

(fli:define-c-struct (llama-token-data-array (:foreign-name "llama_token_data_array"))
    (data (:pointer llama-token-data))
  (size :unsigned-long)
  (selected :unsigned-long)
  (sorted (:boolean :byte)))

(fli:define-c-typedef (llama-progress-callback (:foreign-name "llama_progress_callback"))
    (:pointer (:function (:float (:pointer :void)) (:boolean :byte))))

(fli:define-c-struct (llama-batch (:foreign-name "llama_batch"))
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
;; (fli:define-c-struct (llama-batch (:foreign-name "llama_model_kv_override")))

(fli:define-c-struct (llama-model-params (:foreign-name "llama_model_params"))
    (n-gpu-layers :int)
  (split-mode :int)
  (main-gpu :int)
  (tensor-split (:pointer :float))
  (rpc-servers (:pointer :char))
  (progress-callback (:pointer :void))
  (progress-callback-user-data (:pointer :void))
  (kv-overrides (:pointer :void))
  (vocab-only (:boolean :byte))
  (use-mmap (:boolean :byte))
  (use-mlock (:boolean :byte))
  (check-tensors (:boolean :byte)))

(fli:define-c-struct (llama-context-params (:foreign-name "llama_context_params"))
    (n-ctx :unsigned-int)
  (n-batch :unsigned-int)
  (n-ubatch :unsigned-int)
  (n-seq-max :unsigned-int)
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
  (yarn-orig-ctx :unsigned-int)
  (defrag-thold :float)
  (cb-eval (:pointer :void))
  (cb-eval-user-data (:pointer :void))
  (type-k :int)
  (type-v :int)
  (logits-all (:boolean :byte))
  (embeddings (:boolean :byte))
  (offload-kqv (:boolean :byte))
  (flash-attn (:boolean :byte))
  (no-perf (:boolean :byte))
  (abort-callback (:pointer :void))
  (abort-callback-data (:pointer :void)))

(fli:define-c-struct (llama-model-quantize-params (:foreign-name "llama_model_quantize_params"))
    (nthread :int)
  (ftype :int)
  (output-tensor-type :int)
  (token-embedding-type :int)
  (allow-requantize (:boolean :byte))
  (quantize-output-tensor (:boolean :byte))
  (only-copy (:boolean :byte))    
  (pure (:boolean :byte))
  (keep-split (:boolean :byte))
  (imatrix (:pointer :void))
  (kv-overrides (:pointer :void)))

(fli:define-c-struct (llama-logit-bias (:foreign-name "llama_logit_bias"))
    (token llama-token)
  (bias :float))

(fli:define-c-struct (llama-sampler-chain-params (:foreign-name "llama_sampler_chain_params"))
    (no-perf (:boolean :byte)))

(fli:define-c-struct (llama-chat-message (:foreign-name "llama_chat_message"))
    (role (:pointer :char))
  (content (:pointer :char)))

(fli:define-c-struct (llama-lora-adapter (:foreign-name "llama_lora_adapter")))

(fli:define-foreign-function (llama-model-default-params "llama_model_default_params")
  nil
  :result-type (:struct llama-model-params))

(fli:define-foreign-function (llama-context-default-params "llama_context_default_params")  
  nil
  :result-type (:struct llama-context-params))

(fli:define-foreign-function (llama-sampler-chain-default-params "llama_sampler_chain_default_params")
  nil
  :result-type (:struct llama-sampler-chain-params))

(fli:define-foreign-function (llama-model-quantize-default-params "llama_model_quantize_default_params")
  nil
  :result-type (:struct llama-model-quantize-params))

(fli:define-foreign-function (llama-backend-init "llama_backend_init")
  nil
  :result-type :void)

(fli:define-foreign-function (llama-numa-init "llama_numa_init")
    ((numa :int))
  :result-type :void)

(fli:define-foreign-function (llama-attach-threadpool "llama_attach_threadpool")
    ((ctx (:pointer (:struct llama-context)))
     (threadpool (:pointer :void))
     (threadpool-batch (:pointer :void)))
  :result-type :void)

(fli:define-foreign-function (llama-detach-threadpool "llama_detach_threadpool")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :void)

(fli:define-foreign-function (llama-backend-free "llama_backend_free")
  nil
  :result-type :void)

(fli:define-foreign-function (llama-load-model-from-file "llama_load_model_from_file")
    ((path-model (:reference-pass :ef-mb-string))
     (params (:struct llama-model-params)))
  :result-type (:pointer (:struct llama-model)))

(fli:define-foreign-function (llama-free-model "llama_free_model")
    ((model (:pointer (:struct llama-model))))
  :result-type :void)

(fli:define-foreign-function (llama-new-context-with-model "llama_new_context_with_model")
    ((model (:pointer (:struct llama-model)))
     (params (:struct llama-context-params)))
  :result-type (:pointer (:struct llama-context)))

(fli:define-foreign-function (llama-free "llama_free")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :void)

(fli:define-foreign-function (llama-time-us "llama_time_us")
  nil
  :result-type :unsigned-long)

(fli:define-foreign-function (llama-max-devices "llama_max_devices")
  nil
  :result-type :int)

(fli:define-foreign-function (llama-supports-mmap "llama_supports_mmap")
  nil
  :result-type (:boolean :byte))

(fli:define-foreign-function (llama-supports-mlock "llama_supports_mlock")
  nil
  :result-type (:boolean :byte))

(fli:define-foreign-function (llama-supports-gpu-offload "llama_supports_gpu_offload")
  nil
  :result-type (:boolean :byte))

(fli:define-foreign-function (llama-n-ctx "llama_n_ctx")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-n-batch "llama_n_batch")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-n-ubatch "llama_n_ubatch")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-n-seq-max "llama_n_seq_max")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-n-vocab "llama_n_vocab")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

(fli:define-foreign-function (llama-n-ctx-train "llama_n_ctx_train")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

(fli:define-foreign-function (llama-n-embd "llama_n_embd")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

(fli:define-foreign-function (llama-n-layer "llama_n_layer")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

(fli:define-foreign-function (llama-n-head "llama_n_head")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

(fli:define-foreign-function (llama-get-model "llama_get_model")
    ((ctx (:pointer (:struct llama-context))))
  :result-type (:pointer (:struct llama-model)))

(fli:define-foreign-function (llama-pooling-type "llama_pooling_type")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-vocab-type "llama_vocab_type")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

(fli:define-foreign-function (llama-rope-type "llama_rope_type")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

(fli:define-foreign-function (llama-rope-freq-scale-train "llama_rope_freq_scale_train")
    ((model (:pointer (:struct llama-model))))
  :result-type :float)

;; llama_model_meta_val_str
;; llama_model_meta_count
;; llama_model_meta_key_by_index
;; llama_model_meta_val_str_by_index

(fli:define-foreign-function (llama-model-desc "llama_model_desc")
    ((model (:pointer (:struct llama-model)))
     (buf (:pointer :char))
     (buf-size :unsigned-long))
  :result-type :int)

(fli:define-foreign-function (llama-model-size "llama_model_size")
    ((model (:pointer (:struct llama-model))))
  :result-type :unsigned-long)

(fli:define-foreign-function (llama-model-n-params "llama_model_n_params")
    ((model (:pointer (:struct llama-model))))
  :result-type :unsigned-long)

;; llama_get_model_tensor

(fli:define-foreign-function (llama-model-has-encoder "llama_model_has_encoder")
    ((model (:pointer (:struct llama-model))))
  :result-type (:boolean :byte))

(fli:define-foreign-function (llama-model-has-decoder "llama_model_has_decoder")
    ((model (:pointer (:struct llama-model))))
  :result-type (:boolean :byte))

(fli:define-foreign-function (llama-model-decoder-start-token "llama_model_decoder_start_token")
    ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

(fli:define-foreign-function (llama-model-is-recurrent "llama_model_is_recurrent")
    ((model (:pointer (:struct llama-model))))
  :result-type (:boolean :byte))

(fli:define-foreign-function (llama-model-quantize "llama_model_quantize")
    ((fname-inp (:reference-pass :ef-mb-string))
     (fname-out (:reference-pass :ef-mb-string))
     (params (:pointer (:struct llama-model-quantize-params))))
  :result-type :unsigned-long)

(fli:define-foreign-function (llama-lora-adapter-init "llama_lora_adapter_init")
    ((model (:pointer (:struct llama-model)))
     (path-lora (:reference-pass :ef-mb-string)))
  :result-type (:pointer (:struct llama-lora-adapter)))

(fli:define-foreign-function (llama-lora-adapter-set "llama_lora_adapter_set")
    ((ctx (:pointer (:struct llama-context)))
     (adapter (:pointer (:struct llama-lora-adapter)))
     (scale :float))
  :result-type :int)

(fli:define-foreign-function (llama-lora-adapter-remove "llama_lora_adapter_remove")
    ((ctx (:pointer (:struct llama-context)))
     (adapter (:pointer (:struct llama-lora-adapter))))
  :result-type :int)

(fli:define-foreign-function (llama-lora-adapter-clear "llama_lora_adapter_clear")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :void)

(fli:define-foreign-function (llama-lora-adapter-free "llama_lora_adapter_free")
    ((adapter (:pointer (:struct llama-lora-adapter))))
  :result-type :void)

(fli:define-foreign-function (llama-control-vector-apply "llama_control_vector_apply")
    ((lctx (:pointer (:struct llama-context)))
     (data (:pointer :float))
     (len :int)
     (n-embd :int)
     (il-start :int)
     (il-end :int))
  :result-type :int)

;; KV cache

;; llama_kv_cache_view_cell
;; llama_kv_cache_view
;; llama_kv_cache_view_init
;; llama_kv_cache_view_free
;; llama_kv_cache_view_update
;; llama_get_kv_cache_token_count
;; llama_get_kv_cache_used_cells

(fli:define-foreign-function (llama-kv-cache-clear "llama_kv_cache_clear")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :void)

;; llama_kv_cache_seq_rm
;; llama_kv_cache_seq_cp
;; llama_kv_cache_seq_keep
;; llama_kv_cache_seq_add
;; llama_kv_cache_seq_div
;; llama_kv_cache_seq_pos_max
;; llama_kv_cache_defrag
;; llama_kv_cache_update

;; State / sessions

(fli:define-foreign-function (llama-state-get-size "llama_state_get_size")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :unsigned-long)

(fli:define-foreign-function (llama-state-get-data "llama_state_get_data")
    ((ctx (:pointer (:struct llama-context)))
     (dest (:pointer :unsigned-byte))
     (size :int))
  :result-type :unsigned-long)

(fli:define-foreign-function (llama-state-set-data "llama_state_set_data")
    ((ctx (:pointer (:struct llama-context)))
     (src (:pointer :unsigned-byte))
     (size :int))
  :result-type :unsigned-long)

;; llama_state_load_file
;; llama_state_save_file

;; llama_state_seq_get_size
;; llama_state_seq_get_data
;; llama_state_seq_set_data
;; llama_state_seq_save_file
;; llama_state_seq_load_file

;; Decoding

(fli:define-foreign-function (llama-batch-get-one "llama_batch_get_one")
    ((tokens (:pointer llama-token))
     (n-tokens :int)
     (pos-0 llama-pos)
     (seq-id llama-seq-id))
  :result-type (:struct llama-batch))

(fli:define-foreign-function (llama-batch-init "llama_batch_init")
    ((n-tokens :int)
     (embd :int)
     (n-seq-max :int))
  :result-type (:struct llama-batch))

(fli:define-foreign-function (llama-batch-free "llama_batch_free")
    ((batch (:pointer (:struct llama-batch))))
  :result-type :void)

(fli:define-foreign-function (llama-encode "llama_encode")
    ((ctx (:pointer (:struct llama-context)))
     (batch (:pointer (:struct llama-batch))))
  :result-type :int)

(fli:define-foreign-function (llama-decode "llama_decode")
    ((ctx (:pointer (:struct llama-context)))
     (batch (:pointer (:struct llama-batch))))
  :result-type :int)

(fli:define-foreign-function (llama-set-n-threads "llama_set_n_threads")
    ((ctx (:pointer (:struct llama-context)))
     (n-threads :int)
     (n-threads-batch :int))
  :result-type :void)

(fli:define-foreign-function (llama-n-threads "llama_n_threads")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-n-threads-batch "llama_n_threads_batch")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-set-embeddings "llama_set_embeddings")
    ((ctx (:pointer (:struct llama-context)))
     (embeddings (:boolean :byte)))
  :result-type :int)

(fli:define-foreign-function (llama-set-causal-attn "llama_set_causal_attn")
    ((ctx (:pointer (:struct llama-context)))
     (causal-attn (:boolean :byte)))
  :result-type :void)

(fli:define-foreign-function (llama-set-abort-callback "llama_set_abort_callback")
    ((ctx (:pointer (:struct llama-context)))
     (abort-callback (:pointer :void))
     (abort-callback-data (:pointer :void)))
  :result-type :void)

(fli:define-foreign-function (llama-synchronize "llama_synchronize")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :void)

(fli:define-foreign-function (llama-get-logits "llama_get_logits")
    ((ctx (:pointer (:struct llama-context))))
  :result-type (:pointer :float))

(fli:define-foreign-function (llama-get-logits-ith "llama_get_logits_ith")
    ((ctx (:pointer (:struct llama-context)))
     (i :int))
  :result-type (:pointer :float))

(fli:define-foreign-function (llama-get-embeddings "llama_get_embeddings")
    ((ctx (:pointer (:struct llama-context))))
  :result-type (:pointer :float))

(fli:define-foreign-function (llama-get-embeddings-ith "llama_get_embeddings_ith")
    ((ctx (:pointer (:struct llama-context)))
     (i :int))
  :result-type (:pointer :float))

(fli:define-foreign-function (llama-get-embeddings-seq "llama_get_embeddings_seq")
    ((ctx (:pointer (:struct llama-context)))
     (seq-id llama-seq-id))
  :result-type (:pointer :float))

;; Vocab

(fli:define-foreign-function (llama-token-get-text "llama_token_get_text")
    ((model (:pointer (:struct llama-model)))
     (token llama-token))
  :result-type (:pointer (:const :char)))

(fli:define-foreign-function (llama-token-get-score "llama_token_get_score")
    ((model (:pointer (:struct llama-model)))
     (token llama-token))
  :result-type :float)

(fli:define-foreign-function (llama-token-get-attr "llama_token_get_attr")
    ((model (:pointer (:struct llama-model)))
     (token llama-token))
  :result-type :int)

(fli:define-foreign-function (llama-token-is-eog "llama_token_is_eog")
    ((model (:pointer (:struct llama-model)))
     (token llama-token))
  :result-type (:boolean :byte))

(fli:define-foreign-function (llama-token-is-control "llama_token_is_control")
    ((model (:pointer (:struct llama-model)))
     (token llama-token))
  :result-type (:boolean :byte))

(fli:define-foreign-function (llama-token-bos "llama_token_bos")
    ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

(fli:define-foreign-function (llama-token-eos "llama_token_eos")
    ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

(fli:define-foreign-function (llama-token-cls "llama_token_cls")
    ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

(fli:define-foreign-function (llama-token-sep "llama_token_sep")
    ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

(fli:define-foreign-function (llama-token-nl "llama_token_nl")
    ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

(fli:define-foreign-function (llama-token-pad "llama_token_pad")
    ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

(fli:define-foreign-function (llama-add-bos-token "llama_add_bos_token")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

(fli:define-foreign-function (llama-add-eos-token "llama_add_eos_token")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

(fli:define-foreign-function (llama-token-prefix "llama_token_prefix")
    ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

(fli:define-foreign-function (llama-token-middle "llama_token_middle")
    ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

(fli:define-foreign-function (llama-token-suffix "llama_token_suffix")
    ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

(fli:define-foreign-function (llama-token-eot "llama_token_eot")
    ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

;; Tokenization

(fli:define-foreign-function (llama-tokenize "llama_tokenize")
    ((model (:pointer (:struct llama-model)))
     (text (:reference-pass (:ef-mb-string :external-format :utf-8)))
     (text-len :int)     
     (tokens (:pointer llama-token))
     (n-max-tokens :int)
     (add-special (:boolean :byte))
     (parse-special (:boolean :byte)))
  :result-type :int)

(fli:define-foreign-function (llama-token-to-piece "llama_token_to_piece")
    ((model (:pointer (:struct llama-model)))
     (token llama-token)
     (buf (:pointer :char))
     (length :int)
     (lstrip :int)
     (special (:boolean :byte)))
  :result-type :int)

(fli:define-foreign-function (llama-detokenize "llama_detokenize")
    ((model (:pointer (:struct llama-model)))
     (tokens (:pointer llama-token))
     (n-tokens :int)
     (text (:reference-pass (:ef-mb-string :external-format :utf-8)))
     (text-len-max :int)     
     (remove-special (:boolean :byte))
     (unparse-special (:boolean :byte)))
  :result-type :int)

;; Chat templates

(fli:define-foreign-function (llama-chat-apply-template "llama_chat_apply_template")
    ((model (:pointer (:struct llama-model)))
     (templ (:pointer :char))     
     (chat (:pointer (:struct llama-chat-message)))
     (n-msg :int)
     (bad-ass (:boolean :byte))
     (buf (:pointer :char))     
     (length :int))
  :result-type :int)

;; Sampling API

;; llama_sampler_context_t
;; llama_sampler_i
;; llama_sampler

(fli:define-foreign-function (llama-sampler-name "llama_sampler_name")
    ((smpl (:pointer (:struct llama-sampler))))
  :result-type (:pointer (:const :char)))

(fli:define-foreign-function (llama-sampler-accept "llama_sampler_accept")
    ((smpl (:pointer (:struct llama-sampler)))
     (token llama-token))
  :result-type :void)

(fli:define-foreign-function (llama-sampler-apply "llama_sampler_apply")
    ((smpl (:pointer (:struct llama-sampler)))
     (cur-p (:pointer llama-token-data-array)))
  :result-type :void)

(fli:define-foreign-function (llama-sampler-reset "llama_sampler_reset")
    ((smpl (:pointer (:struct llama-sampler))))
  :result-type :void)

(fli:define-foreign-function (llama-sampler-clone "llama_sampler_clone")
    ((smpl (:pointer (:struct llama-sampler))))
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-free "llama_sampler_free")
    ((smpl (:pointer (:struct llama-sampler))))
  :result-type :void)

(fli:define-foreign-function (llama-sampler-chain-init "llama_sampler_chain_init")
    ((params (:struct llama-sampler-chain-params)))
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-chain-add "llama_sampler_chain_add")
    ((chain (:pointer (:struct llama-sampler)))
     (smpl (:pointer (:struct llama-sampler))))
  :result-type :void)

(fli:define-foreign-function (llama-sampler-chain-get "llama_sampler_chain_get")
    ((chain (:pointer (:struct llama-sampler)))
     (i :int))
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-chain-n "llama_sampler_chain_n")
    ((chain (:pointer (:struct llama-sampler))))
  :result-type :int)

(fli:define-foreign-function (llama-sampler-chain-remove "llama_sampler_chain_remove")
    ((chain (:pointer (:struct llama-sampler)))
     (i :int))
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-greedy "llama_sampler_init_greedy")
    ()
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-dist "llama_sampler_init_dist")
    ((seed :unsigned-int))
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-softmax "llama_sampler_init_softmax")
    ()
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-top-k "llama_sampler_init_top_k")
    ((k :int))
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-top-p "llama_sampler_init_top_p")
    ((p :float)
     (min-keep :unsigned-long))  
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-min-p "llama_sampler_init_min_p")
    ((p :float)
     (min-keep :unsigned-long))  
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-tail-free "llama_sampler_init_tail_free")
    ((z :float)
     (min-keep :unsigned-long))  
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-typical "llama_sampler_init_typical")
    ((p :float)
     (min-keep :unsigned-long))  
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-temp "llama_sampler_init_temp")
    ((temp :float))
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-temp-ext "llama_sampler_init_temp_ext")
    ((temp :float)
     (delta :float)
     (exponent :float))  
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-mirostat "llama_sampler_init_mirostat")
    ((n-vocab :int)
     (seed :unsigned-int)
     (tau :float)
     (eta :float)
     (m :int))
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-mirostat-v2 "llama_sampler_init_mirostat_v2")
    ((seed :unsigned-int)
     (tau :float)
     (eta :float))
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-grammar "llama_sampler_init_grammar")
    ((model (:pointer (:struct llama-model)))
     (grammar-str (:reference-pass :ef-mb-string))
     (grammar-root (:reference-pass :ef-mb-string)))
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-penalties "llama_sampler_init_penalties")
    ((n-vocab :int)
     (special-eos-id llama-token)
     (linefeed-id llama-token)
     (penalty-last-n :int)
     (penalty-repeat :float)
     (penalty-freq :float)
     (penalty-present :float)
     (penalize-nl (:boolean :byte))
     (ignore-eos (:boolean :byte)))
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-init-logit-bias "llama_sampler_init_logit_bias")
    ((n-vocab :int)
     (n-logit-bias :int)
     (logit-bias (:pointer (:struct llama-logit-bias))))
  :result-type (:pointer (:struct llama-sampler)))

(fli:define-foreign-function (llama-sampler-get-seed "llama_sampler_get_seed")
    ((smpl (:pointer (:struct llama-sampler))))
  :result-type :unsigned-int)

(fli:define-foreign-function (llama-sampler-sample "llama_sampler_sample")
    ((smpl (:pointer (:struct llama-sampler)))
     (ctx (:pointer (:struct llama-context)))
     (idx :int))
  :result-type llama-token)

;; Model split

;; llama_split_path
;; llama_split_prefix

(fli:define-foreign-function (llama-print-system-info "llama_print_system_info")
  nil
  :result-type (:pointer (:const :char)))

(fli:define-foreign-function (llama-log-set "llama_log_set")
    ((log-callback (:pointer :void))
     (user-data (:pointer :void)))
  :result-type :void)

;; Performance utils

;; llama_perf_context_data
;; llama_perf_sample_data
;; llama_perf_context

(fli:define-foreign-function (llama-perf-context-print "llama_perf_context_print")
  ((ctx (:pointer (:struct llama-context))))
  :result-type :void)

(fli:define-foreign-function (llama-perf-context-reset "llama_perf_context_reset")
  ((ctx (:pointer (:struct llama-context))))
  :result-type :void)

;; llama_perf_sampler

(fli:define-foreign-function (llama-perf-sampler-print "llama_perf_sampler_print")
  ((chain (:pointer (:struct llama-sampler))))
  :result-type :void)

(fli:define-foreign-function (llama-perf-sampler-reset "llama_perf_sampler_reset")
  ((chain (:pointer (:struct llama-sampler))))
  :result-type :void)

;; llama_perf_dump_yaml
