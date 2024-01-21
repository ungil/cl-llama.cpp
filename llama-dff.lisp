(in-package :llama)

(fli:define-c-struct (llama-model (:foreign-name "llama_model")))

(fli:define-c-struct (llama-context (:foreign-name "llama_context")))

(fli:define-c-typedef (llama-pos (:foreign-name "llama_pos")) :int)

(fli:define-c-typedef (llama-token (:foreign-name "llama_token")) :int)

(fli:define-c-typedef (llama-seq-id (:foreign-name "llama_seq_id")) :int)

;; llama_log_level

;; llama_vocab_type

;; llama_token_type

;; llama_ftype

;; llama_rope_scaling_type

;; llama_split_mode

(fli:define-c-struct (llama-token-data (:foreign-name "llama_token_data"))
    (id llama-token)
  (logit :float)
  (p :float))

(fli:define-c-typedef (llama-token-data (:foreign-name "llama_token_data"))
    (:struct llama-token-data))

(fli:define-c-struct (llama-token-data-array (:foreign-name "llama_token_data_array"))
    (data (:pointer llama-token-data))
  (size :unsigned-long)
  (sorted (:boolean :byte)))

(fli:define-c-typedef (llama-token-data-array (:foreign-name "llama_token_data_array"))
    (:struct llama-token-data-array))

(fli:define-c-typedef (llama-progress-callback (:foreign-name "llama_progress_callback"))
    (:pointer (:function (:float (:pointer :void)) :bool)))

;; (fli:define-c-struct (llama-batch (:foreign-name "llama_batch"))

;; llama_model_kv_override_type

;; (fli:define-c-struct (llama-batch (:foreign-name "llama_model_kv_override"))

(fli:define-c-struct (llama-model-params (:foreign-name "llama_model_params"))
    (n-gpu-layers :int)
  (split-mode :int)
  (main-gpu :int)
  (tensor-split (:pointer :float))
  (progress-callback llama-progress-callback)
  (progress-callback-user-data (:pointer :void))
  (kv-overrides (:pointer :void))
  (vocab-only (:boolean :byte))
  (use-mmap (:boolean :byte))
  (use-mlock (:boolean :byte)))

(fli:define-c-struct (llama-context-params (:foreign-name "llama_context_params"))
    (seed :int)
  (n-ctx :int)
  (n-batch :int)
  (n-threads :int)
  (n-threads-batch :int)
  (rope-scaling-type :int8)
  (rope-freq-base :float)
  (rope-freq-scale :float)
  (yarn-ext-factor :float)
  (yarn-attn-factor :float)
  (yarn-beta-fast :float)
  (yarn-beta-slow :float)
  (yarn-orig-ctx :int)
  (cb-eval (:pointer :void))
  (cb-eval-user-data (:pointer :void))
  (type-k :int)
  (type-v :int)
  (mul-mat (:boolean :byte))
  (logits-all (:boolean :byte))
  (embedding (:boolean :byte))
  (offload-kqv (:boolean :byte)))

;; llama_log_callback

;; llama_model_quantize_params

(fli:define-c-struct (llama-grammar (:foreign-name "llama_grammar")))

;; llama_gretype

(fli:define-c-struct (llama-grammar-element (:foreign-name "llama_grammar_element"))
    (type :int) ;; enum llama_gretype
  (value :unsigned-long))

(fli:define-c-struct (llama-timings (:foreign-name "llama_timings"))
    (t-start-ms :double)
  (t-end-ms :double)
  (t-load-ms :double)
  (t-sample-ms :double)
  (t-p-eval-ms :double)
  (t-eval-ms :double)
  (n-sample :int)
  (n-p-eval :int)
  (n-eval :int))

(fli:define-foreign-function (llama-model-default-params "llama_model_default_params")
  nil
  :result-type (:struct llama-model-params))

(fli:define-foreign-function (llama-context-default-params "llama_context_default_params")  
  nil
  :result-type (:struct llama-context-params))

;; llama_model_quantize_default_params

(fli:define-foreign-function (llama-backend-init "llama_backend_init")
    ((numa (:boolean :byte)))
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

;; ;; DEPRECATED
;; (fli:define-foreign-function (llama-init-from-file "llama_init_from_file")
;;     ((path-model (:reference-pass :ef-mb-string))
;;      (params (:struct llama-context-params)))
;;   :result-type (:pointer (:struct llama-context)))

(fli:define-foreign-function (llama-free "llama_free")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :void)

(fli:define-foreign-function (llama-time-us "llama_time_us")
  nil
  :result-type :unsigned-long)

(fli:define-foreign-function (llama-max-devices "llama_max_devices")
  nil
  :result-type :int)

(fli:define-foreign-function (llama-mmap-supported "llama_mmap_supported")
  nil
  :result-type (:boolean :byte))

(fli:define-foreign-function (llama-mlock-supported "llama_mlock_supported")
  nil
  :result-type (:boolean :byte))

(fli:define-foreign-function (llama-get-model "llama_get_model")
     ((ctx (:pointer (:struct llama-context))))
  :result-type (:pointer (:struct llama-model)))

(fli:define-foreign-function (llama-n-ctx "llama_n_ctx")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-n-batch "llama_n_batch")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-vocab-type "llama_vocab_type")
    ((model (:pointer (:struct llama-model))))
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

;; llama_rope_freq_scale_train

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

;; llama_model_quantize

;; ;; DEPRECATED
;; (fli:define-foreign-function (llama-apply-lora-from-file "llama_apply_lora_from_file")
;;     ((ctx (:pointer (:struct llama-context)))
;;      (path-lora (:reference-pass :ef-mb-string))
;;      (scale :float)
;;      (path-base-model (:reference-pass :ef-mb-string))
;;      (n-threads :int))
;;   :result-type :int)

(fli:define-foreign-function (llama-model-apply-lora-from-file "llama_model_apply_lora_from_file")
    ((model (:pointer (:struct llama-model)))
     (path-lora (:reference-pass :ef-mb-string))
     (path-base-model (:reference-pass :ef-mb-string))
     (n-threads :int))
  :result-type :int)

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

(fli:define-foreign-function (llama-get-state-size "llama_get_state_size")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :unsigned-long)

;; llama_copy_state_data

;; llama_set_state_data

;; llama_load_session_file

;; llama_save_session_file

;; DEPRECATED - use llama_decode
(fli:define-foreign-function (llama-eval "llama_eval")
    ((ctx (:pointer (:struct llama-context)))
     (tokens (:pointer (:const llama-token)))
     (n-tokens :int)
     (n-past :int)
     (n-threads :int))
  :result-type :int)

;; DEPRECATED - use llama_decode
(fli:define-foreign-function (llama-eval-emdb "llama_eval_embd")
    ((ctx (:pointer (:struct llama-context)))
     (embd (:pointer :float))
     (n-tokens :int)
     (n-past :int)
     (n-threads :int))
  :result-type :int)

;; llama_batch_get_one

;; llama_batch_init

;; llama_batch_free

;; llama_decode

;; llama_set_n_threads

(fli:define-foreign-function (llama-get-logits "llama_get_logits")
    ((ctx (:pointer (:struct llama-context))))
  :result-type (:pointer :float))

;; llama_get_logits_ith

(fli:define-foreign-function (llama-get-embeddings "llama_get_embeddings")
    ((ctx (:pointer (:struct llama-context))))
  :result-type (:pointer :float))

(fli:define-foreign-function (llama-token-get-text "llama_token_get_text")
   ((model (:pointer (:struct llama-model)))
    (token llama-token))
  :result-type (:pointer (:const :char)))

(fli:define-foreign-function (llama-token-get-score "llama_token_get_score")
   ((model (:pointer (:struct llama-model)))
    (token llama-token))
  :result-type :float)

(fli:define-foreign-function (llama-token-get-type "llama_token_get_type")
   ((model (:pointer (:struct llama-model)))
    (token llama-token))
  :result-type :int)

(fli:define-foreign-function (llama-token-bos "llama_token_bos")
    ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

(fli:define-foreign-function (llama-token-eos "llama_token_eos")
     ((model (:pointer (:struct llama-model))))
  :result-type llama-token)

(fli:define-foreign-function (llama-token-nl "llama_token_nl")
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

(fli:define-foreign-function (llama-tokenize "llama_tokenize")
    ((model (:pointer (:struct llama-model)))
     (text (:reference-pass (:ef-mb-string :external-format :utf-8)))
     (text-len :int)     
     (tokens (:pointer llama-token))
     (n-max-tokens :int)
     (add-bos (:boolean :byte))
     (special (:boolean :byte)))
  :result-type :int)

(fli:define-foreign-function (llama-token-to-piece "llama_token_to_piece")
    ((model (:pointer (:struct llama-model)))
     (token llama-token)
     (buf (:pointer :char))
     (length :int))
  :result-type :int)

(fli:define-foreign-function (llama-grammar-init "llama_grammar_init")
    ((grammar (:pointer (:pointer (:struct llama-grammar-element))))
     (n-rules :unsigned-long)
     (start-rule-index :unsigned-long))
  :result-type (:pointer (:struct llama-grammar)))

(fli:define-foreign-function (llama-grammar-free "llama_grammar_free")
    ((grammar (:pointer (:struct llama-grammar))))
  :result-type :void)

(fli:define-foreign-function (llama-grammar-copy "llama_grammar_copy")
    ((grammar (:pointer (:struct llama-grammar))))
  :result-type (:pointer (:struct llama-grammar)))

(fli:define-foreign-function (llama-set-rng-seed "llama_set_rng_seed")
    ((ctx (:pointer (:struct llama-context)))
     (seed :int))
  :result-type :void)

(fli:define-foreign-function (llama-sample-repetition-penalties "llama_sample_repetition_penalties")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (last-tokens (:pointer (:const llama-token)))
     (penalty-last-n :unsigned-long)
     (penalty-repeat :float)
     (penalty-freq :float)
     (penalty-present :float))
  :result-type :void)

(fli:define-foreign-function (llama-sample-apply-guidance "llama_sample_apply_guidance")
    ((ctx (:pointer (:struct llama-context)))
     (logits (:pointer :float))
     (logits-guidance (:pointer :float))
     (scale :float))
  :result-type :void)

;; deprecated - use llama-sample-apply-guidance
(fli:define-foreign-function (llama-sample-classifier-free-guidance "llama_sample_classifier_free_guidance")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (guidance-ctx (:pointer (:struct llama-context)))
     (scale :float))
  :result-type :void)

(fli:define-foreign-function (llama-sample-softmax "llama_sample_softmax")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array)))
  :result-type :void)

(fli:define-foreign-function (llama-sample-top-k "llama_sample_top_k")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (k :int)
     (min-keep :unsigned-long))
  :result-type :void)

(fli:define-foreign-function (llama-sample-top-p "llama_sample_top_p")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (p :float)
     (min-keep :unsigned-long))
  :result-type :void)

(fli:define-foreign-function (llama-sample-min-p "llama_sample_min_p")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (p :float)
     (min-keep :unsigned-long))
  :result-type :void)

(fli:define-foreign-function (llama-sample-tail-free "llama_sample_tail_free")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (z :float)
     (min-keep :unsigned-long))
  :result-type :void)

(fli:define-foreign-function (llama-sample-typical "llama_sample_typical")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (p :float)
     (min-keep :unsigned-long))
  :result-type :void)

(fli:define-foreign-function (llama-sample-temp "llama_sample_temp")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (temp :float))
  :result-type :void)

;; ;; DEPRECATED
;; (fli:define-foreign-function (llama-sample-temperature "llama_sample_temperature")
;;     ((ctx (:pointer (:struct llama-context)))
;;      (candidates (:pointer llama-token-data-array))
;;      (temp :float))
;;   :result-type :void)

(fli:define-foreign-function (llama-sample-grammar "llama_sample_grammar")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (grammar (:pointer (:struct llama-grammar))))
  :result-type :void)

(fli:define-foreign-function (llama-sample-token-mirostat "llama_sample_token_mirostat")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (tau :float)
     (eta :float)
     (m :int)
     (mu (:pointer :float)))
  :result-type llama-token)

(fli:define-foreign-function (llama-sample-token-mirostat-v2 "llama_sample_token_mirostat_v2")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (tau :float)
     (eta :float)
     (mu (:pointer :float)))
  :result-type llama-token)

(fli:define-foreign-function (llama-sample-token-greedy "llama_sample_token_greedy")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array)))
  :result-type llama-token)

(fli:define-foreign-function (llama-sample-token "llama_sample_token")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array)))
  :result-type llama-token)

(fli:define-foreign-function (llama-grammar-accept-token "llama_grammar_accept_token")
    ((ctx (:pointer (:struct llama-context)))
     (grammar (:pointer (:struct llama-grammar)))
     (token llama-token))
  :result-type :void)

;; llama_beam_view

;; llama_beam_state

;; llama_beam_search_callback_fn_t

;; llama_beam_search

(fli:define-foreign-function (llama-get-timings "llama_get_timings")
    ((ctx (:pointer (:struct llama-context))))
  :result-type (:struct llama-timings))

(fli:define-foreign-function (llama-print-timings "llama_print_timings")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :void)

(fli:define-foreign-function (llama-reset-timings "llama_reset_timings")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :void)

(fli:define-foreign-function (llama-print-system-info "llama_print_system_info")
  nil
  :result-type (:pointer (:const :char)))

;; (fli:define-foreign-function (llama-log-set "llama_log_set")
;;     ((log-callback (:struct llama-log-callback))
;;      (user-data (:pointer :void)))
;;   :result-type :void)

;; llama_dump_timing_info_yaml
