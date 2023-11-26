(in-package :llama)

(ff:def-foreign-type llama-model (* :void))

(ff:def-foreign-type llama-context (* :void))

(ff:def-foreign-type llama-pos :int)

(ff:def-foreign-type llama-token :int)

(ff:def-foreign-type llama-seq-id :int)

;; llama_log_level

;; llama_vocab_type

;; llama_token_type

;; llama_ftype

;; llama_rope_scaling_type

(ff:def-foreign-type llama-token-data
    (:struct (id llama-token)
	     (logit :float)
	     (p :float)))

(ff:def-foreign-type llama-token-data-array
    (:struct (data (* llama-token-data))
	     (size :unsigned-long)
	     (sorted :char boolean)))

(ff:def-foreign-type llama-progress-callback (* :void))

;; (ff:def-foreign-type llama-batch

(ff:def-foreign-type llama-model-params
    (:struct (n-gpu-layers :int)
	     (main-gpu :int)
	     (tensor-split (* :float))
	     (progress-callback (* :void))
	     (progress-callback-user-data (* :void))
	     (vocab-only :char boolean)
	     (use-mmap :char boolean)
	     (use-mlock :char boolean)))

(ff:def-foreign-type llama-context-params
    (:struct (seed :int)
	     (n-ctx :int)
	     (n-batch :int)
	     (n-threads :int)
	     (n-threads-batch :int)
	     (rope-scaling-type :char)
	     (rope-freq-base :float)
	     (rope-freq-scale :float)
	     (yarn-ext-factor :float)
	     (yarn-attn-factor :float)
	     (yarn-beta-fast :float)
	     (yarn-beta-slow :float)
	     (yarn-orig-ctx :int)
	     (mul-mat :char boolean)
	     (f16-kv :char boolean)
	     (logits-all :char boolean)
	     (embedding :char boolean)))

;; llama_log_callback

;; llama_model_quantize_params

(ff:def-foreign-type llama-grammar (* :void))

;; llama_gretype

(ff:def-foreign-type llama-grammar-element
    (:struct (type :int) ;; enum llama_gretype
	     (value :unsigned-long)))

(ff:def-foreign-type llama-timings
    (:struct (t-start-ms :double)
	     (t-end-ms :double)
	     (t-load-ms :double)
	     (t-sample-ms :double)
	     (t-p-eval-ms :double)
	     (t-eval-ms :double)
	     (n-sample :int)
	     (n-p-eval :int)
	     (n-eval :int)))

(ff:def-foreign-call (llama-model-default-params "llama_model_default_params")
    (:void)
  :returning llama-model-params
  :pass-structs-by-value t)

(ff:def-foreign-call (llama-context-default-params "llama_context_default_params")
    (:void)
  :returning llama-context-params
  :pass-structs-by-value t)

;; llama_model_quantize_default_params

(ff:def-foreign-call (llama-backend-init "llama_backend_init")
    ((numa :char boolean))
  :returning :void)

(ff:def-foreign-call (llama-backend-free "llama_backend_free")
    (:void)
  :returning :void)

(ff:def-foreign-call (llama-load-model-from-file "llama_load_model_from_file")
    ((path-model (* :char))
     (params (* llama-model-params)))
  :returning llama-model)

(ff:def-foreign-call (llama-free-model "llama_free_model")
    ((model (* llama-model)))
  :returning :void)

(ff:def-foreign-call (llama-new-context-with-model "llama_new_context_with_model")
    ((model (* llama-model))
     (params (* llama-context-params)))
  :returning llama-context)

;; ;; DEPRECATED
;; (ff:def-foreign-call (llama-init-from-file "llama_init_from_file")
;;     ((path-model (* :char))
;;      (params (* llama-context-params)))
;;   :returning llama-context)

(ff:def-foreign-call (llama-free "llama_free")
    ((ctx (* llama-context)))
  :returning :void)

(ff:def-foreign-call (llama-time-us "llama_time_us")
    (:void)
  :returning :unsigned-long)

(ff:def-foreign-call (llama-max-devices "llama_max_devices")
    (:void)
  :returning :int)

(ff:def-foreign-call (llama-mmap-supported "llama_mmap_supported")
    (:void)
  :returning :boolean)

(ff:def-foreign-call (llama-mlock-supported "llama_mlock_supported")
    (:void)
  :returning :boolean)

(ff:def-foreign-call (llama-get-model "llama_get_model")
    ((ctx (* llama-context)))
  :returning llama-model)

(ff:def-foreign-call (llama-n-ctx "llama_n_ctx")
    ((ctx (* llama-context)))
  :returning :int)

(ff:def-foreign-call (llama-vocab-type "llama_vocab_type")
    ((ctx (* llama-model)))
  :returning :int)

(ff:def-foreign-call (llama-n-vocab "llama_n_vocab")
    ((model (* llama-model)))
  :returning :int)

(ff:def-foreign-call (llama-n-ctx-train "llama_n_ctx_train")
    ((model (* llama-model)))
  :returning :int)

(ff:def-foreign-call (llama-n-embd "llama_n_embd")
    ((model (* llama-model)))
  :returning :int)

;; llama_rope_freq_scale_train

;; llama_model_meta_val_str

;; llama_model_meta_count

;; llama_model_meta_key_by_index

;; llama_model_meta_val_str_by_index

(ff:def-foreign-call (llama-model-desc "llama_model_desc")
    ((model (* llama-model))
     (buf (* :char))
     (buf-size :unsigned-long))
  :returning :int)

(ff:def-foreign-call (llama-model-size "llama_model_size")
    ((model (* llama-model)))
  :returning :unsigned-long)

(ff:def-foreign-call (llama-model-n-params "llama_model_n_params")
    ((model (* llama-model)))
  :returning :unsigned-long)

;; llama_get_model_tensor

;; llama_model_quantize

;; ;; DEPRECATED
;; (ff:def-foreign-call (llama-apply-lora-from-file "llama_apply_lora_from_file")
;;     ((ctx (* llama-context))
;;      (path-lora (* :char))
;;      (scale :float)
;;      (path-base-model (* :char))
;;      (n-threads :int))
;;   :returning :int)

(ff:def-foreign-call (llama-model-apply-lora-from-file "llama_model_apply_lora_from_file")
    ((model (* llama-model))
     (path-lora (* :char))
     (path-base-model (* :char))
     (n-threads :int))
  :returning :int)

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

(ff:def-foreign-call (llama-get-state-size "llama_get_state_size")
    ((ctx (* llama-context)))
  :returning :unsigned-long)

;; llama_copy_state_data

;; llama_set_state_data

;; llama_load_session_file

;; llama_save_session_file

;; DEPRECATED - use llama_decode
(ff:def-foreign-call (llama-eval "llama_eval")
    ((ctx (* llama-context))
     (tokens (* llama-token))
     (n-tokens :int)
     (n-past :int)
     (n-threads :int))
  :returning :int)

;; DEPRECATED - use llama_decode
(ff:def-foreign-call (llama-eval-embd "llama_eval_embd")
    ((ctx (* llama-context))
     (embd (* :float))
     (n-tokens :int)
     (n-past :int)
     (n-threads :int))
  :returning :int)

;; llama_batch_get_one

;; llama_batch_init

;; llama_batch_free

;; llama_decode

;; llama_set_n_threads

(ff:def-foreign-call (llama-get-logits "llama_get_logits")
    ((ctx (* llama-context)))
  :returning ((* :float)))

;; llama_get_logits_ith

(ff:def-foreign-call (llama-get-embeddings "llama_get_embeddings")
    ((ctx (* llama-context)))
  :returning ((* :float)))

(ff:def-foreign-call (llama-token-get-text "llama_token_get_text")
    ((model(* llama-model))
     (token llama-token))
  :returning ((* :char)))

(ff:def-foreign-call (llama-token-get-score "llama_token_get_score")
    ((model(* llama-model))
     (token llama-token))
  :returning :float)

(ff:def-foreign-call (llama-token-get-type "llama_token_get_type")
    ((model (* llama-model))
     (token llama-token))
  :returning :int)

(ff:def-foreign-call (llama-token-bos "llama_token_bos")
    ((model (* llama-model)))
  :returning llama-token)

(ff:def-foreign-call (llama-token-eos "llama_token_eos")
    ((model (* llama-model)))
  :returning llama-token)

(ff:def-foreign-call (llama-token-nl "llama_token_nl")
    ((model (* llama-model)))
  :returning llama-token)

(ff:def-foreign-call (llama-add-bos-token "llama_add_bos_token")
    ((model (* llama-model)))
  :returning :int)

(ff:def-foreign-call (llama-add-eos-token "llama_add_eos_token")
    ((model (* llama-model)))
  :returning :int)

(ff:def-foreign-call (llama-token-prefix "llama_token_prefix")
    ((model (* llama-model)))
  :returning llama-token)

(ff:def-foreign-call (llama-token-middle "llama_token_middle")
    ((model (* llama-model)))
  :returning llama-token)

(ff:def-foreign-call (llama-token-suffix "llama_token_suffix")
    ((model (* llama-model)))
  :returning llama-token)

(ff:def-foreign-call (llama-token-eot "llama_token_eot")
    ((model (* llama-model)))
  :returning llama-token)

(ff:def-foreign-call (llama-tokenize "llama_tokenize")
    ((model (* llama-model))
     (text (* :char))
     (text-len :int)     
     (tokens (* llama-token))
     (n-max-tokens :int)
     (add-bos :int boolean)
     (special :int boolean))
  :returning :int)

(ff:def-foreign-call (llama-token-to-piece "llama_token_to_piece")
    ((model (* llama-model))
     (token llama-token)
     (buf (* :char))
     (length :int))
  :returning :int)

(ff:def-foreign-call (llama-grammar-init "llama_grammar_init")
    ((grammar (* (* llama-grammar-element)))
     (n-rules :unsigned-long)
     (start-rule-index :unsigned-long))
  :returning ((* llama-grammar)))

(ff:def-foreign-call (llama-grammar-free "llama_grammar_free")
    ((grammar (* llama-grammar)))
  :returning :void)

(ff:def-foreign-call (llama-grammar-copy "llama_grammar_copy")
    ((grammar (* llama-grammar)))
  :returning ((* llama-grammar)))

(ff:def-foreign-call (llama-set-rng-seed "llama_set_rng_seed")
    ((ctx (* llama-context))
     (seed :int))
  :returning :void)

(ff:def-foreign-call (llama-sample-repetition-penalties "llama_sample_repetition_penalties")
    ((ctx (* llama-context))
     (candidates (* llama-token-data-array))
     (last_tokens (* llama-token))
     (penalty-last-n :unsigned-long)
     (penalty-repeat :float)
     (penalty-freq :float)
     (penalty-present :float))
  :returning :void)

(ff:def-foreign-call (llama-sample-classifier-free-guidance "llama_sample_classifier_free_guidance")
    ((ctx (* llama-context))
     (candidates (* llama-token-data-array))
     (guidance-ctx (* llama-context))
     (scale :float))
  :returning :void)

(ff:def-foreign-call (llama-sample-softmax "llama_sample_softmax")
  ((ctx (* llama-context))
   (candidates (* llama-token-data-array)))
  :returning :void)

(ff:def-foreign-call (llama-sample-top-k "llama_sample_top_k")
  ((ctx (* llama-context))
   (candidates (* llama-token-data-array))
   (k :int)
   (min-keep :unsigned-long))
  :returning :void)

(ff:def-foreign-call (llama-sample-top-p "llama_sample_top_p")
  ((ctx (* llama-context))
   (candidates (* llama-token-data-array))
   (p :float)
   (min-keep :unsigned-long))
  :returning :void)

(ff:def-foreign-call (llama-sample-min-p "llama_sample_min_p")
  ((ctx (* llama-context))
   (candidates (* llama-token-data-array))
   (p :float)
   (min-keep :unsigned-long))
  :returning :void)

(ff:def-foreign-call (llama-sample-tail-free "llama_sample_tail_free")
  ((ctx (* llama-context))
   (candidates (* llama-token-data-array))
   (z :float)
   (min-keep :unsigned-long))
  :returning :void)

(ff:def-foreign-call (llama-sample-typical "llama_sample_typical")
  ((ctx (* llama-context))
   (candidates (* llama-token-data-array))
   (p :float)
   (min-keep :unsigned-long))
  :returning :void)

(ff:def-foreign-call (llama-sample-temp "llama_sample_temp")
  ((ctx (* llama-context))
   (candidates (* llama-token-data-array))
   (temp :float))
  :returning :void)

;; ;; DEPRECATED
;; (ff:def-foreign-call (llama-sample-temperature "llama_sample_temperature")
;;   ((ctx (* llama-context))
;;    (candidates (* llama-token-data-array))
;;    (temp :float))
;;   :returning :void)

(ff:def-foreign-call (llama-sample-grammar "llama_sample_grammar")
    ((ctx (* llama-context))
     (candidates (* llama-token-data-array))
     (grammar (* llama-grammar)))
  :returning :void)

(ff:def-foreign-call (llama-sample-token-mirostat "llama_sample_token_mirostat")
  ((ctx (* llama-context))
   (candidates (* llama-token-data-array))
   (tau :float)
   (eta :float)
   (m :int)
   (mu (* :float)))
  :returning llama-token)

(ff:def-foreign-call (llama-sample-token-mirostat-v2 "llama_sample_token_mirostat_v2")
  ((ctx (* llama-context))
   (candidates (* llama-token-data-array))
   (tau :float)
   (eta :float)
   (mu (* :float)))
  :returning llama-token)

(ff:def-foreign-call (llama-sample-token-greedy "llama_sample_token_greedy")
  ((ctx (* llama-context))
   (candidates (* llama-token-data-array)))
  :returning llama-token)

(ff:def-foreign-call (llama-sample-token "llama_sample_token")
  ((ctx (* llama-context))
   (candidates (* llama-token-data-array)))
  :returning llama-token)

(ff:def-foreign-call (llama-grammar-accept-token "llama_grammar_accept_token")
    ((ctx (* llama-context))
     (grammar (* llama-grammar))
     (token llama-token))
  :returning :void)

;; llama_beam_view

;; llama_beam_state

;; llama_beam_search_callback_fn_t

;; llama_beam_search

(ff:def-foreign-call (llama-get-timings "llama_get_timings")
    ((ctx (* llama-context)))
  :returning llama-timings
  :pass-structs-by-value t)

(ff:def-foreign-call (llama-print-timings "llama_print_timings")
    ((ctx (* llama-context)))
  :returning :void)

(ff:def-foreign-call (llama-reset-timings "llama_reset_timings")
    ((ctx (* llama-context)))
  :returning :void)

(ff:def-foreign-call (llama-print-system-info "llama_print_system_info")
    (:void)
  :returning ((* :char)))

;; llama_log_set

;; llama_dump_timing_info_yaml
