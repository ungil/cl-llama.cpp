(in-package :llama)

(fli:define-c-struct (llama-model (:foreign-name "llama_model")))

(fli:define-c-struct (llama-context (:foreign-name "llama_context")))

(fli:define-c-typedef (llama-token (:foreign-name "llama_token")) :int)

;; llama_log_level

;; llama_vocab_type

;; llama_token_type

;; llama_ftype

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
    (:pointer (:function (:float (:pointer :void)) :void)))

(fli:define-c-struct (llama-context-params (:foreign-name "llama_context_params"))
    (seed :int)
  (n-ctx :int)
  (n-batch :int)
  (n-gpu-layers :int)
  (main-gpu :int)
  (tensor-split (:pointer :float))
  (rope-freq-base :float)
  (rope-freq-scale :float)
  (progress-callback llama-progress-callback)
  (progress-callback-user-data (:pointer :void))
  (low-vram (:boolean :byte))
  (mul-mat (:boolean :byte))
  (f16-kv (:boolean :byte))
  (logits-all (:boolean :byte))
  (vocab-only (:boolean :byte))
  (use-mmap (:boolean :byte))
  (use-mlock (:boolean :byte))
  (embedding (:boolean :byte)))

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
     (params (:struct llama-context-params)))
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

(fli:define-foreign-function (llama-n-vocab "llama_n_vocab")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-n-ctx "llama_n_ctx")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-n-ctx-train "llama_n_ctx_train")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-n-embd "llama_n_embd")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-vocab-type "llama_vocab_type")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-model-n-vocab "llama_model_n_vocab")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

(fli:define-foreign-function (llama-model-n-ctx "llama_model_n_ctx")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

(fli:define-foreign-function (llama-model-n-ctx-train "llama_model_n_ctx_train")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

(fli:define-foreign-function (llama-model-n-embd "llama_model_n_embd")
    ((model (:pointer (:struct llama-model))))
  :result-type :int)

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

;; llama_model_quantize

;; ;; DEPRECATED
;; (fli:define-foreign-function (llama-apply-lora-from-file "llama_apply_lora_from_file")
;;     ((ctx (:pointer (:struct llama-context)))
;;      (path-lora (:reference-pass :ef-mb-string))
;;      (path-base-model (:reference-pass :ef-mb-string))
;;      (n-threads :int))
;;   :result-type :int)

(fli:define-foreign-function (llama-model-apply-lora-from-file "llama_model_apply_lora_from_file")
    ((model (:pointer (:struct llama-model)))
     (path-lora (:reference-pass :ef-mb-string))
     (path-base-model (:reference-pass :ef-mb-string))
     (n-threads :int))
  :result-type :int)

(fli:define-foreign-function (llama-get-kv-cache-token-count "llama_get_kv_cache_token_count")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :int)

(fli:define-foreign-function (llama-set-rng-seed "llama_set_rng_seed")
    ((ctx (:pointer (:struct llama-context)))
     (seed :int))
  :result-type :void)

(fli:define-foreign-function (llama-get-state-size "llama_get_state_size")
    ((ctx (:pointer (:struct llama-context))))
  :result-type :unsigned-long)

;; llama_copy_state_data

;; llama_set_state_data

;; llama_load_session_file

;; llama_save_session_file

(fli:define-foreign-function (llama-eval "llama_eval")
    ((ctx (:pointer (:struct llama-context)))
     (tokens (:pointer (:const llama-token)))
     (n-tokens :int)
     (n-past :int)
     (n-threads :int))
  :result-type :int)

(fli:define-foreign-function (llama-eval-emdb "llama_eval_embd")
    ((ctx (:pointer (:struct llama-context)))
     (embd (:pointer :float))
     (n-tokens :int)
     (n-past :int)
     (n-threads :int))
  :result-type :int)

;; // IMPORTANT: do not use for anything else other than debugging and testing!
;; (fli:define-foreign-function (llama-eval-export "llama_eval_export")
;;     ((ctx (:pointer (:struct llama-context)))
;;      (fname (:pointer (:const :char))))
;;   :result-type :int)

(fli:define-foreign-function (llama-get-logits "llama_get_logits")
    ((ctx (:pointer (:struct llama-context))))
  :result-type (:pointer :float))

(fli:define-foreign-function (llama-get-embeddings "llama_get_embeddings")
    ((ctx (:pointer (:struct llama-context))))
  :result-type (:pointer :float))

(fli:define-foreign-function (llama-token-get-text "llama_token_get_text")
   ((ctx (:pointer (:struct llama-context)))
    (token llama-token))
  :result-type (:pointer (:const :char)))

(fli:define-foreign-function (llama-token-get-score "llama_token_get_score")
   ((ctx (:pointer (:struct llama-context)))
    (token llama-token))
  :result-type :float)

(fli:define-foreign-function (llama-token-get-type "llama_token_get_type")
   ((ctx (:pointer (:struct llama-context)))
    (token llama-token))
  :result-type :int)

(fli:define-foreign-function (llama-token-bos "llama_token_bos")
    ((ctx (:pointer (:struct llama-context))))
  :result-type llama-token)

(fli:define-foreign-function (llama-token-eos "llama_token_eos")
     ((ctx (:pointer (:struct llama-context))))
  :result-type llama-token)

(fli:define-foreign-function (llama-token-nl "llama_token_nl")
    ((ctx (:pointer (:struct llama-context))))
  :result-type llama-token)

(fli:define-foreign-function (llama-tokenize "llama_tokenize")
    ((ctx (:pointer (:struct llama-context)))
     (text (:reference-pass (:ef-mb-string :external-format :utf-8)))
     (tokens (:pointer llama-token))
     (n-max-tokens :int)
     (add-bos (:boolean :byte)))
  :result-type :int)

(fli:define-foreign-function (llama-tokenize-with-model "llama_tokenize_with_model")
    ((model (:pointer (:struct llama-model)))
     (text (:reference-pass (:ef-mb-string :external-format :utf-8)))
     (tokens (:pointer llama-token))
     (n-max-tokens :int)
     (add-bos (:boolean :byte)))
  :result-type :int)

(fli:define-foreign-function (llama-token-to-piece "llama_token_to_piece")
    ((ctx (:pointer (:struct llama-context)))
     (token llama-token)
     (buf (:pointer :char))
     (length :int))
  :result-type :int)

(fli:define-foreign-function (llama-token-to-piece-with-model "llama_token_to_piece_with_model")
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

(fli:define-foreign-function (llama-sample-repetition-penalty "llama_sample_repetition_penalty")
    ((ctx (:pointer (:struct llama-context)))
     (candidates
      (:pointer llama-token-data-array))
     (last-tokens
      (:pointer (:const llama-token)))
     (last-tokens-size :unsigned-long)
     (penalty :float))
  :result-type :void)

(fli:define-foreign-function (llama-sample-frequency-and-presence-penalties "llama_sample_frequency_and_presence_penalties")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (last-tokens (:pointer (:const llama-token)))
     (last-tokens-size :unsigned-long)
     (alpha-frequency :float)
     (alpha-presence :float))
  :result-type :void)

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

(fli:define-foreign-function (llama-sample-temperature "llama_sample_temperature")
    ((ctx (:pointer (:struct llama-context)))
     (candidates (:pointer llama-token-data-array))
     (temp :float))
  :result-type :void)

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
