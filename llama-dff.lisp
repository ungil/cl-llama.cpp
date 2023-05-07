
#| DATE           : 7 May 2023 
 | USER           : ungil 
 | PROCESSED FILE : /Users/ungil/lisp/llama/llama.cpp/llama.h
 |#

(in-package "LLAMA")

(fli:define-c-typedef (size-t (:foreign-name "size_t"))
                      (:unsigned :long))

(fli:define-c-typedef (uint8-t (:foreign-name "uint8_t"))
                      (:unsigned :char))

;;; Derived from file : "/Users/ungil/lisp/llama/llama.cpp/llama.h"

(fli:define-c-struct (llama-context
                      (:foreign-name "llama_context")
                      (:forward-reference-p t)))
(fli:define-c-typedef (llama-token (:foreign-name "llama_token")) :int)
(fli:define-c-struct (llama-token-data
                      (:foreign-name "llama_token_data"))
                     (id llama-token)
                     (logit :float)
                     (p :float))
(fli:define-c-typedef (llama-token-data
                       (:foreign-name "llama_token_data"))
                      (:struct llama-token-data))
(fli:define-c-struct (llama-token-data-array
                      (:foreign-name "llama_token_data_array"))
                     (data (:pointer llama-token-data))
                     (size size-t)
                     (sorted (:boolean :standard)))
(fli:define-c-typedef (llama-token-data-array
                       (:foreign-name "llama_token_data_array"))
                      (:struct llama-token-data-array))
(fli:define-c-typedef (llama-progress-callback
                       (:foreign-name "llama_progress_callback"))
                      (:pointer
                       (:function (:float (:pointer :void)) :void)))
(fli:define-c-struct (llama-context-params
                      (:foreign-name "llama_context_params"))
                     (n-ctx :int)
                     (n-parts :int)
                     (seed :int)
                     (f16-kv (:boolean :standard))
                     (logits-all (:boolean :standard))
                     (vocab-only (:boolean :standard))
                     (use-mmap (:boolean :standard))
                     (use-mlock (:boolean :standard))
                     (embedding (:boolean :standard))
                     (progress-callback llama-progress-callback)
                     (progress-callback-user-data (:pointer :void)))
(fli:define-c-enum (llama-ftype (:foreign-name "llama_ftype"))
                   (llama-ftype-all-f32 0)
                   (llama-ftype-mostly-f16 1)
                   (llama-ftype-mostly-q4-0 2)
                   (llama-ftype-mostly-q4-1 3)
                   (llama-ftype-mostly-q4-1-some-f16 4)
                   (llama-ftype-mostly-q4-2 5)
                   (llama-ftype-mostly-q8-0 7)
                   (llama-ftype-mostly-q5-0 8)
                   (llama-ftype-mostly-q5-1 9))
(fli:define-foreign-function (llama-context-default-params "llama_context_default_params"
                                                           :source)
                             nil
                             :result-type
                             (:struct llama-context-params)
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-mmap-supported "llama_mmap_supported"
                                                   :source)
                             nil
                             :result-type
                             (:boolean :standard)
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-mlock-supported "llama_mlock_supported"
                                                    :source)
                             nil
                             :result-type
                             (:boolean :standard)
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-init-from-file "llama_init_from_file"
                                                   :source)
                             ((path-model (:reference-pass :ef-mb-string))
                              (params (:struct llama-context-params)))
                             :result-type
                             (:pointer (:struct llama-context))
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-free "llama_free" :source)
                             ((ctx (:pointer (:struct llama-context))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-model-quantize "llama_model_quantize"
                                                   :source)
                             ((fname-inp (:reference-pass :ef-mb-string))
                              (fname-out (:reference-pass :ef-mb-string))
                              (ftype (:enum llama-ftype))
                              (nthread :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-apply-lora-from-file "llama_apply_lora_from_file"
                                                         :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (path-lora (:reference-pass :ef-mb-string))
                              (path-base-model
                               (:reference-pass :ef-mb-string))
                              (n-threads :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-get-kv-cache-token-count "llama_get_kv_cache_token_count"
                                                             :source)
                             ((ctx
                               (:pointer
                                (:const (:struct llama-context)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-set-rng-seed "llama_set_rng_seed"
                                                 :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (seed :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-get-state-size "llama_get_state_size"
                                                   :source)
                             ((ctx
                               (:pointer
                                (:const (:struct llama-context)))))
                             :result-type
                             size-t
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-copy-state-data "llama_copy_state_data"
                                                    :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (dest (:pointer uint8-t)))
                             :result-type
                             size-t
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-set-state-data "llama_set_state_data"
                                                   :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (src (:pointer (:const uint8-t))))
                             :result-type
                             size-t
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-load-session-file "llama_load_session_file"
                                                      :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (path-session (:reference-pass :ef-mb-string))
                              (tokens-out (:pointer llama-token))
                              (n-token-capacity size-t)
                              (n-token-count-out (:pointer size-t)))
                             :result-type
                             (:boolean :standard)
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-save-session-file "llama_save_session_file"
                                                      :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (path-session (:reference-pass :ef-mb-string))
                              (tokens (:pointer (:const llama-token)))
                              (n-token-count size-t))
                             :result-type
                             (:boolean :standard)
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-eval "llama_eval" :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (tokens (:pointer (:const llama-token)))
                              (n-tokens :int)
                              (n-past :int)
                              (n-threads :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-tokenize "llama_tokenize" :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (text (:reference-pass (:ef-mb-string :external-format :utf-8)))
                              (tokens (:pointer llama-token))
                              (n-max-tokens :int)
                              (add-bos (:boolean :standard)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-n-vocab "llama_n_vocab" :source)
                             ((ctx
                               (:pointer
                                (:const (:struct llama-context)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-n-ctx "llama_n_ctx" :source)
                             ((ctx
                               (:pointer
                                (:const (:struct llama-context)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-n-embd "llama_n_embd" :source)
                             ((ctx
                               (:pointer
                                (:const (:struct llama-context)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-get-logits "llama_get_logits"
                                               :source)
                             ((ctx (:pointer (:struct llama-context))))
                             :result-type
                             (:pointer :float)
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-get-embeddings "llama_get_embeddings"
                                                   :source)
                             ((ctx (:pointer (:struct llama-context))))
                             :result-type
                             (:pointer :float)
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-token-to-str "llama_token_to_str"
                                                 :source)
                             ((ctx
                               (:pointer
                                (:const (:struct llama-context))))
                              (token llama-token))
                             :result-type
                             (:pointer (:const :char))
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-token-bos "llama_token_bos"
                                              :source)
                             nil
                             :result-type
                             llama-token
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-token-eos "llama_token_eos"
                                              :source)
                             nil
                             :result-type
                             llama-token
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-token-nl "llama_token_nl" :source)
                             nil
                             :result-type
                             llama-token
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-sample-repetition-penalty "llama_sample_repetition_penalty"
                                                              :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (candidates
                               (:pointer llama-token-data-array))
                              (last-tokens
                               (:pointer (:const llama-token)))
                              (last-tokens-size size-t)
                              (penalty :float))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-sample-frequency-and-presence-penalties "llama_sample_frequency_and_presence_penalties"
                                                                            :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (candidates
                               (:pointer llama-token-data-array))
                              (last-tokens
                               (:pointer (:const llama-token)))
                              (last-tokens-size size-t)
                              (alpha-frequency :float)
                              (alpha-presence :float))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-sample-softmax "llama_sample_softmax"
                                                   :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (candidates
                               (:pointer llama-token-data-array)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-sample-top-k "llama_sample_top_k"
                                                 :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (candidates
                               (:pointer llama-token-data-array))
                              (k :int)
                              (min-keep size-t))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-sample-top-p "llama_sample_top_p"
                                                 :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (candidates
                               (:pointer llama-token-data-array))
                              (p :float)
                              (min-keep size-t))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-sample-tail-free "llama_sample_tail_free"
                                                     :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (candidates
                               (:pointer llama-token-data-array))
                              (z :float)
                              (min-keep size-t))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-sample-typical "llama_sample_typical"
                                                   :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (candidates
                               (:pointer llama-token-data-array))
                              (p :float)
                              (min-keep size-t))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-sample-temperature "llama_sample_temperature"
                                                       :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (candidates
                               (:pointer llama-token-data-array))
                              (temp :float))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-sample-token-mirostat "llama_sample_token_mirostat"
                                                          :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (candidates
                               (:pointer llama-token-data-array))
                              (tau :float)
                              (eta :float)
                              (m :int)
                              (mu (:pointer :float)))
                             :result-type
                             llama-token
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-sample-token-mirostat-v2 "llama_sample_token_mirostat_v2"
                                                             :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (candidates
                               (:pointer llama-token-data-array))
                              (tau :float)
                              (eta :float)
                              (mu (:pointer :float)))
                             :result-type
                             llama-token
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-sample-token-greedy "llama_sample_token_greedy"
                                                        :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (candidates
                               (:pointer llama-token-data-array)))
                             :result-type
                             llama-token
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-sample-token "llama_sample_token"
                                                 :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (candidates
                               (:pointer llama-token-data-array)))
                             :result-type
                             llama-token
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-print-timings "llama_print_timings"
                                                  :source)
                             ((ctx (:pointer (:struct llama-context))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-reset-timings "llama_reset_timings"
                                                  :source)
                             ((ctx (:pointer (:struct llama-context))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-print-system-info "llama_print_system_info"
                                                      :source)
                             nil
                             :result-type
                             (:pointer (:const :char))
                             :language
                             :ansi-c)
