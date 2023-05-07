
#| DATE           : 7 May 2023 
 | USER           : ungil 
 | PROCESSED FILE : /Users/ungil/lisp/llama/llama.cpp/llama.h
 |#

(in-package "LLAMA")

;;; Derived from file : "/Library/Developer/CommandLineTools/usr/lib/clang/14.0.3/include/stddef.h"

(fli:define-c-typedef (ptrdiff-t (:foreign-name "ptrdiff_t")) :long)
(fli:define-c-typedef (size-t (:foreign-name "size_t"))
                      (:unsigned :long))
(fli:define-c-typedef (wchar-t (:foreign-name "wchar_t")) :int)

;;; Derived from file : "/Library/Developer/CommandLineTools/usr/lib/clang/14.0.3/include/__stddef_max_align_t.h"

(fli:define-c-typedef (max-align-t (:foreign-name "max_align_t"))
                      :long-double)

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_int8_t.h"

(fli:define-c-typedef (int8-t (:foreign-name "int8_t")) (:signed :char))

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_int16_t.h"

(fli:define-c-typedef (int16-t (:foreign-name "int16_t")) :short)

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_int32_t.h"

(fli:define-c-typedef (int32-t (:foreign-name "int32_t")) :int)

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_int64_t.h"

(fli:define-c-typedef (int64-t (:foreign-name "int64_t")) :long-long)

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uint8_t.h"

(fli:define-c-typedef (uint8-t (:foreign-name "uint8_t"))
                      (:unsigned :char))

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uint16_t.h"

(fli:define-c-typedef (uint16-t (:foreign-name "uint16_t"))
                      (:unsigned :short))

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uint32_t.h"

(fli:define-c-typedef (uint32-t (:foreign-name "uint32_t"))
                      (:unsigned :int))

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uint64_t.h"

(fli:define-c-typedef (uint64-t (:foreign-name "uint64_t"))
                      (:unsigned :long-long))

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/stdint.h"

(fli:define-c-typedef (int-least8-t (:foreign-name "int_least8_t"))
                      int8-t)
(fli:define-c-typedef (int-least16-t (:foreign-name "int_least16_t"))
                      int16-t)
(fli:define-c-typedef (int-least32-t (:foreign-name "int_least32_t"))
                      int32-t)
(fli:define-c-typedef (int-least64-t (:foreign-name "int_least64_t"))
                      int64-t)
(fli:define-c-typedef (uint-least8-t (:foreign-name "uint_least8_t"))
                      uint8-t)
(fli:define-c-typedef (uint-least16-t (:foreign-name "uint_least16_t"))
                      uint16-t)
(fli:define-c-typedef (uint-least32-t (:foreign-name "uint_least32_t"))
                      uint32-t)
(fli:define-c-typedef (uint-least64-t (:foreign-name "uint_least64_t"))
                      uint64-t)
(fli:define-c-typedef (int-fast8-t (:foreign-name "int_fast8_t"))
                      int8-t)
(fli:define-c-typedef (int-fast16-t (:foreign-name "int_fast16_t"))
                      int16-t)
(fli:define-c-typedef (int-fast32-t (:foreign-name "int_fast32_t"))
                      int32-t)
(fli:define-c-typedef (int-fast64-t (:foreign-name "int_fast64_t"))
                      int64-t)
(fli:define-c-typedef (uint-fast8-t (:foreign-name "uint_fast8_t"))
                      uint8-t)
(fli:define-c-typedef (uint-fast16-t (:foreign-name "uint_fast16_t"))
                      uint16-t)
(fli:define-c-typedef (uint-fast32-t (:foreign-name "uint_fast32_t"))
                      uint32-t)
(fli:define-c-typedef (uint-fast64-t (:foreign-name "uint_fast64_t"))
                      uint64-t)

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/arm/_types.h"

(fli:define-c-typedef (--int8-t (:foreign-name "__int8_t"))
                      (:signed :char))
(fli:define-c-typedef (--uint8-t (:foreign-name "__uint8_t"))
                      (:unsigned :char))
(fli:define-c-typedef (--int16-t (:foreign-name "__int16_t")) :short)
(fli:define-c-typedef (--uint16-t (:foreign-name "__uint16_t"))
                      (:unsigned :short))
(fli:define-c-typedef (--int32-t (:foreign-name "__int32_t")) :int)
(fli:define-c-typedef (--uint32-t (:foreign-name "__uint32_t"))
                      (:unsigned :int))
(fli:define-c-typedef (--int64-t (:foreign-name "__int64_t"))
                      :long-long)
(fli:define-c-typedef (--uint64-t (:foreign-name "__uint64_t"))
                      (:unsigned :long-long))
(fli:define-c-typedef (--darwin-intptr-t
                       (:foreign-name "__darwin_intptr_t"))
                      :long)
(fli:define-c-typedef (--darwin-natural-t
                       (:foreign-name "__darwin_natural_t"))
                      (:unsigned :int))
(fli:define-c-typedef (--darwin-ct-rune-t
                       (:foreign-name "__darwin_ct_rune_t"))
                      :int)
(fli:define-c-typedef (--mbstate-t (:foreign-name "__mbstate_t"))
                      (:union
                       (--mbstate8 (:c-array :char 128))
                       (-mbstatel :long-long)))
(fli:define-c-typedef (--darwin-mbstate-t
                       (:foreign-name "__darwin_mbstate_t"))
                      --mbstate-t)
(fli:define-c-typedef (--darwin-ptrdiff-t
                       (:foreign-name "__darwin_ptrdiff_t"))
                      :long)
(fli:define-c-typedef (--darwin-size-t
                       (:foreign-name "__darwin_size_t"))
                      (:unsigned :long))
(fli:define-c-typedef (--darwin-va-list
                       (:foreign-name "__darwin_va_list"))
                      (:pointer :void))
(fli:define-c-typedef (--darwin-wchar-t
                       (:foreign-name "__darwin_wchar_t"))
                      :int)
(fli:define-c-typedef (--darwin-rune-t
                       (:foreign-name "__darwin_rune_t"))
                      --darwin-wchar-t)
(fli:define-c-typedef (--darwin-wint-t
                       (:foreign-name "__darwin_wint_t"))
                      :int)
(fli:define-c-typedef (--darwin-clock-t
                       (:foreign-name "__darwin_clock_t"))
                      (:unsigned :long))
(fli:define-c-typedef (--darwin-socklen-t
                       (:foreign-name "__darwin_socklen_t"))
                      --uint32-t)
(fli:define-c-typedef (--darwin-ssize-t
                       (:foreign-name "__darwin_ssize_t"))
                      :long)
(fli:define-c-typedef (--darwin-time-t
                       (:foreign-name "__darwin_time_t"))
                      :long)

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types.h"

(fli:define-c-typedef (--darwin-blkcnt-t
                       (:foreign-name "__darwin_blkcnt_t"))
                      --int64-t)
(fli:define-c-typedef (--darwin-blksize-t
                       (:foreign-name "__darwin_blksize_t"))
                      --int32-t)
(fli:define-c-typedef (--darwin-dev-t (:foreign-name "__darwin_dev_t"))
                      --int32-t)
(fli:define-c-typedef (--darwin-fsblkcnt-t
                       (:foreign-name "__darwin_fsblkcnt_t"))
                      (:unsigned :int))
(fli:define-c-typedef (--darwin-fsfilcnt-t
                       (:foreign-name "__darwin_fsfilcnt_t"))
                      (:unsigned :int))
(fli:define-c-typedef (--darwin-gid-t (:foreign-name "__darwin_gid_t"))
                      --uint32-t)
(fli:define-c-typedef (--darwin-id-t (:foreign-name "__darwin_id_t"))
                      --uint32-t)
(fli:define-c-typedef (--darwin-ino64-t
                       (:foreign-name "__darwin_ino64_t"))
                      --uint64-t)
(fli:define-c-typedef (--darwin-ino-t (:foreign-name "__darwin_ino_t"))
                      --darwin-ino64-t)
(fli:define-c-typedef (--darwin-mach-port-name-t
                       (:foreign-name "__darwin_mach_port_name_t"))
                      --darwin-natural-t)
(fli:define-c-typedef (--darwin-mach-port-t
                       (:foreign-name "__darwin_mach_port_t"))
                      --darwin-mach-port-name-t)
(fli:define-c-typedef (--darwin-mode-t
                       (:foreign-name "__darwin_mode_t"))
                      --uint16-t)
(fli:define-c-typedef (--darwin-off-t (:foreign-name "__darwin_off_t"))
                      --int64-t)
(fli:define-c-typedef (--darwin-pid-t (:foreign-name "__darwin_pid_t"))
                      --int32-t)
(fli:define-c-typedef (--darwin-sigset-t
                       (:foreign-name "__darwin_sigset_t"))
                      --uint32-t)
(fli:define-c-typedef (--darwin-suseconds-t
                       (:foreign-name "__darwin_suseconds_t"))
                      --int32-t)
(fli:define-c-typedef (--darwin-uid-t (:foreign-name "__darwin_uid_t"))
                      --uint32-t)
(fli:define-c-typedef (--darwin-useconds-t
                       (:foreign-name "__darwin_useconds_t"))
                      --uint32-t)
(fli:define-c-typedef (--darwin-uuid-t
                       (:foreign-name "__darwin_uuid_t"))
                      (:c-array (:unsigned :char) 16))
(fli:define-c-typedef (--darwin-uuid-string-t
                       (:foreign-name "__darwin_uuid_string_t"))
                      (:c-array :char 37))

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_types.h"

(fli:define-c-struct (--darwin-pthread-handler-rec
                      (:foreign-name "__darwin_pthread_handler_rec")
                      (:forward-reference-p t)))
(fli:define-c-struct (--darwin-pthread-handler-rec
                      (:foreign-name "__darwin_pthread_handler_rec"))
                     (--routine
                      (:pointer (:function ((:pointer :void)) :void)))
                     (--arg (:pointer :void))
                     (--next
                      (:pointer
                       (:struct --darwin-pthread-handler-rec))))
(fli:define-c-struct (-opaque-pthread-attr-t
                      (:foreign-name "_opaque_pthread_attr_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 56)))
(fli:define-c-struct (-opaque-pthread-cond-t
                      (:foreign-name "_opaque_pthread_cond_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 40)))
(fli:define-c-struct (-opaque-pthread-condattr-t
                      (:foreign-name "_opaque_pthread_condattr_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 8)))
(fli:define-c-struct (-opaque-pthread-mutex-t
                      (:foreign-name "_opaque_pthread_mutex_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 56)))
(fli:define-c-struct (-opaque-pthread-mutexattr-t
                      (:foreign-name "_opaque_pthread_mutexattr_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 8)))
(fli:define-c-struct (-opaque-pthread-once-t
                      (:foreign-name "_opaque_pthread_once_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 8)))
(fli:define-c-struct (-opaque-pthread-rwlock-t
                      (:foreign-name "_opaque_pthread_rwlock_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 192)))
(fli:define-c-struct (-opaque-pthread-rwlockattr-t
                      (:foreign-name "_opaque_pthread_rwlockattr_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 16)))
(fli:define-c-struct (-opaque-pthread-t
                      (:foreign-name "_opaque_pthread_t"))
                     (--sig :long)
                     (--cleanup-stack
                      (:pointer
                       (:struct --darwin-pthread-handler-rec)))
                     (--opaque (:c-array :char 8176)))
(fli:define-c-typedef (--darwin-pthread-attr-t
                       (:foreign-name "__darwin_pthread_attr_t"))
                      (:struct -opaque-pthread-attr-t))
(fli:define-c-typedef (--darwin-pthread-cond-t
                       (:foreign-name "__darwin_pthread_cond_t"))
                      (:struct -opaque-pthread-cond-t))
(fli:define-c-typedef (--darwin-pthread-condattr-t
                       (:foreign-name "__darwin_pthread_condattr_t"))
                      (:struct -opaque-pthread-condattr-t))
(fli:define-c-typedef (--darwin-pthread-key-t
                       (:foreign-name "__darwin_pthread_key_t"))
                      (:unsigned :long))
(fli:define-c-typedef (--darwin-pthread-mutex-t
                       (:foreign-name "__darwin_pthread_mutex_t"))
                      (:struct -opaque-pthread-mutex-t))
(fli:define-c-typedef (--darwin-pthread-mutexattr-t
                       (:foreign-name "__darwin_pthread_mutexattr_t"))
                      (:struct -opaque-pthread-mutexattr-t))
(fli:define-c-typedef (--darwin-pthread-once-t
                       (:foreign-name "__darwin_pthread_once_t"))
                      (:struct -opaque-pthread-once-t))
(fli:define-c-typedef (--darwin-pthread-rwlock-t
                       (:foreign-name "__darwin_pthread_rwlock_t"))
                      (:struct -opaque-pthread-rwlock-t))
(fli:define-c-typedef (--darwin-pthread-rwlockattr-t
                       (:foreign-name "__darwin_pthread_rwlockattr_t"))
                      (:struct -opaque-pthread-rwlockattr-t))
(fli:define-c-typedef (--darwin-pthread-t
                       (:foreign-name "__darwin_pthread_t"))
                      (:pointer (:struct -opaque-pthread-t)))

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int8_t.h"

(fli:define-c-typedef (u-int8-t (:foreign-name "u_int8_t"))
                      (:unsigned :char))

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int16_t.h"

(fli:define-c-typedef (u-int16-t (:foreign-name "u_int16_t"))
                      (:unsigned :short))

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int32_t.h"

(fli:define-c-typedef (u-int32-t (:foreign-name "u_int32_t"))
                      (:unsigned :int))

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int64_t.h"

(fli:define-c-typedef (u-int64-t (:foreign-name "u_int64_t"))
                      (:unsigned :long-long))

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/arm/types.h"

(fli:define-c-typedef (register-t (:foreign-name "register_t")) int64-t)

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uintptr_t.h"

(fli:define-c-typedef (uintptr-t (:foreign-name "uintptr_t"))
                      (:unsigned :long))

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/arm/types.h"

(fli:define-c-typedef (user-addr-t (:foreign-name "user_addr_t"))
                      u-int64-t)
(fli:define-c-typedef (user-size-t (:foreign-name "user_size_t"))
                      u-int64-t)
(fli:define-c-typedef (user-ssize-t (:foreign-name "user_ssize_t"))
                      int64-t)
(fli:define-c-typedef (user-long-t (:foreign-name "user_long_t"))
                      int64-t)
(fli:define-c-typedef (user-ulong-t (:foreign-name "user_ulong_t"))
                      u-int64-t)
(fli:define-c-typedef (user-time-t (:foreign-name "user_time_t"))
                      int64-t)
(fli:define-c-typedef (user-off-t (:foreign-name "user_off_t")) int64-t)
(fli:define-c-typedef (syscall-arg-t (:foreign-name "syscall_arg_t"))
                      u-int64-t)

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_intptr_t.h"

(fli:define-c-typedef (intptr-t (:foreign-name "intptr_t"))
                      --darwin-intptr-t)

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_intmax_t.h"

(fli:define-c-typedef (intmax-t (:foreign-name "intmax_t")) :long)

;;; Derived from file : "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/_types/_uintmax_t.h"

(fli:define-c-typedef (uintmax-t (:foreign-name "uintmax_t"))
                      (:unsigned :long))

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
                             ((path-model (:pointer (:const :char)))
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
                             ((fname-inp (:pointer (:const :char)))
                              (fname-out (:pointer (:const :char)))
                              (ftype (:enum llama-ftype))
                              (nthread :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(fli:define-foreign-function (llama-apply-lora-from-file "llama_apply_lora_from_file"
                                                         :source)
                             ((ctx (:pointer (:struct llama-context)))
                              (path-lora (:pointer (:const :char)))
                              (path-base-model
                               (:pointer (:const :char)))
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
                              (path-session (:pointer (:const :char)))
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
                              (path-session (:pointer (:const :char)))
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
                              (text (:pointer (:const :char)))
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
