(in-package :llama)

(defvar *libggml* (asdf:system-relative-pathname "llama" "llama.cpp/libggml.so"))

(defvar *libllama* (asdf:system-relative-pathname "llama" "llama.cpp/libllama.so"))

(unless (and (probe-file *libggml*) (probe-file *libllama*))
  (error "to build the library execute 'make clean libllama.so' in the llama.cpp subdirectory"))

#-(or lispworks allegro)
(progn
  (cffi:load-foreign-library *libggml*)
  (cffi:load-foreign-library *libllama*)
  (format t "~%~%   Foreign libraries ~A and ~A loaded~%~%~%" *libggml* *libllama*))

;; #-(or lispworks allegro) (load (asdf:system-relative-pathname "llama" "llama-cffi.lisp"))

#+lispworks
(progn
  (fli:register-module "libggml.so" :file-name *libggml*)  
  (fli:register-module "libllama.so" :file-name *libllama*)
  (format cl:t "~%~%   Foreign libraries ~A and ~A loaded~%~%~%" *libggml* *libllama*))

#+lispworks (assert (not (eq lw:*default-character-element-type* 'base-char)))

#|
(require "foreign-parser")

(defun pretty-name (name) (string-upcase (substitute #\- #\_ name)))

(foreign-parser:process-foreign-file (asdf:system-relative-pathname "llama" "llama.cpp/llama.h")
:dff (asdf:system-relative-pathname "llama" "llama-dff-original.lisp")
:package "LLAMA" :case-sensitive '(:user-routine pretty-name))

;; make a copy of llama-dff-original.lisp as llama-dff.lisp
;; keep only the definitions of size-t (derived from stddef.h)
;; and uint8-t (derived from _uint8_t.h) and the whole
;; last section derived from llama.h changing occurrences of
;;   (:pointer (:const :char))
;; in function arguments only (without modifying result types) to
;;   (:reference-pass :ef-mb-string)
;; except for argument text in llama-tokenize that should have the form
;;   (:reference-pass (:ef-mb-string :external-format :utf-8))
|#

;; #+lispworks (load (asdf:system-relative-pathname "llama" "llama-dff.lisp"))

#+allegro
(progn
  (load *libggml* :foreign t) ;;"libggml.so" :file-name library)
  (load *libllama* :foreign t) ;;"libllama.so" :file-name library)  
  (format cl:t "~%~%   Foreign libraries ~A and ~A loaded~%~%~%" *libggml* *libllama*))

;; #+allegro (load (asdf:system-relative-pathname "llama" "llama-ff.lisp"))
