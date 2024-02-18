(in-package :llama)

(defvar *metal* (stringp (ignore-errors
			  #+(or ARM ARM64)
			  (uiop:run-program (format nil "otool -L ~A | grep Metal" *lib*)
					    :force-shell t :output :string))))

;; (defparameter *numa* t)

(defparameter *model* (truename "~/llama.cpp/models/7B/ggml-model-f16.gguf"))

(defparameter *max-ctx* 2048)

(defparameter *threads* 4)

(defparameter *predict* -1)

(defparameter *n-ctx* 512)

(defparameter *n-batch* 512)

(defparameter *n-keep* 0)

(defparameter *top-k* 40)

(defparameter *top-p* 0.95)

(defparameter *tfs-z* 1.0)

(defparameter *typical-p* 1.0)

(defparameter *temp* 0.8)

(defparameter *repeat-penalty* 1.1)

(defparameter *repeat-last-n* 64) ;; 0 = disable penalty, -1 = context size)

(defparameter *frequency-penalty* 0.0)

(defparameter *presence-penalty* 0.0)

(defparameter *penalize-newlines* t)

(defparameter *mirostat* 0) ;; 0 = disabled, 1 = mirostat, 2 = mirostat 2.0

(defparameter *mirostat-tau* 5.0) ;; target entropy

(defparameter *mirostat-eta* 0.1) ;; learning rate

