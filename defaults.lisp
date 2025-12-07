(in-package :llama)

(defparameter *metal* (gpu-offload-supported))

(defparameter *numa* 0) ;; 0 disabled - 1 distribute - 2 isolate - 3 numactl - 4 mirror

;; https://huggingface.co/QuantFactory/SmolLM-135M-GGUF
(defvar *model* (truename "~/llama.cpp/models/SmolLM-135M.Q8_0.gguf"))

(defparameter *threads* 4)

(defparameter *predict* -1)

(defparameter *n-batch* 2048)

(defparameter *n-ubatch* 512)

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
