(in-package :llama)

(defun simple (&key (prompt "Hello my name is") (model *model*) (n-predict 32) (metal *metal*)
		 (print-while-generating t) (print-timings t))
  "See llama.cpp/examples/simple/simple.cpp"
  #+sbcl (sb-int:set-floating-point-modes :traps nil)
  (llama-backend-init)
  (llama-numa-init *numa*)
  (let* ((mdl (make-instance 'mdl :file model :params (model-parameters :n-gpu-layers (if metal 1 0))))
	 (ctx (make-instance 'ctx :model mdl :params (context-parameters)))
	 (chain (make-instance 'chain))
	 (tokens (make-instance 'tokens :size n-predict))
	 (batch (make-instance 'batch :n-tokens-max n-predict)))
    (llama-sampler-chain-add (ptr chain) (llama-sampler-init-greedy))
    (assert (<= n-predict (n-ctx ctx)))
    (tokenize (model ctx) tokens prompt :add-special t)
    (when print-while-generating (format t "~{~A~}" (list-tokens tokens :context ctx :limit nil)))
    (loop for token in (list-tokens tokens)
	  for pos from 0
	  for logits = (= pos (1- (n tokens)))
	  do (add batch token pos logits))
    (assert (decode ctx batch))
    (prog1	
	(loop for n-cur from (n tokens) below n-predict
	      collect (let* ((new-token-id (llama-sampler-sample (ptr chain) (ptr ctx) -1))
			     (new-token (get-token ctx new-token-id)))
			(when (token-is-eog mdl new-token-id) (return))
			(when print-while-generating (format t "~A" new-token))
			(clear batch)
			(add batch new-token-id n-cur t)
			(assert (decode ctx batch))
			new-token) into output
	      finally (return (format nil "~{~A~}" (append (list-tokens tokens :context ctx :limit nil) output))))
      (when print-timings
	;;(print-timings chain) ;;TBD
	(print-timings ctx))
      (llama-backend-free))))

;; ./llama-simple -m models/Meta-Llama-3-8B.Q4_1.gguf -ngl 1
;; Hello my name is {name} and I am {age} years old. I am currently studying {course} at {university}. My hobbies include {

;; ./llama-simple -m models/Meta-Llama-3-8B.Q4_1.gguf -ngl 0
;; Hello my name is {name} and I am {age} years old. I am currently studying {course} at {university}. My hobbies include {

;; (simple :model "~/llama.cpp/models/Meta-Llama-3-8B.Q4_1.gguf")
;; "Hello my name is {name} and I am {age} years old. I am currently studying {course} at {university}. My hobbies include {"

;; ./llama-simple -m models/Meta-Llama-3-8B.Q8_0.gguf
;; Hello my name is {name} and I am {age} years old. I am a {profession} and I have been working in the field for {

;; (simple :model "~/llama.cpp/models/Meta-Llama-3-8B.Q8_0.gguf")
;; "Hello my name is {name} and I am {age} years old. I am a {profession} and I have been working in the field for {"
