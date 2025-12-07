(in-package :llama)

(defun simple (&key (prompt "Hello my name is") (model *model*) (n-predict 32) (metal *metal*)
		 (print-while-generating t) (print-timings t))
  "See llama.cpp/examples/simple/simple.cpp"
  #+sbcl (sb-int:set-floating-point-modes :traps nil)
  (llama-backend-init)
  (llama-numa-init *numa*)
  (let* ((mdl (make-instance 'mdl :file model :params (model-parameters :n-gpu-layers (if metal 999 0))))
	 (ctx (make-instance 'ctx :model mdl :params (context-parameters)))
	 (chain (make-instance 'chain))
	 (tokens (make-instance 'tokens :size (n-ctx ctx)))
	 (batch (make-instance 'batch :n-tokens-max (n-ctx ctx))))
    (llama-sampler-chain-add (ptr chain) (llama-sampler-init-greedy))
    (tokenize (model ctx) tokens prompt :add-special t)
    (assert (<= (+ (n tokens) n-predict) (n-ctx ctx)))
    (when print-while-generating (format t "~{~A~}" (list-tokens tokens :context ctx :limit nil)))
    (loop for token in (list-tokens tokens)
	  for pos from 0
	  for logits = (= pos (1- (n tokens)))
	  do (add batch token pos logits))
    (assert (decode ctx batch))
    (prog1	
	(loop for n-cur from (n tokens) repeat n-predict
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

;; ./llama-simple -m ~/llama.cpp/models/SmolLM-135M.Q8_0.gguf -n 40
;; Hello my name is <NAME> and I am a student at the University of California, Santa Barbara. I am currently studying Computer Science and I am interested in learning more about the world of computers and how they work.

;; ./llama-simple -m ~/llama.cpp/models/SmolLM-135M.Q8_0.gguf -n 40 -ngl 0
;; Hello my name is Alex and I am a student at the University of California, Santa Barbara. I am currently studying in the Department of Computer Science and Engineering. I am also a member of the Computer Science and Engineering Research

;; (simple :model "~/llama.cpp/models/SmolLM-135M.Q8_0.gguf" :n-predict 40 :print-while-generating nil :metal t)
;;"Hello my name is <NAME> and I am a student at the University of California, Santa Barbara. I am currently studying Computer Science and I am interested in learning more about the world of computers and how they work."

;; (simple :model "~/llama.cpp/models/SmolLM-135M.Q8_0.gguf" :n-predict 40 :print-while-generating nil :metal nil)
;;"Hello my name is Alex and I am a student at the University of California, Santa Barbara. I am currently studying in the Department of Computer Science and Engineering. I am also a member of the Computer Science and Engineering Research"
