# cl-llama.cpp

cl-llama.cpp provides a Common Lisp wrapper for [llama.cpp](https://github.com/ggerganov/llama.cpp).

Go into the llama.cpp sub-directory and run _make libllama.so_ to compile the llama.cpp library.

Put (or symlink) the code in quicklisp/local-projects to be able to load the package via (ql:quickload :llama).

Tested on MacOS only (Apple Silicon and Intel). Uses the implementation-specific FFI for Lispworks 8 and Allegro CL 11 (the latter works on Apple Silicon but not on Intel) and CFFI for other lisp implementations (works on SBCL, ClozureCL, ECL).

perplexity.lisp/embedding.lisp/llama.lisp are direct ports of the perplexity/embedding/main examples in llama.cpp. The latter implements only part of the functionality - interactive sessions in particular are not supported.
