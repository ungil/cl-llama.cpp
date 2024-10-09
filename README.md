# cl-llama.cpp

cl-llama.cpp provides a Common Lisp wrapper for [llama.cpp](https://github.com/ggerganov/llama.cpp).

Go into the llama.cpp sub-directory and run _make libllama.so_ to compile the llama.cpp library.

Put (or symlink) the code in quicklisp/local-projects to be able to load the package via (ql:quickload :llama).

Tested on MacOS only (Apple Silicon and Intel). Uses the implementation-specific FFI for Lispworks 8 and Allegro CL 11 (not working properly at the moment) and CFFI for other lisp implementations (SBCL crashes on Apple Silicon).

perplexity.lisp/embedding.lisp/simple.lisp are direct ports of the perplexity/embedding/simple examples in llama.cpp.
