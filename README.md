# cl-llama.cpp

cl-llama.cpp provides a Common Lisp wrapper for [llama.cpp](https://github.com/ggerganov/llama.cpp).

Go into the llama.cpp sub-directory and run _make libllama.so_ to compile the llama.cpp library.

Put (or symlink) the code in quicklisp/local-projects to be able to load the package via (ql:quickload :llama).

Tested mostly on MacOS only (Apple Silicon and Intel). Uses the implementation-specific FFI for Lispworks 8 (not working on Intel right now) and Allegro CL 11 (not working properly due to a bug in the implementation) and CFFI for other lisp implementations (SBCL crashes on Apple Silicon).

On Linux _make GGML_CUDA=1 libllama.so_ works but if the GPU has not enough memory the full test suite may not run sucessfully (even though no test fails on its own). I also had to set _LD_LIBRARY_PATH_ pointing to the llama.cpp subdirectory mentioned before which contains the libraries libllama.so and liblggml.so.

perplexity.lisp/embedding.lisp/simple.lisp are direct ports of the perplexity/embedding/simple examples in llama.cpp.

