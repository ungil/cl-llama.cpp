# cl-llama.cpp

cl-llama.cpp provides a Common Lisp wrapper for [llama.cpp](https://github.com/ggerganov/llama.cpp).

You need to compile the llama.cpp library and put the resulting files in directory llama.cpp (see readme.txt there).

Put (or symlink) the code in quicklisp/local-projects to be able to load the package via (ql:quickload :llama).

This release works on MacOS/Apple Silicon. It uses the implementation-specific FFI for Lispworks 8 and Allegro CL and CFFI for other lisp implementations (SBCL doesn't work but ECL and ABCL do so the CFFI part seems to be fine).

perplexity.lisp/embedding.lisp/simple.lisp are direct ports of the perplexity/embedding/simple examples in llama.cpp.

The test suite runs (except on SBCL) successfully but then the lisp implementation crashes on exit. Note that this has never been used for anything beyond the test suite itself, it's just done as an exercise.
