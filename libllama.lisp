(in-package :llama)

(defvar *libraries*
  (append (directory (merge-pathnames (asdf:system-relative-pathname "llama" "llama.cpp/")
				      "*.dylib"))
	  (directory (merge-pathnames (asdf:system-relative-pathname "llama" "llama.cpp/")
				      "*.dll"))
	  (directory (merge-pathnames (asdf:system-relative-pathname "llama" "llama.cpp/")
				      "*.so"))))

(cl:terpri)
(loop for library in *libraries* do
      #-(or lispworks allegro)
	(cffi:load-foreign-library library)
      #+lispworks
       (fli:register-module (pathname-name library) :file-name library)
      #+allegro
       (load library :foreign t)
       (format cl:t "~&   Foreign library ~A loaded~&" library))
(cl:terpri)

#+lispworks (assert (not (eq lw:*default-character-element-type* 'base-char)))

#+sbcl (sb-int:set-floating-point-modes :traps nil)

#|
cd /tmp
git clone -b "b7314" https://github.com/ggerganov/llama.cpp.git
cd llama.cpp
mkdir buildlib
cd buildlib
cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DLLAMA_BUILD_COMMON=OFF ..
make
cp */*dylib */*/*dylib */*/*/*dylib ~/lisp/llama/llama.cpp/
cp */*so */*/*so */*/*/*so ~/lisp/llama/llama.cpp/
cp */*dll */*/*dll */*/*/*dll ~/lisp/llama/llama.cpp/
|#
