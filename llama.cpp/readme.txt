Copy the llama.cpp libraries here using something like the code below, assuming this file is in ~/lisp/llama/llama.cpp/

This works on Apple Silicon, on other platforms you may need to pass -DGGML_CUDA=ON or -DGGML_METAL=OFF for example.

----

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
