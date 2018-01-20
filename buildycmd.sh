cd ../ycmd && git submodule update --init --recursive
python3 ../ycmd/build.py --clang-completer --go-completer --js-completer --system-libclang --system-boost --enable-coverage
