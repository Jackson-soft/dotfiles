cd ../ycmd && git submodule update --init --recursive
python3 ../ycmd/build.py --clang-completer --gocode-completer --tern-completer --system-libclang
