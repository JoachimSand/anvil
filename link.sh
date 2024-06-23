# llc -filetype=obj ./outputs/output.bc
clang++ outputs/output.bc main.cpp -o outputs/main
./outputs/main
