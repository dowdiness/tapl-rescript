all: a.out

hello.ll: hello.c
	clang -emit-llvm -S -O0 hello.c

hello.s: hello.ll
	llc hello.ll

a.out: hello.s
	clang hello.s
