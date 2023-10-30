SNAKE_EXT= garter
UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  NASM_FORMAT=elf64
  CLANG_FLAGS=-gdwarf-4 -mstackrealign -m64 -g -fstack-protector-all -Wstack-protector -fno-omit-frame-pointer 
else
ifeq ($(UNAME), Darwin)
  NASM_FORMAT=macho64
  CLANG_FLAGS=-mstackrealign -arch x86_64 -fno-pie -m64 -g -fstack-protector-all -Wstack-protector -fno-omit-frame-pointer
endif
endif

PKGS=ounit2,extlib,unix,str
BUILD=ocamlbuild -r -use-ocamlfind -cflag -annot -ocamlyacc 'ocamlyacc -v'

main: *.ml parser.mly lexer.mll
	$(BUILD) -package $(PKGS) main.native
	mv main.native main

# The test.native executable is not linked with the clang command using the CLANG_FLAGS variable directly in the Makefile.
# Instead, the Makefile builds the test.native executable using the $(BUILD) -package $(PKGS) test.native command, which invokes 
# the OCamlbuild build system to compile and link the test.ml source file, as well as any other relevant source files and libraries 
# specified in the test.mlbuild file. When OCamlbuild links the test.native executable, it automatically includes any relevant 
# flags specified in the environment, such as the CLANG_FLAGS variable. In this case, the CLANG_FLAGS variable is set to the 
# appropriate compilation and linking options for the platform being used, and so these options are included by OCamlbuild when it 
# links the test.native executable.
test: *.ml parser.mly lexer.mll main
	$(BUILD) -package $(PKGS) test.native
	mv test.native test

# The command 'make <filename.run> creates executable by providing .garter file as input to a pipeline process
# Run 'make <filename>.fdl' in comand line. 
# the -fno-pie flag is crucial because this fixed alignment issues that I have been getting
# which failed linking. 
# Also added a line to make main.s file. 
# Need to provide gc.o to the linking process. 
gc.o: gc.c gc.h
	clang $(CLANG_FLAGS) -c -o gc.o gc.c

%.run: %.$(SNAKE_EXT) main.c | main
	./main $< > $*.s && \
	nasm -f $(NASM_FORMAT) -o $*.o $*.s && \
	clang $(CLANG_FLAGS) -S -fno-pie -o main.s main.c && \
	clang $(CLANG_FLAGS) -c -o gc.o gc.c && \
	clang $(CLANG_FLAGS) -fno-pie -o $@ main.c $*.o gc.o
	rm $*.o

output/%.run: output/%.o main.c gc.c
	clang $(CLANG_FLAGS) -o $@ gc.c main.c $<

output/%.o: output/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: output/%.s
output/%.s: input/%.$(SNAKE_EXT) main
	./main $< > $@

output/do_pass/%.run: output/do_pass/%.o main.c gc.c
	clang $(CLANG_FLAGS) -o $@ gc.c main.c $<

output/do_pass/%.o: output/do_pass/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: output/do_pass/%.s
output/do_pass/%.s: input/do_pass/%.$(SNAKE_EXT) main
	./main $< > $@


output/dont_pass/%.run: output/dont_pass/%.o main.c gc.c
	clang -g $(CLANG_FLAGS) -o $@ gc.c main.c $<

output/dont_pass/%.o: output/dont_pass/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: output/dont_pass/%.s
output/dont_pass/%.s: input/dont_pass/%.$(SNAKE_EXT) main
	./main $< > $@


output/do_err/%.run: output/do_err/%.o main.c gc.c
	clang $(CLANG_FLAGS) -o $@ gc.c main.c $<

output/do_err/%.o: output/do_err/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: output/do_err/%.s
output/do_err/%.s: input/do_err/%.$(SNAKE_EXT) main
	./main $< > $@


output/dont_err/%.run: output/dont_err/%.o main.c gc.c
	clang -g $(CLANG_FLAGS) -o $@ gc.c main.c $<

output/dont_err/%.o: output/dont_err/%.s
	nasm -f $(NASM_FORMAT) -o $@ $<

.PRECIOUS: output/dont_err/%.s
output/dont_err/%.s: input/dont_err/%.$(SNAKE_EXT) main
	./main $< > $@

gctest.o: gctest.c gc.h
	gcc gctest.c -m64 -c -g -o gctest.o

# gc.o: gc.c gc.h
# 	gcc gc.c -m64 -c -g -o gc.o

# cutest-1.5/CuTest.o: cutest-1.5/CuTest.c cutest-1.5/CuTest.h
# 	gcc -m32 cutest-1.5/CuTest.c -c -g -o cutest-1.5/CuTest.o

# gctest: gctest.o gc.c cutest-1.5/CuTest.o cutest-1.5/CuTest.h
# 	gcc -m32 cutest-1.5/AllTests.c cutest-1.5/CuTest.o gctest.o gc.c -o gctest


clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log *.o
	rm -rf output/*/*.o output/*/*.s output/*/*.dSYM output/*/*.run
	rm -rf _build/
	rm -f main test
	rm *.run *.s
