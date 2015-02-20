BUILD = build/
CMP   = $(BUILD)compiler
ASM   = $(BUILD)assembler
EXEC  = $(BUILD)executer
MCCFLAGS = -i -inline 5
LIB = min-caml/zekamashi/libmincaml.txt min-caml/lib/fl.ml
STDLIB = -lib min-caml/zekamashi/libmincaml.txt -glib min-caml/lib/fl
TESTS = test/ack.test test/fib.test test/gcd.test test/inprod.test \
  test/join-reg.test test/join-reg2.test \
  test/join-stack.test test/join-stack2.test test/join-stack3.test \
  test/sum.test test/sum-tail.test \
  test/spill.test test/spill2.test test/spill3.test \
  test/float-easy.test test/float-sqrt.test \
  test/cls-bug.test test/cls-bug2.test \
  test/ary-test.test \
  test/matmul.test test/matmul-flat.test \
  test/float-atan.test test/float-sin.test test/float-cos.test test/float.test test/float-array.test \
  test/pair.test test/tuple-float.test \
  test/gcd.testlib test/extvar.testlib

.PHONY: all clean
all: $(TESTS)
clean:
	rm -f $(CMP) $(ASM) $(EXEC)
	-make -C min-caml clean
	-cd Zekamashi/asm; omake clean
	-make -C Zekamashi/sim clean
%.test: %.x %.ml $(EXEC) converter
	$(EXEC) $*.x >$*.out 2>$*.err; if test $$? -ne 0 ; then cat $*.err; false; fi
	ocaml $*.ml | tr -d '\n' >$*.out-oc
	./converter <$*.out >$*.out-mc
	diff --ignore-blank-lines --ignore-all-space $*.out-mc $*.out-oc
	rm $*.err
%.testlib: %-main.x %-main.ml %-lib.ml $(EXEC) converter
	$(EXEC) $*-main.x >$*-main.out 2>$*-main.err; if test $$? -ne 0 ; then cat $*-main.err; false; fi
	rm $*-main.err
%.x: %.s $(ASM)
	$(ASM) $*.s -o $*.x >/dev/null
%-main.s: %-lib.ml %-main.ml $(CMP) $(LIB)
	$(CMP) $(MCCFLAGS) $(STDLIB) -glib $*-lib $*-main
%.s: %.ml $(CMP) $(LIB)
	$(CMP) $(MCCFLAGS) $(STDLIB) $*
min-caml/min-caml:
	$(MAKE) -C min-caml -f Makefile.zek min-caml
$(CMP): min-caml/min-caml
	cp min-caml/min-caml $(CMP)
$(ASM): Zekamashi
	cd Zekamashi/asm; omake
	cp Zekamashi/asm/asagumo $(ASM)
Zekamashi/sim/amatsukaze:
	$(MAKE) -C Zekamashi/sim
$(EXEC): Zekamashi/sim/amatsukaze
	cp Zekamashi/sim/amatsukaze $(EXEC)


raytrace: raytracer/min-rt.ml raytracer/globals.ml $(CMP) $(ASM) $(LIB)
	$(CMP) $(MCCFLAGS) $(STDLIB) -glib raytracer/globals raytracer/min-rt
	$(ASM) raytracer/min-rt.s
	mv raytracer/min-rt raytracer/min-rt.x

flfuntest: ppmtest.ml $(CMP) $(ASM) $(EXEC) $(LIB)
	$(CMP) $(MCCFLAGS) $(STDLIB) ppmtest
	$(ASM) ppmtest.s
	$(EXEC) ppmtest >ppm.ppm

converter: converter.c
	gcc $< -o $@


