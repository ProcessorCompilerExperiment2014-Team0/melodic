BUILD = build/
CMP   = $(BUILD)compiler
ASM   = $(BUILD)assembler
EXEC  = $(BUILD)executer
MCCFLAGS = -i -inline 5
LIB = min-caml/zekamashi/libmincaml.txt min-caml/lib/fl.ml
STDLIB = -lib min-caml/zekamashi/libmincaml.txt # -glib min-caml/lib/fl # currently, min-caml for zekamashi cannot compile library 

TESTS = test/ack.test test/fib.test test/gcd.test test/inprod.test \
  test/gcd.testlib test/extvar.testlib \
  test/cls-bug.test test/cls-bug2.test \
  test/join-reg.test test/join-reg2.test \
  test/join-stack.test test/join-stack2.test test/join-stack3.test \
  test/print_char.teststdio \
  test/sum.test test/sum-tail.test \
  test/spill.test test/spill2.test test/spill3.test \
  test/matmul.teststdio test/matmul-flat.teststdio \
  test/ary-test.teststdio \
  test/float-easy.test test/float-sqrt.test \
  test/float-atan.test test/float-sin.test test/float-cos.test test/float.test 

.PHONY: all clean
all: $(TESTS)
clean:
	rm -f $(CMP) $(ASM) $(EXEC)
	-make -C min-caml clean
	-cd Zekamashi/asm; omake clean
	-make -C Zekamashi/sim clean
test/%.test: test/%.x test/%.ml $(EXEC) converter
	$(EXEC) test/$*.x >test/$*.out 2>test/$*.err; if test $$? -ne 0 ; then cat test/$*.err; false; fi
	ocaml test/$*.ml >test/$*.out-oc
	./converter <test/$*.out >test/$*.out-mc
	diff test/$*.out-mc test/$*.out-oc
	rm test/$*.err
test/%.x: test/%.s $(ASM)
	$(ASM) test/$*.s >/dev/null
	mv test/$* test/$*.x
test/%-main.s: test/%-lib.ml test/%-main.ml $(CMP) $(LIB)
	$(CMP) $(MCCFLAGS) $(STDLIB) -glib test/$*-lib test/$*-main
test/%.s: test/%.ml $(CMP) $(LIB)
	$(CMP) $(MCCFLAGS) $(STDLIB) test/$*
min-caml/min-caml:
	$(MAKE) -C min-caml -f Makefile.zek min-caml
$(CMP): min-caml/min-caml
	cp min-caml/min-caml $(CMP)
$(ASM): Zekamashi
	cd Zekamashi/asm; omake
	cp Zekamashi/asm/asagumo $(ASM)
$(EXEC): Zekamashi
	$(MAKE) -C Zekamashi/sim
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


