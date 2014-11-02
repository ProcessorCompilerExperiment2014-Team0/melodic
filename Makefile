BUILD = build/
CMP   = $(BUILD)compiler
ASM   = $(BUILD)assembler
EXEC  = $(BUILD)executer
LIB = min-caml/zebius/libmincaml.txt

TESTS = test/ack.test test/fib.test test/gcd.test test/inprod.test test/float-easy.test test/float-sqrt.test \
  test/float-atan.test test/float-sin.test test/float-cos.test test/float.test test/gcd.testlib

.PHONY: all clean
all: $(TESTS)
clean:
	rm -f $(CMP) $(ASM) $(EXEC)
	-make -C min-caml clean
	-cd Zebius/asm; omake clean
	-make -C Zebius/sim clean
test/%.test: test/%.x test/%.cons $(EXEC)
	$(EXEC) test/$*.x test/$*.cons 2>test/$*.err; if test $$? -ne 0 ; then cat test/$*.err; false; fi
	rm test/$*.err
test/%.testlib: test/%-main.x test/%-main.cons $(EXEC)
	$(EXEC) test/$*-main.x test/$*-main.cons 2>test/$*-main.err; if test $$? -ne 0 ; then cat test/$*-main.err; false; fi
	rm test/$*-main.err
	
test/%.x: test/%.s $(ASM)
	$(ASM) test/$*.s >/dev/null
	mv test/$* test/$*.x
test/%-main.s: test/%-lib.ml test/%-main.ml $(CMP) $(LIB)
	$(CMP) -lib $(LIB) -glib test/$*-lib test/$*-main
test/%.s: test/%.ml $(CMP) $(LIB)
	$(CMP) -lib $(LIB) test/$*
$(CMP): min-caml/min-caml
	cd min-caml; ./to_zebius
	make -C min-caml min-caml
	cp min-caml/min-caml $(CMP)
$(ASM): Zebius
	cd Zebius/asm; omake
	cp Zebius/asm/zasm $(ASM)
$(EXEC): Zebius
	make -C Zebius/sim
	cp Zebius/sim/zsim $(EXEC)


raytrace: raytracer/min-rt.ml $(CMP)
	$(CMP) -glib raytracer/globals raytracer/min-rt

