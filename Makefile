BUILD = build/
CMP   = $(BUILD)compiler
ASM   = $(BUILD)assembler
EXEC  = $(BUILD)executer

TESTS = test/fib.test

.PHONY: all clean
all: $(TESTS)
clean:
	rm -f $(CMP) $(ASM) $(EXEC)
	-make -C min-caml clean
	-cd Zebius/asm; omake clean
	-make -C Zebius/sim clean
test/%.test: test/%.x $(EXEC)
	$(EXEC) test/$*.x
test/%.x: test/%.s $(ASM)
	$(ASM) test/$*.s
	mv test/$* test/$*.x
test/%.s: test/%.ml $(CMP)
	$(CMP) test/$*
$(CMP):
	cd min-caml; ./to_zebius
	make -C min-caml min-caml
	cp min-caml/min-caml $(CMP)
$(ASM): Zebius
	cd Zebius/asm; omake
	cp Zebius/asm/zasm $(ASM)
$(EXEC): Zebius
	make -C Zebius/sim
	cp Zebius/sim/zsim $(EXEC)

